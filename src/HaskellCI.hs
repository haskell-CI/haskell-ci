{-# LANGUAGE CPP                 #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | New-style @.travis.yml@ script generator using cabal 1.24's nix-style
-- tech-preview facilities.
--
-- See also <https://github.com/haskell-CI/haskell-ci>
--
-- NB: This code deliberately avoids relying on non-standard packages and
--     is expected to compile/work with at least GHC 7.0 through GHC 8.0
module HaskellCI (
    main,
    -- * for tests
    parseTravis,
    travisFromConfigFile, Options (..), defaultOptions,
    ) where

import HaskellCI.Prelude

import Data.List                 (intercalate, isPrefixOf, nub, nubBy, sort, sortBy, (\\))
import Distribution.Compat.ReadP (readP_to_S)
import System.Directory          (canonicalizePath, doesDirectoryExist, doesFileExist, makeRelativeToCurrentDirectory, setCurrentDirectory)
import System.Environment        (getArgs)
import System.Exit               (ExitCode (..), exitFailure)
import System.FilePath.Posix     (takeDirectory, takeExtension, takeFileName, (</>))
import System.IO                 (hClose, hFlush, hPutStr, hPutStrLn, stderr)
import System.IO.Temp            (withSystemTempFile)
import System.Process            (readProcessWithExitCode)

import Distribution.PackageDescription        (package, packageDescription, testedWith)
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)
import Distribution.Text
import Distribution.Version

import qualified Data.ByteString       as BS
import qualified Data.List.NonEmpty    as NE
import qualified Data.Set              as S
import qualified Data.Traversable      as T
import qualified Distribution.Compiler as Compiler
import qualified Distribution.Package  as Pkg
import qualified Options.Applicative   as O

import HaskellCI.Compiler
import HaskellCI.Cli
import HaskellCI.Config
import HaskellCI.Config.Dump
import HaskellCI.Config.Ubuntu
import HaskellCI.Diagnostics
import HaskellCI.Glob
import HaskellCI.Jobs
import HaskellCI.Package
import HaskellCI.Project
import HaskellCI.TestedWith
import HaskellCI.Travis
import HaskellCI.YamlSyntax

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
    argv0 <- getArgs
    (cmd, opts) <- O.execParser cliParserInfo
    case cmd of
        CommandListGHC -> do
            putStrLn $ "Supported GHC versions:"
            for_ groupedVersions $ \(v, vs) -> do
                putStr $ prettyMajVersion v ++ ": "
                putStrLn $ intercalate ", " (map display $ toList vs)
        CommandDumpConfig -> do
            putStr $ unlines $ runDG configGrammar

        CommandRegenerate -> do
            let fp = case optOutput opts of
                    Just (OutputFile fp') -> fp'
                    _                     -> defaultTravisPath

            -- read, and then change to the directory
            contents <- readFile fp
            absFp <- canonicalizePath fp
            let dir = takeDirectory fp
            setCurrentDirectory dir
            newFp <- makeRelativeToCurrentDirectory absFp

            case findArgv (lines contents) of
                Nothing     -> do
                    hPutStrLn stderr $ "Error: expected REGENDATA line in " ++ fp
                    exitFailure
                Just argv   -> do
                    (f, opts') <- parseTravis argv
                    doTravis argv f (optionsWithOutputFile newFp <> opts' <> opts)
        CommandTravis f -> doTravis argv0 f opts
  where
    findArgv :: [String] -> Maybe [String]
    findArgv ls = do
        l <- findMaybe (afterInfix "REGENDATA") ls
        readMaybe l

    groupedVersions :: [(Version, NonEmpty Version)]
    groupedVersions = map ((\vs -> (head vs, vs)) . NE.sortBy (flip compare))
                    . groupBy ((==) `on` ghcMajVer)
                    $ sort knownGhcVersions

    prettyMajVersion :: Version -> String
    prettyMajVersion v = case ghcMajVer v of
        (x, y) -> show x ++ "." ++ show y

defaultTravisPath :: FilePath
defaultTravisPath = ".travis.yml"

doTravis :: [String] -> FilePath -> Options -> IO ()
doTravis args path opts = do
    ls <- travisFromConfigFile args opts path
    let contents = unlines ls
    case optOutput opts of
        Nothing              -> writeFile defaultTravisPath contents
        Just OutputStdout    -> putStr contents
        Just (OutputFile fp) -> writeFile fp contents

travisFromConfigFile
    :: forall m. (MonadIO m, MonadDiagnostics m, MonadMask m)
    => [String]
    -> Options
    -> FilePath
    -> m [String]
travisFromConfigFile args opts path = do
    cabalFiles <- getCabalFiles
    config' <- maybe (return emptyConfig) readConfigFile (optConfig opts)
    let config = optConfigMorphism opts config'
    pkgs <- T.mapM (configFromCabalFile config) cabalFiles
    (ghcs, prj) <- case checkVersions (cfgTestedWith config) pkgs of
        Right x     -> return x
        Left []     -> putStrLnErr "panic: checkVersions failed without errors"
        Left (e:es) -> putStrLnErrs (e :| es)

    when (any isGHCJS ghcs && cfgUbuntu config /= Bionic) $
        putStrLnErr "GHCJS jobs require ubuntu: bionic"

    let prj' | cfgGhcHead config = over (mapped . #pkgJobs) (S.insert GHCHead) prj
             | otherwise         = prj

    ls <- genTravisFromConfigs args config prj' ghcs
    patchTravis config ls
  where
    isCabalProject :: Maybe FilePath
    isCabalProject
        | "cabal.project" `isPrefixOf` takeFileName path = Just path
        | otherwise = Nothing

    getCabalFiles :: m (Project Void FilePath)
    getCabalFiles
        | isNothing isCabalProject = return $ emptyProject & #prjPackages .~ [path]
        | otherwise = do
            contents <- liftIO $ BS.readFile path
            prj  <- either putStrLnErr return $ parseProjectFile path contents
            prj' <- bitraverse findOptProjectPackage findProjectPackage prj
            return prj'
                { prjPackages    = concat $ prjPackages prj' ++ prjOptPackages prj'
                , prjOptPackages = []
                }

    rootdir = takeDirectory path

    -- See findProjectPackages in cabal-install codebase
    -- this is simple variant.
    findProjectPackage :: String -> m [FilePath]
    findProjectPackage pkglocstr = do
        mfp <- checkisFileGlobPackage pkglocstr `mplusMaybeT`
               checkIsSingleFilePackage pkglocstr
        maybe (putStrLnErr $ "bad package location: " ++ pkglocstr) return mfp

    findOptProjectPackage :: String -> m [FilePath]
    findOptProjectPackage pkglocstr = do
        mfp <- checkisFileGlobPackage pkglocstr `mplusMaybeT`
               checkIsSingleFilePackage pkglocstr
        maybe (return []) return mfp

    checkIsSingleFilePackage pkglocstr = do
        let abspath = rootdir </> pkglocstr
        isFile <- liftIO $ doesFileExist abspath
        isDir  <- liftIO $ doesDirectoryExist abspath
        if | isFile && takeExtension pkglocstr == ".cabal" -> return (Just [abspath])
           | isDir -> checkisFileGlobPackage (pkglocstr </> "*.cabal")
           | otherwise -> return Nothing

    -- if it looks like glob, glob
    checkisFileGlobPackage pkglocstr =
        case filter (null . snd) $ readP_to_S parseFilePathGlobRel pkglocstr of
            [(g, "")] -> do
                files <- liftIO $ expandRelGlob rootdir g
                let files' = filter ((== ".cabal") . takeExtension) files
                -- if nothing is matched, skip.
                if null files' then return Nothing else return (Just files')
            _         -> return Nothing

    mplusMaybeT :: Monad m => m (Maybe a) -> m (Maybe a) -> m (Maybe a)
    mplusMaybeT ma mb = do
        mx <- ma
        case mx of
            Nothing -> mb
            Just x  -> return (Just x)

genTravisFromConfigs
    :: (Monad m, MonadDiagnostics m)
    => [String]
    -> Config
    -> Project Void Package
    -> Set CompilerVersion
    -> m [String]
genTravisFromConfigs argv config prj vs = do
    let jobVersions = makeJobVersions config vs
    case makeTravis argv config prj jobVersions of
        Left err     -> putStrLnErr (show err) -- TODO
        Right travis -> do
            describeJobs (cfgTestedWith config) jobVersions (prjPackages prj)
            return $
                lines (prettyYaml id $ reann (travisHeader (cfgInsertVersion config) argv ++) $ toYaml travis)
                ++
                [ ""
                , "# REGENDATA " ++ show argv
                , "# EOF"
                ]

-- | Adjust the generated Travis YAML output with patch files, if specified.
-- We do this in a temporary file in case the user did not pass --output (as
-- it would be awkward to patch the generated output otherwise).
patchTravis
    :: (MonadIO m, MonadMask m)
    => Config -> [String] -> m [String]
patchTravis cfg ls
  | null patches = pure ls
  | otherwise =
      withSystemTempFile ".travis.yml.tmp" $ \fp h -> liftIO $ do
        hPutStr h $ unlines ls
        hFlush h
        for_ patches $ applyPatch fp
        hClose h
        lines <$> readFile fp
  where
    patches :: [FilePath]
    patches = cfgTravisPatches cfg

    applyPatch :: FilePath -- ^ The temporary file path to patch
               -> FilePath -- ^ The path of the .patch file
               -> IO ()
    applyPatch temp patch = do
        exists <- doesFileExist patch
        unless exists $ putStrLnErr $ "Cannot find " ++ patch
        (ec, stdOut, stdErr) <- readProcessWithExitCode
            "patch" [ "--input", patch
            , "--silent"
            , temp
            ] ""
        case ec of
            ExitSuccess -> pure ()
            ExitFailure n -> putStrLnErr $ unlines
                [ "patch returned exit code " ++ show n
                , "Stdout: " ++ stdOut
                , "Stderr: " ++ stdErr
                ]

configFromCabalFile
    :: (MonadIO m, MonadDiagnostics m)
    => Config -> FilePath -> m Package
configFromCabalFile cfg cabalFile = do
    gpd <- liftIO $ readGenericPackageDescription maxBound cabalFile

    let compilers = testedWith $ packageDescription gpd
        pkgNameStr = display $ Pkg.pkgName $ package $ packageDescription gpd

    let unknownComps = nub [ c | (c,_) <- compilers, c /= Compiler.GHC, c /= Compiler.GHCJS ]

        ghcVerConstrs   = [ vc | (Compiler.GHC,vc) <- compilers ]
        ghcVerConstrs'  = simplifyVersionRange $ foldr unionVersionRanges noVersion ghcVerConstrs
        specificGhcVers = nub $ mapMaybe isSpecificVersion ghcVerConstrs

        ghcjsVerConstrs   = [ vc | (Compiler.GHCJS,vc) <- compilers ]
        ghcjsVerConstrs'  = simplifyVersionRange $ foldr unionVersionRanges noVersion ghcjsVerConstrs
        specificGhcjsVers = nub $ mapMaybe isSpecificVersion ghcjsVerConstrs

        twoDigitGhcVerConstrs = mapMaybe isTwoDigitGhcVersion ghcVerConstrs :: [Version]

    unless (null twoDigitGhcVerConstrs) $ do
        putStrLnWarn $ "'tested-with:' uses two digit GHC versions (which don't match any existing GHC version): " ++ intercalate ", " (map display twoDigitGhcVerConstrs)
        putStrLnInfo $ "Either use wild-card format, for example 'tested-with: GHC ==7.10.*' or a specific existing version 'tested-with: GHC ==7.10.3'"

    when (null compilers) $ do
        putStrLnErr (unlines $
                     [ "empty or missing top-level 'tested-with:' definition in " ++ cabalFile ++ " file; example definition:"
                     , ""
                     , "tested-with: " ++ intercalate ", " [ "GHC==" ++ display v | v <- lastStableGhcVers ]
                     ])

    unless (null unknownComps) $ do
        putStrLnWarn $ "ignoring unsupported compilers mentioned in tested-with: " ++ show unknownComps

    when (null ghcVerConstrs) $ do
        putStrLnErr "'tested-with:' doesn't mention any 'GHC' version"

    when (isNoVersion ghcVerConstrs') $ do
        putStrLnErr "'tested-with:' describes an empty version range for 'GHC'"

    when (isAnyVersion ghcVerConstrs') $ do
        putStrLnErr "'tested-with:' allows /any/ 'GHC' version"

    let unknownGhcVers = sort $ specificGhcVers \\ knownGhcVersions
    unless (null unknownGhcVers) $ do
        putStrLnErr ("'tested-with:' specifically refers to unknown 'GHC' versions: "
                     ++ intercalate ", " (map display unknownGhcVers) ++ "\n"
                     ++ "Known GHC versions: " ++ intercalate ", " (map display knownGhcVersions))

    let unknownGhcjsVers = sort $ specificGhcjsVers \\ knownGhcjsVersions
    unless (null unknownGhcjsVers) $ do
        putStrLnErr ("'tested-with:' specifically refers to unknown 'GHCJS' versions: "
                     ++ intercalate ", " (map display unknownGhcjsVers) ++ "\n"
                     ++ "Known GHCJS versions: " ++ intercalate ", " (map display knownGhcjsVersions))

    let knownGhcVersions'
            | cfgLastInSeries cfg = filterLastMajor knownGhcVersions
            | otherwise           = knownGhcVersions

    let testedGhcVersions   = filter (`withinRange` ghcVerConstrs') knownGhcVersions'
    let testedGhcjsVersions = filter (`withinRange` ghcjsVerConstrs') knownGhcjsVersions

    when (null testedGhcVersions) $ do
        putStrLnErr "no known GHC version is allowed by the 'tested-with' specification"

    let compilerRange :: Set CompilerVersion
        compilerRange = S.fromList $
            [ GHC v
            | v <- testedGhcVersions
            ] ++
            [ GHCJS v
            | v <- testedGhcjsVersions
            ]

    let pkg = Pkg pkgNameStr compilerRange (takeDirectory cabalFile) gpd

    return pkg
  where
    lastStableGhcVers
        = nubBy ((==) `on` ghcMajVer)
        $ sortBy (flip compare)
        $ filter (not . previewGHC defaultHeadHackage . GHC)
        $ knownGhcVersions

    isTwoDigitGhcVersion :: VersionRange -> Maybe Version
    isTwoDigitGhcVersion vr = isSpecificVersion vr >>= t
      where
        t v | [_,_] <- versionNumbers v = Just v
        t _                             = Nothing

    filterLastMajor = map maximum . groupBy ((==) `on` ghcMajVer)
