{-# LANGUAGE CPP                 #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NamedFieldPuns      #-}
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

import           Prelude                                ()
import           Prelude.Compat

import           Control.Monad                          (forM_, liftM, unless,
                                                         when)
import           Control.Monad.IO.Class                 (MonadIO (..))
import           Data.Function                          (on)
import           Data.Generics.Labels                   ()
import           Data.List                              (groupBy, intercalate,
                                                         isPrefixOf, nub, nubBy,
                                                         sort, sortBy, (\\))
import           Data.List.NonEmpty                     (NonEmpty (..))
import           Data.Maybe                             (isNothing, mapMaybe)
import           Data.Semigroup                         (Semigroup (..))
import           Data.Set                               (Set)
import           Distribution.Compat.ReadP              (readP_to_S)
import           Lens.Micro
import           System.Directory                       (canonicalizePath,
                                                         doesDirectoryExist,
                                                         doesFileExist,
                                                         makeRelativeToCurrentDirectory,
                                                         setCurrentDirectory)
import           System.Environment                     (getArgs)
import           System.Exit                            (exitFailure)
import           System.IO                              (hPutStrLn, stderr)
import           System.Path                            (Absolute, FsPath, Path,
                                                         fromFilePath,
                                                         fromUnrootedFilePath,
                                                         makeAbsolute,
                                                         takeDirectory,
                                                         takeExtension,
                                                         takeFileName, FileExt (..),
                                                         toFilePath,
                                                         toUnrootedFilePath,
                                                         (</>))
import           System.Path.Extras
import           Text.Read                              (readMaybe)

import           Distribution.Compiler                  (CompilerFlavor (..))
import           Distribution.PackageDescription        (package,
                                                         packageDescription,
                                                         testedWith)
import           Distribution.PackageDescription.Parsec (readGenericPackageDescription)
import           Distribution.Text
import           Distribution.Version

import qualified Data.ByteString                        as BS
import qualified Data.Set                               as S
import qualified Data.Traversable                       as T
import qualified System.FilePath as Raw
import qualified Distribution.Package                   as Pkg
import qualified Options.Applicative                    as O

import           HaskellCI.Cli
import           HaskellCI.Config
import           HaskellCI.Config.Dump
import           HaskellCI.Diagnostics
import           HaskellCI.Extras
import           HaskellCI.GHC
import           HaskellCI.Glob
import           HaskellCI.Jobs
import           HaskellCI.Package
import           HaskellCI.Project
import           HaskellCI.TestedWith
import           HaskellCI.Travis
import           HaskellCI.YamlSyntax

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
            forM_ groupedVersions $ \(v, vs) -> do
                putStr $ prettyMajVersion v ++ ": "
                putStrLn $ intercalate ", " (map display vs)
        CommandDumpConfig -> do
            putStr $ unlines $ runDG configGrammar

        CommandRegenerate -> do
            let fp = case optOutput opts of
                    Just (OutputFile fp') -> fp'
                    _                     -> defaultTravisPath

            -- read, and then change to the directory
            absFp <- makeAbsolute fp
            contents <- readFile (toFilePath absFp)
            let dir = takeDirectory absFp

            case findArgv (lines contents) of
                Nothing     -> do
                    hPutStrLn stderr $ "Error: expected REGENDATA line in " ++ toFilePath absFp
                    exitFailure
                Just argv   -> do
                    (cfp, opts') <- parseTravis argv
                    absCfp <- makeAbsolute cfp
                    doTravis argv absCfp (optionsWithOutputFile absFp <> opts' <> opts)
        CommandTravis f -> do
            f' <- makeAbsolute f
            doTravis argv0 f' opts
  where
    findArgv :: [String] -> Maybe [String]
    findArgv ls = do
        l <- findMaybe (afterInfix "REGENDATA") ls
        readMaybe l

    groupedVersions :: [(Version, [Version])]
    groupedVersions = map ((\vs -> (head vs, vs)) . sortBy (flip compare))
                    . groupBy ((==) `on` ghcMajVer)
                    $ sort knownGhcVersions

    prettyMajVersion :: Version -> String
    prettyMajVersion v
        | Just v == ghcAlpha = "alpha"
        | otherwise = case ghcMajVer v of (x,y) -> show x ++ "." ++ show y

defaultTravisPath :: FsPath
defaultTravisPath = fromFilePath ".travis.yml"

doTravis :: [String] -> Path Absolute -> Options -> IO ()
doTravis args path opts = do
    ls <- travisFromConfigFile args opts path
    let contents = unlines ls
    case optOutput opts of
        Nothing              -> do
            fp <- makeAbsolute defaultTravisPath
            writeFile (toFilePath fp) contents
        Just OutputStdout    ->
            putStr contents
        Just (OutputFile fp) -> do
            fp' <- makeAbsolute fp
            writeFile (toFilePath fp') contents

travisFromConfigFile
    :: forall m. (MonadIO m, MonadDiagnostics m)
    => [String]
    -> Options
    -> Path Absolute
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
    genTravisFromConfigs args config prj ghcs
  where
    isCabalProject :: Maybe (Path Absolute)
    isCabalProject
        | "cabal.project" `isPrefixOf` p = Just path
        | otherwise                      = Nothing
      where
        p = toUnrootedFilePath $ takeFileName path

    getCabalFiles :: m (Project (Path Absolute))
    getCabalFiles
        | isNothing isCabalProject = return $ emptyProject & #prjPackages .~ [path]
        | otherwise = do
            contents <- liftIO $ BS.readFile $ toFilePath path
            pkgs <- either putStrLnErr return $ parseProjectFile path contents
            over #prjPackages concat `liftM` T.mapM findProjectPackage pkgs

    rootdir = takeDirectory path

    -- See findProjectPackages in cabal-install codebase
    -- this is simple variant.
    findProjectPackage :: String -> m [Path Absolute]
    findProjectPackage pkglocstr = do
        mfp <- checkIsFileGlobPackage pkglocstr `mplusMaybeT`
               checkIsSingleFilePackage pkglocstr
        maybe (putStrLnErr $ "bad package location: " ++ pkglocstr) return mfp

    checkIsSingleFilePackage :: String -> m (Maybe [Path Absolute])
    checkIsSingleFilePackage pkglocstr = do
        let abspath = rootdir </> fromUnrootedFilePath pkglocstr
        isFile <- liftIO $ doesFileExist $ toFilePath abspath
        isDir  <- liftIO $ doesDirectoryExist $ toFilePath abspath
        if | isFile && Raw.takeExtension pkglocstr == ".cabal" -> return (Just [abspath])
           | isDir -> checkIsFileGlobPackage (pkglocstr ++ "/" ++ "*.cabal")
           | otherwise -> return Nothing

    -- if it looks like glob, glob
    checkIsFileGlobPackage :: String -> m (Maybe [Path Absolute])
    checkIsFileGlobPackage pkglocstr =
        case filter (null . snd) $ readP_to_S parseFilePathGlobRel pkglocstr of
            [(g, "")] -> do
                files <- liftIO $ expandRelGlob rootdir g
                let files' = filter ((== Just (FileExt ".cabal")) . takeExtension) files
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
    -> Project Package
    -> Set Version
    -> m [String]
genTravisFromConfigs argv config prj versions' = do
    let jobVersions = makeJobVersions config versions'
    case makeTravis argv config prj jobVersions of
        Left err     -> putStrLnErr (show err) -- TODO
        Right travis -> do
            describeJobs jobVersions
            return $
                lines (prettyYaml id $ reann (travisHeader argv ++) $ toYaml travis)
                ++
                [ ""
                , "# REGENDATA " ++ show argv
                , "# EOF"
                ]

configFromCabalFile
    :: (MonadIO m, MonadDiagnostics m)
    => Config -> Path Absolute -> m (Package, Set Version)
configFromCabalFile cfg cabalFile = do
    gpd <- liftIO $ readGenericPackageDescription maxBound $ toFilePath cabalFile

    let compilers = testedWith $ packageDescription gpd
        pkgNameStr = display $ Pkg.pkgName $ package $ packageDescription gpd

    let unknownComps = nub [ c | (c,_) <- compilers, c /= GHC ]
        ghcVerConstrs = [ vc | (GHC,vc) <- compilers ]
        ghcVerConstrs' = simplifyVersionRange $ foldr unionVersionRanges noVersion ghcVerConstrs
        twoDigitGhcVerConstrs = mapMaybe isTwoDigitGhcVersion ghcVerConstrs :: [Version]
        specificGhcVers = nub $ mapMaybe isSpecificVersion ghcVerConstrs

    unless (null twoDigitGhcVerConstrs) $ do
        putStrLnWarn $ "'tested-with:' uses two digit GHC versions (which don't match any existing GHC version): " ++ intercalate ", " (map display twoDigitGhcVerConstrs)
        putStrLnInfo $ "Either use wild-card format, for example 'tested-with: GHC ==7.10.*' or a specific existing version 'tested-with: GHC ==7.10.3'"

    when (null compilers) $ do
        putStrLnErr (unlines $
                     [ "empty or missing top-level 'tested-with:' definition in " ++ toFilePath cabalFile ++ " file; example definition:"
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

    let knownGhcVersions'
            | cfgLastInSeries cfg = filterLastMajor knownGhcVersions
            | otherwise           = knownGhcVersions

    let testedGhcVersions = filter (`withinRange` ghcVerConstrs') knownGhcVersions'

    when (null testedGhcVersions) $ do
        putStrLnErr "no known GHC version is allowed by the 'tested-with' specification"

    let pkg = Pkg pkgNameStr anyVersion (takeDirectory cabalFile) gpd

    return (pkg, S.fromList testedGhcVersions)
  where
    lastStableGhcVers
        = nubBy ((==) `on` ghcMajVer)
        $ sortBy (flip compare)
        $ filter (not . previewGHC . Just)
        $ knownGhcVersions

    isTwoDigitGhcVersion :: VersionRange -> Maybe Version
    isTwoDigitGhcVersion vr = isSpecificVersion vr >>= t
      where
        t v | [_,_] <- versionNumbers v = Just v
        t _ = Nothing

    filterLastMajor = map maximum . groupBy ((==) `on` ghcMajVer)
