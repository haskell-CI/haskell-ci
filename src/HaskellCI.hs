{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

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
    parseOptions,
    Options (..), defaultOptions,
    Config (..), GitConfig (..),
    InputType (..),
    runDiagnosticsT,
    -- ** Variants
    bashFromConfigFile,
    travisFromConfigFile,
    githubFromConfigFile,
    sourcehutFromConfigFile,
    ) where

import HaskellCI.Prelude

import Control.Exception     (try)
import Data.List             (nubBy, sort, sortBy, (\\), isSuffixOf)
import qualified Data.Map.Strict as M
import System.Directory      (createDirectoryIfMissing, doesFileExist, setCurrentDirectory, getDirectoryContents)
import System.Environment    (getArgs)
import System.Exit           (ExitCode (..), exitFailure)
import System.FilePath       ((</>))
import System.FilePath.Posix (takeDirectory)
import System.IO             (hClose, hPutStrLn, stderr)
import System.IO.Error       (catchIOError, isDoesNotExistError)
import System.IO.Temp        (withSystemTempFile)
import System.Process        (readProcessWithExitCode)

import Distribution.PackageDescription (GenericPackageDescription, package, packageDescription, testedWith)
import Distribution.Simple.Utils       (fromUTF8BS, toUTF8BS)
import Distribution.Text
import Distribution.Version

import qualified Data.ByteString       as BS
import qualified Data.List.NonEmpty    as NE
import qualified Data.Map              as Map
import qualified Data.Set              as S
import qualified Data.Traversable      as T
import qualified Distribution.Compiler as Compiler
import qualified Distribution.Package  as Pkg
import qualified Options.Applicative   as O

import Cabal.Parse
import Cabal.Project
import HaskellCI.Bash
import HaskellCI.Cli
import HaskellCI.Compiler
import HaskellCI.Config
import HaskellCI.Config.Dump
import HaskellCI.Diagnostics
import HaskellCI.GitConfig
import HaskellCI.GitHub
import HaskellCI.HeadHackage
import HaskellCI.Jobs
import HaskellCI.Package
import HaskellCI.Sourcehut
import HaskellCI.Sourcehut.Yaml (sourcehutManifests)
import HaskellCI.TestedWith
import HaskellCI.Travis
import HaskellCI.VersionInfo
import HaskellCI.YamlSyntax

import qualified HaskellCI.Bash.Template as Bash

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
    argv0 <- getArgs
    (cmd, opts) <- O.customExecParser (O.prefs O.subparserInline) cliParserInfo
    case cmd of
        CommandListGHC -> do
            putStrLn $ "Supported GHC versions:"
            for_ groupedVersions $ \(v, vs) -> do
                putStr $ prettyMajVersion v ++ ": "
                putStrLn $ intercalate ", " (map display $ toList vs)

        CommandDumpConfig -> do
            putStr $ unlines $ runDG configGrammar

        CommandRegenerate -> do
            regenerateBash opts
            regenerateGitHub opts
            regenerateTravis opts
            regenerateSourcehut opts

        CommandBash      f -> doBash argv0 f opts
        CommandGitHub    f -> doGitHub argv0 f opts
        CommandTravis    f -> doTravis argv0 f opts
        CommandSourcehut f -> doSourcehut argv0 f opts

        CommandVersionInfo -> do
            putStrLn $ "haskell-ci " ++ haskellCIVerStr ++ " with dependencies"
            ifor_ dependencies $ \p v -> do
                putStrLn $ "  " ++ p ++ "-" ++ v
  where
    groupedVersions :: [(Version, NonEmpty Version)]
    groupedVersions = map ((\vs -> (head vs, vs)) . NE.sortBy (flip compare))
                    . groupBy ((==) `on` ghcMajVer)
                    $ sort knownGhcVersions

    prettyMajVersion :: Version -> String
    prettyMajVersion v = case ghcMajVer v of
        (x, y) -> show x ++ "." ++ show y

    ifor_ :: Map.Map k v -> (k -> v -> IO a) -> IO ()
    ifor_ xs f = Map.foldlWithKey' (\m k a -> m >> void (f k a)) (return ()) xs

-------------------------------------------------------------------------------
-- Travis
-------------------------------------------------------------------------------

defaultTravisPath :: FilePath
defaultTravisPath = ".travis.yml"

doTravis :: [String] -> FilePath -> Options -> IO ()
doTravis args path opts = do
    contents <- travisFromConfigFile args opts path
    case optOutput opts of
        Nothing              -> BS.writeFile defaultTravisPath contents
        Just OutputStdout    -> BS.putStr contents
        Just (OutputFile fp) -> BS.writeFile fp contents

travisFromConfigFile
    :: forall m. (MonadIO m, MonadDiagnostics m, MonadMask m)
    => [String]
    -> Options
    -> FilePath
    -> m ByteString
travisFromConfigFile args opts path = do
    gitconfig <- liftIO readGitConfig
    cabalFiles <- getCabalFiles (optInputType' opts path) path
    config' <- findConfigFile (optConfig opts)
    let config = optConfigMorphism opts config'
    pkgs <- T.mapM (configFromCabalFile config) cabalFiles
    (ghcs, prj) <- case checkVersions (cfgTestedWith config) pkgs of
        Right x     -> return x
        Left []     -> putStrLnErr "panic: checkVersions failed without errors"
        Left (e:es) -> putStrLnErrs (e :| es)

    let prj' | cfgGhcHead config = over (mapped . field @"pkgJobs") (S.insert GHCHead) prj
             | otherwise         = prj

    ls <- genTravisFromConfigs args config gitconfig prj' ghcs
    patchTravis config ls

genTravisFromConfigs
    :: (Monad m, MonadDiagnostics m)
    => [String]
    -> Config
    -> GitConfig
    -> Project URI Void Package
    -> Set CompilerVersion
    -> m ByteString
genTravisFromConfigs argv config _gitconfig prj vs = do
    let jobVersions = makeJobVersions config vs
    case makeTravis argv config prj jobVersions of
        Left err     -> putStrLnErr $ displayException err
        Right travis -> do
            describeJobs "Travis-CI config" (cfgTestedWith config) jobVersions (prjPackages prj)
            return $ toUTF8BS $
                prettyYaml id (reann (travisHeader (cfgInsertVersion config) argv ++) $ toYaml travis)
                ++ unlines
                [ ""
                , "# REGENDATA " ++ if cfgInsertVersion config then show (haskellCIVerStr, argv) else show argv
                , "# EOF"
                ]

regenerateTravis :: Options -> IO ()
regenerateTravis opts = do
    let fp = defaultTravisPath

    -- change the directory
    for_ (optCwd opts) setCurrentDirectory

    -- read, and then change to the directory
    withContents fp noTravisYml $ \contents -> case findRegendataArgv contents of
        Nothing     -> do
            hPutStrLn stderr $ "Error: expected REGENDATA line in " ++ fp
            exitFailure

        Just (mversion, argv) -> do
            -- warn if we regenerate using older haskell-ci
            for_ mversion $ \version -> for_ (simpleParsec haskellCIVerStr) $ \haskellCIVer ->
                when (haskellCIVer < version) $ do
                    hPutStrLn stderr $ "Regenerating using older haskell-ci-" ++ haskellCIVerStr
                    hPutStrLn stderr $ "File generated using haskell-ci-" ++ prettyShow version

            (f, opts') <- parseOptions argv
            doTravis argv f ( optionsWithOutputFile fp <> opts' <> opts)
  where
    noTravisYml :: IO ()
    noTravisYml = putStrLn "No .travis.yml, skipping travis regeneration"

-------------------------------------------------------------------------------
-- Bash
-------------------------------------------------------------------------------

defaultBashPath :: FilePath
defaultBashPath = "haskell-ci.sh"

doBash :: [String] -> FilePath -> Options -> IO ()
doBash args path opts = do
    contents <- bashFromConfigFile args opts path
    case optOutput opts of
        Nothing              -> BS.writeFile defaultBashPath contents
        Just OutputStdout    -> BS.putStr contents
        Just (OutputFile fp) -> BS.writeFile fp contents

bashFromConfigFile
    :: forall m. (MonadIO m, MonadDiagnostics m, MonadMask m)
    => [String]
    -> Options
    -> FilePath
    -> m ByteString
bashFromConfigFile args opts path = do
    gitconfig <- liftIO readGitConfig
    cabalFiles <- getCabalFiles (optInputType' opts path) path
    config' <- findConfigFile (optConfig opts)
    let config = optConfigMorphism opts config'
    pkgs <- T.mapM (configFromCabalFile config) cabalFiles
    (ghcs, prj) <- case checkVersions (cfgTestedWith config) pkgs of
        Right x     -> return x
        Left []     -> putStrLnErr "panic: checkVersions failed without errors"
        Left (e:es) -> putStrLnErrs (e :| es)

    let prj' | cfgGhcHead config = over (mapped . field @"pkgJobs") (S.insert GHCHead) prj
             | otherwise         = prj

    genBashFromConfigs args config gitconfig prj' ghcs

genBashFromConfigs
    :: (Monad m, MonadIO m, MonadDiagnostics m)
    => [String]
    -> Config
    -> GitConfig
    -> Project URI Void Package
    -> Set CompilerVersion
    -> m ByteString
genBashFromConfigs argv config _gitconfig prj vs = do
    let jobVersions = makeJobVersions config vs
    case makeBash argv config prj jobVersions of
        Left err    -> putStrLnErr $ displayException err
        Right bashZ -> do
            describeJobs "Bash script" (cfgTestedWith config) jobVersions (prjPackages prj)
            fmap toUTF8BS $ liftIO $ Bash.renderIO bashZ
                { Bash.zRegendata = if cfgInsertVersion config then show (haskellCIVerStr, argv) else show argv
                }

regenerateBash :: Options -> IO ()
regenerateBash opts = do
    let fp = defaultBashPath

    -- change the directory
    for_ (optCwd opts) setCurrentDirectory

    -- read, and then change to the directory
    withContents fp noBashScript $ \contents -> case findRegendataArgv contents of
        Nothing     -> do
            hPutStrLn stderr $ "Error: expected REGENDATA line in " ++ fp
            exitFailure

        Just (mversion, argv) -> do
            -- warn if we regenerate using older haskell-ci
            for_ mversion $ \version -> for_ (simpleParsec haskellCIVerStr) $ \haskellCIVer ->
                when (haskellCIVer < version) $ do
                    hPutStrLn stderr $ "Regenerating using older haskell-ci-" ++ haskellCIVerStr
                    hPutStrLn stderr $ "File generated using haskell-ci-" ++ prettyShow version

            (f, opts') <- parseOptions argv
            doBash argv f ( optionsWithOutputFile fp <> opts' <> opts)
  where
    noBashScript :: IO ()
    noBashScript = putStrLn "No haskell-ci.sh, skipping bash regeneration"

-------------------------------------------------------------------------------
-- GitHub actions
-------------------------------------------------------------------------------

defaultGitHubPath :: FilePath
defaultGitHubPath = ".github/workflows/haskell-ci.yml"

doGitHub :: [String] -> FilePath -> Options -> IO ()
doGitHub args path opts = do
    contents <- githubFromConfigFile args opts path
    case optOutput opts of
        Nothing              -> do
            createDir defaultGitHubPath
            BS.writeFile defaultGitHubPath contents
        Just OutputStdout    -> BS.putStr contents
        Just (OutputFile fp) -> do
            createDir fp
            BS.writeFile fp contents
  where
    createDir p = createDirectoryIfMissing True (takeDirectory p)

githubFromConfigFile
    :: forall m. (MonadIO m, MonadDiagnostics m, MonadMask m)
    => [String]
    -> Options
    -> FilePath
    -> m ByteString
githubFromConfigFile args opts path = do
    gitconfig <- liftIO readGitConfig
    cabalFiles <- getCabalFiles (optInputType' opts path) path
    config' <- findConfigFile (optConfig opts)
    let config = optConfigMorphism opts config'
    pkgs <- T.mapM (configFromCabalFile config) cabalFiles
    (ghcs, prj) <- case checkVersions (cfgTestedWith config) pkgs of
        Right x     -> return x
        Left []     -> putStrLnErr "panic: checkVersions failed without errors"
        Left (e:es) -> putStrLnErrs (e :| es)

    let prj' | cfgGhcHead config = over (mapped . field @"pkgJobs") (S.insert GHCHead) prj
             | otherwise         = prj

    ls <- genGitHubFromConfigs args config gitconfig prj' ghcs
    patchGitHub config ls

genGitHubFromConfigs
    :: (Monad m, MonadIO m, MonadDiagnostics m)
    => [String]
    -> Config
    -> GitConfig
    -> Project URI Void Package
    -> Set CompilerVersion
    -> m ByteString
genGitHubFromConfigs argv config gitconfig prj vs = do
    let jobVersions = makeJobVersions config vs
    case makeGitHub argv config gitconfig prj jobVersions of
        Left err     -> putStrLnErr $ displayException err
        Right github -> do
            describeJobs "GitHub config" (cfgTestedWith config) jobVersions (prjPackages prj)
            return $ toUTF8BS $ prettyYaml id $ reann (githubHeader (cfgInsertVersion config) argv ++) $ toYaml github

regenerateGitHub :: Options -> IO ()
regenerateGitHub opts = do
    -- change the directory
    for_ (optCwd opts) setCurrentDirectory

    -- read, and then change to the directory
    withContents fp noGitHubScript $ \contents -> case findRegendataArgv contents of
        Nothing     -> do
            hPutStrLn stderr $ "Error: expected REGENDATA line in " ++ fp
            exitFailure

        Just (mversion, argv) -> do
            -- warn if we regenerate using older haskell-ci
            for_ mversion $ \version -> for_ (simpleParsec haskellCIVerStr) $ \haskellCIVer ->
                when (haskellCIVer < version) $ do
                    hPutStrLn stderr $ "Regenerating using older haskell-ci-" ++ haskellCIVerStr
                    hPutStrLn stderr $ "File generated using haskell-ci-" ++ prettyShow version

            (f, opts') <- parseOptions argv
            doGitHub argv f ( optionsWithOutputFile fp <> opts' <> opts)
  where
    fp = defaultGitHubPath

    noGitHubScript :: IO ()
    noGitHubScript = putStrLn $ "No " ++ fp ++ ", skipping GitHub config regeneration"

-------------------------------------------------------------------------------
-- Sourcehut
-------------------------------------------------------------------------------

defaultSourcehutPath :: FilePath
defaultSourcehutPath = ".builds"

doSourcehut :: [String] -> FilePath -> Options -> IO ()
doSourcehut args path opts = do
    contents <- sourcehutFromConfigFile args opts path
    case optOutput opts of
        Nothing              -> do
            createDir defaultSourcehutPath
            for_ (M.toList contents) $ \(fn, content) ->
              BS.writeFile (defaultSourcehutPath </> (fn ++ ".yml")) content
        Just OutputStdout    -> case M.toList contents of
          [(_,content)] -> BS.putStr content
          _ -> fail "Cannot print multiple files to standard output"
        Just (OutputFile fp) -> do
            createDir fp
            for_ (M.toList contents) $ \(fn, content) ->
              BS.writeFile (fp </> (fn ++ ".yml")) content
  where
    createDir p = createDirectoryIfMissing True p

sourcehutFromConfigFile
    :: forall m. (MonadIO m, MonadDiagnostics m, MonadMask m)
    => [String]
    -> Options
    -> FilePath
    -> m (M.Map FilePath ByteString)
sourcehutFromConfigFile args opts path = do
    gitconfig <- liftIO readGitConfig
    cabalFiles <- getCabalFiles (optInputType' opts path) path
    config' <- findConfigFile (optConfig opts)
    let config = optConfigMorphism opts config'
    pkgs <- T.mapM (configFromCabalFile config) cabalFiles
    (ghcs, prj) <- case checkVersions (cfgTestedWith config) pkgs of
        Right x     -> return x
        Left []     -> putStrLnErr "panic: checkVersions failed without errors"
        Left (e:es) -> putStrLnErrs (e :| es)

    let prj' | cfgGhcHead config = over (mapped . field @"pkgJobs") (S.insert GHCHead) prj
             | otherwise         = prj

    ls <- genSourcehutFromConfigs args config gitconfig prj' ghcs
    return ls -- TODO patchSourcehut config ls

genSourcehutFromConfigs
    :: (Monad m, MonadIO m, MonadDiagnostics m)
    => [String]
    -> Config
    -> GitConfig
    -> Project URI Void Package
    -> Set CompilerVersion
    -> m (M.Map FilePath ByteString)
genSourcehutFromConfigs argv config gitconfig prj vs = do
    let jobVersions = makeJobVersions config vs
    case makeSourcehut argv config gitconfig prj jobVersions of
        Left err     -> putStrLnErr $ displayException err
        Right sourcehut -> do
            describeJobs "Sourcehut config" (cfgTestedWith config) jobVersions (prjPackages prj)
            return $ toUTF8BS . prettyYaml id . reann (sourcehutHeader (cfgInsertVersion config) argv ++) . toYaml
                <$> sourcehutManifests sourcehut

regenerateSourcehut :: Options -> IO ()
regenerateSourcehut opts = do
    -- change the directory
    for_ (optCwd opts) setCurrentDirectory

    yamls <- filter (isSuffixOf ".yml") <$> getDirectoryContents' defaultSourcehutPath
    let fp = case yamls of [] -> ".build.yml"; f : _ -> defaultSourcehutPath </> f
    -- read, and then change to the directory
    withContents fp noSourcehutScript $ \contents -> case findRegendataArgv contents of
        Nothing     -> do
            hPutStrLn stderr $ "Error: expected REGENDATA line in " ++ fp
            exitFailure

        Just (mversion, argv) -> do
            -- warn if we regenerate using older haskell-ci
            for_ mversion $ \version -> for_ (simpleParsec haskellCIVerStr) $ \haskellCIVer ->
                when (haskellCIVer < version) $ do
                    hPutStrLn stderr $ "Regenerating using older haskell-ci-" ++ haskellCIVerStr
                    hPutStrLn stderr $ "File generated using haskell-ci-" ++ prettyShow version

            -- Warn about outdated .yml files. to be safe, we don't delete them all.
            putStrLnWarn "Outdated .yml files will not be deleted"
            (f, opts') <- parseOptions argv
            doSourcehut argv f ( optionsWithOutputFile fp <> opts' <> opts)
  where
    noSourcehutScript :: IO ()
    noSourcehutScript = putStrLn $ "No " ++ defaultSourcehutPath ++ "/*.yml or .build.yml, skipping Sourcehut config regeneration"
    getDirectoryContents' :: FilePath -> IO [FilePath]
    getDirectoryContents' fp =
      getDirectoryContents fp
      `catchIOError`
      \e -> if isDoesNotExistError e then return [] else ioError e

-------------------------------------------------------------------------------
-- Config file
-------------------------------------------------------------------------------

findConfigFile :: MonadIO m => ConfigOpt -> m Config
findConfigFile ConfigOptNo    = return emptyConfig
findConfigFile (ConfigOpt fp) = readConfigFile fp
findConfigFile ConfigOptAuto  = do
    let defaultPath = "cabal.haskell-ci"
    exists <- liftIO (doesFileExist defaultPath)
    if exists
    then readConfigFile defaultPath
    else return emptyConfig

-------------------------------------------------------------------------------
-- Patches
-------------------------------------------------------------------------------

patchTravis
    :: (MonadIO m, MonadMask m)
    => Config -> ByteString -> m ByteString
patchTravis = patchYAML . cfgTravisPatches

patchGitHub
    :: (MonadIO m, MonadMask m)
    => Config -> ByteString -> m ByteString
patchGitHub = patchYAML . cfgGitHubPatches

-- | Adjust the generated YAML output with patch files, if specified.
-- We do this in a temporary file in case the user did not pass --output (as
-- it would be awkward to patch the generated output otherwise).
patchYAML
    :: (MonadIO m, MonadMask m)
    => [FilePath] -> ByteString -> m ByteString
patchYAML patches input
  | null patches = pure input
  | otherwise =
      withSystemTempFile "yml.tmp" $ \fp h -> liftIO $ do
        BS.hPutStr h input
        hClose h
        for_ patches $ applyPatch fp
        BS.readFile fp
  where
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

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

withContents
    :: FilePath            -- ^ filepath
    -> IO r                -- ^ what to do when file don't exist
    -> (String -> IO r)    -- ^ continuation
    -> IO r
withContents path no kont = do
    e <- try (BS.readFile path) :: IO (Either IOError BS.ByteString)
    case e of
        Left _         -> no
        Right contents -> kont (fromUTF8BS contents)

-- | Find @REGENDATA@ in a string
findRegendataArgv :: String -> Maybe (Maybe Version, [String])
findRegendataArgv contents = do
    l <- findMaybe (afterInfix "REGENDATA") (lines contents)
    first simpleParsec <$> (readMaybe l :: Maybe (String, [String]))
        <|> (,) Nothing <$> (readMaybe l :: Maybe [String])

-- | Read project file and associated .cabal files.
getCabalFiles
    :: (MonadDiagnostics m, MonadIO m)
    => InputType
    -> FilePath
    -> m (Project URI Void (FilePath, GenericPackageDescription))
getCabalFiles InputTypeProject path = do
    contents <- liftIO $ BS.readFile path
    prj0 <- either (putStrLnErr . renderParseError) return $ parseProject path contents
    prj1 <- either (putStrLnErr . renderResolveError) return =<< liftIO (resolveProject path prj0)
    either (putStrLnErr . renderParseError) return =<< liftIO (readPackagesOfProject prj1)
getCabalFiles InputTypePackage path = do
    e <- liftIO $ readPackagesOfProject (emptyProject & field @"prjPackages" .~ [path])
    either (putStrLnErr . renderParseError) return e

-------------------------------------------------------------------------------
-- Config
-------------------------------------------------------------------------------

configFromCabalFile
    :: (MonadIO m, MonadDiagnostics m)
    => Config -> (FilePath, GenericPackageDescription) -> m Package
configFromCabalFile cfg (cabalFile, gpd) = do
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
