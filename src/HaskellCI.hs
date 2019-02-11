{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

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
    Result (..),
    Diagnostic (..),
    parseTravis,
    formatDiagnostic, formatDiagnostics,
    travisFromConfigFile, MakeTravisOutput, Options (..), defaultOptions,
    ) where

import Prelude ()
import Prelude.Compat

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (when, unless, liftM, forM_, mzero)
import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import Data.Function
import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Map as M
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Exit
import System.FilePath.Posix ((</>), takeDirectory, takeFileName, takeExtension)
import System.IO
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe
import System.Environment (getArgs)
import Control.Monad.Trans.Writer
import Text.Read (readMaybe)
import Distribution.Compat.ReadP (readP_to_S)


import qualified Options.Applicative         as O

import Distribution.Compiler (CompilerFlavor(..))
import Distribution.Package hiding (Package, pkgName)
import qualified Distribution.Package as Pkg
import Distribution.PackageDescription (GenericPackageDescription,packageDescription, testedWith, package, condLibrary, condTestSuites)
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import qualified Distribution.PackageDescription as PD
import qualified Distribution.ParseUtils as PU
import Distribution.Text
import Distribution.Version
#if MIN_VERSION_Cabal(2,2,0)
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)
#elif MIN_VERSION_Cabal(2,0,0)
import Distribution.PackageDescription.Parse (readGenericPackageDescription)
#else
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Verbosity (Verbosity)
#endif

#ifdef MIN_VERSION_ShellCheck
import ShellCheck.Checker (checkScript)
import qualified ShellCheck.Interface as SC
import qualified ShellCheck.Formatter.Format as SC
import qualified ShellCheck.Formatter.TTY as SC.TTY
import System.IO.Unsafe (unsafePerformIO)
#endif

#if MIN_VERSION_base(4,9,0)
import Data.Semigroup (Semigroup (..))
#else
import Data.Monoid ((<>))
#endif

-- lens
import Lens.Micro
import Data.Generics.Labels () -- IsLabel (->) ...

import HaskellCI.Cli
import HaskellCI.Config
import HaskellCI.Config.ConstraintSet
import HaskellCI.Config.Doctest
import HaskellCI.Config.Dump
import HaskellCI.Config.Folds
import HaskellCI.Config.HLint
import HaskellCI.Config.Installed
import HaskellCI.Config.Jobs
import HaskellCI.Extras
import HaskellCI.GHC
import HaskellCI.Glob
import HaskellCI.Optimization
import HaskellCI.Project

-------------------------------------------------------------------------------
-- Script
-------------------------------------------------------------------------------

-- |  Encode shell command to be YAML safe and (optionally) ShellCheck it.
sh :: String -> String
sh = sh'
    [ 2034 -- VAR appears unused. Verify it or export it.
    , 2086 -- SC2086: Double quote to prevent globbing and word splitting.
    , 2002 -- SC2002: Useless cat. Consider 'cmd < file | ..' or 'cmd file | ..' instead.
    ]

shForJob :: Set Version -> VersionRange -> String -> String
shForJob  versions vr cmd
    | all (`withinRange` vr) versions = sh cmd
    | otherwise                       = sh $ unwords
        [ "if"
        , ghcVersionPredicate vr
        , "; then"
        , cmd
        , "; fi"
        ]

-- | Like 'sh' but with explicit SC exclude codes.
sh' :: [Integer] -> String -> String
#ifndef MIN_VERSION_ShellCheck
sh' _ = shImpl
#else
sh' excl cmd = unsafePerformIO $ do
  res <- checkScript iface spec
  if null (SC.crComments res)
     then return (shImpl cmd)
     else SC.onResult scFormatter res iface >> fail ("ShellCheck! " ++ cmd)
  where
    iface = SC.SystemInterface $ \n -> return $ Left $ "cannot read file: " ++ n
    spec  = SC.emptyCheckSpec { SC.csFilename = "stdin"
                              , SC.csScript = cmd
                              , SC.csExcludedWarnings = excl
                              , SC.csShellTypeOverride = Just SC.Sh
                              }

scFormatter :: SC.Formatter
scFormatter = unsafePerformIO (SC.TTY.format (SC.newFormatterOptions { SC.foColorOption = SC.ColorAlways }))
#endif

-- Non-ShellCheck version of sh'
shImpl :: String -> String
shImpl cmd
    | ':' `elem` cmd = "  - " ++ show cmd
    | otherwise      = "  - " ++ cmd

comment :: String -> String
comment = ("  # " ++)

type MakeTravisOutput = Result Diagnostic [String]

data Diagnostic
    = Info String
    | Warn String
    | Error String
  deriving (Eq, Show)

formatDiagnostics :: [Diagnostic] -> String
formatDiagnostics = unlines . map formatDiagnostic

formatDiagnostic :: Diagnostic -> String
formatDiagnostic (Error s) = "*ERROR* " ++ s
formatDiagnostic (Warn  s) = "*WARNING* " ++ s
formatDiagnostic (Info  s) = "*INFO* " ++ s

-- MaybeT is used to preserve the short-circuiting semantics of 'putStrLnErr'.
type YamlWriter m a = MaybeT (WriterT MakeTravisOutput m) a

putStrLnErr :: Monad m => String -> YamlWriter m a
putStrLnErr m = do
    lift . tell $ Failure [Error m]
    mzero

putStrLnErrs :: Monad m => [String] -> YamlWriter m ()
putStrLnErrs [] = return ()
putStrLnErrs ms = do
    lift (tell (Failure (map Error ms)))
    mzero

putStrLnWarn, putStrLnInfo :: Monad m => String -> YamlWriter m ()
putStrLnWarn m = lift . tell $ Success [Warn m] []
putStrLnInfo m = lift . tell $ Success [Info m] []

tellStrLn :: Monad m => String -> YamlWriter m ()
tellStrLn str = lift . tell $ success [str]

tellStrLns :: Monad m => [String] -> YamlWriter m ()
tellStrLns = lift . tell . success

foldedTellStrLns
    :: Monad m
    => Fold
    -> String
    -> Set Fold
    -> YamlWriter m ()
    -> YamlWriter m ()
foldedTellStrLns label = foldedTellStrLns' label ""

foldedTellStrLns'
    :: Monad m
    => Fold
    -> String
    -> String
    -> Set Fold
    -> YamlWriter m ()
    -> YamlWriter m ()
foldedTellStrLns' label pfx prettyLabel labels output
    | label `S.notMember` labels = output
    | otherwise = tellStrLns [prologue] >> output >> tellStrLns epilogue
  where
    prologue = mconcat
        [ "  - echo ", prettyLabel
        , " && echo -en 'travis_fold:start:", showFold' label, "\\\\r'" ]
    epilogue = ["  - echo -en 'travis_fold:end:" ++ showFold' label ++ "\\\\r'" ]

    showFold' l = showFold l ++ if null pfx then "" else "-" ++ pfx

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
            let fp = ".travis.yml" -- make configurable?
            contents <- readFile fp
            case findArgv (lines contents) of
                Nothing     -> do
                    hPutStrLn stderr $ "Error: expected REGENDATA line in " ++ fp
                    exitFailure
                Just argv   -> do
                    (f, opts') <- parseTravis argv
                    doTravis argv f (opts' <> opts)
        CommandTravis f -> doTravis argv0 f opts
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

doTravis :: [String] -> FilePath -> Options -> IO ()
doTravis args path opts = do
    runYamlWriter (optOutput opts) $ travisFromConfigFile args opts path

runYamlWriter :: Maybe FilePath -> YamlWriter IO () -> IO ()
runYamlWriter mfp m = do
    result <- execWriterT (runMaybeT m)
    case result of
        Failure (formatDiagnostics -> errors) -> hPutStr stderr errors >> exitFailure
        Success (formatDiagnostics -> warnings) (unlines -> contents) -> do
            contents' <- evaluate (force contents)
            hPutStr stderr warnings
            case mfp of
                Nothing -> putStr contents'
                Just fp -> writeFile fp contents'

data Package = Pkg
    { pkgName :: String
    , pkgDir :: FilePath
    , pkgGpd :: GenericPackageDescription
    } deriving (Eq, Show)

travisFromConfigFile
    :: MonadIO m
    => [String]
    -> Options
    -> FilePath
    -> YamlWriter m ()
travisFromConfigFile args opts path = do
    cabalFiles <- getCabalFiles
    config' <- maybe (return emptyConfig) readConfigFile (optConfig opts)
    let config = optConfigMorphism opts config'
    pkgs <- T.mapM (configFromCabalFile config opts) cabalFiles
    (ghcs, prj) <- checkVersions pkgs
    genTravisFromConfigs args opts isCabalProject config prj ghcs
  where
    checkVersions
        :: MonadIO m
        => Project (Package, Set Version)
        -> YamlWriter m (Set Version, Project Package)
    checkVersions prj | null (prjPackages prj) = putStrLnErr "Error reading cabal file(s)!"
    checkVersions prj = do
        let (errors, names) = F.foldl' collectConfig mempty prj
        putStrLnErrs errors
        return (allVersions, prj { prjPackages = names })
      where
        allVersions = F.foldMap snd prj

        collectConfig
            :: ([String], [Package])
            -> (Package, Set Version)
            -> ([String], [Package])
        collectConfig aggregate (pkg, testWith) =
            aggregate <> (errors, [pkg])
          where
            symDiff a b = S.union a b `S.difference` S.intersection a b
            diff = symDiff testWith allVersions
            missingVersions = map display $ S.toList diff
            errors | S.null diff = []
                   | otherwise = pure $ mconcat
                        [ pkgName pkg
                        , " is missing tested-with annotations for: "
                        ] ++ intercalate "," missingVersions

    isCabalProject :: Maybe FilePath
    isCabalProject
        | "cabal.project" `isPrefixOf` takeFileName path = Just path
        | otherwise = Nothing

    getCabalFiles :: MonadIO m => YamlWriter m (Project FilePath)
    getCabalFiles
        | isNothing isCabalProject = return $ emptyProject & #prjPackages .~ [path]
        | otherwise = do
            contents <- liftIO $ BS.readFile path
            pkgs <- either putStrLnErr return $ parseProjectFile path contents
            over #prjPackages concat `liftM` T.mapM findProjectPackage pkgs

    rootdir = takeDirectory path

    -- See findProjectPackages in cabal-install codebase
    -- this is simple variant.
    findProjectPackage :: MonadIO m => String -> YamlWriter m [FilePath]
    findProjectPackage pkglocstr = do
        mfp <- checkisFileGlobPackage pkglocstr `mplusMaybeT`
               checkIsSingleFilePackage pkglocstr
        maybe (putStrLnErr $ "bad package location: " ++ pkglocstr) return mfp

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

configFromCabalFile
    :: MonadIO m => Config ->  Options -> FilePath -> YamlWriter m (Package, Set Version)
configFromCabalFile cfg opts cabalFile = do
    gpd <- liftIO $ readGenericPackageDescription maxBound cabalFile

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

    let knownGhcVersions'
            | cfgLastInSeries cfg = filterLastMajor knownGhcVersions
            | otherwise           = knownGhcVersions

    let testedGhcVersions = filter (`withinRange` ghcVerConstrs') knownGhcVersions'

    when (null testedGhcVersions) $ do
        putStrLnErr "no known GHC version is allowed by the 'tested-with' specification"

    forM_ (optCollections opts) $ \c -> do
        let v = collToGhcVer c
        unless (v `elem` testedGhcVersions) $
            putStrLnErr $ unlines
               [ "collection " ++ c ++ " requires GHC " ++ display v
               , "add 'tested-width: GHC == " ++ display v ++ "' to your .cabal file"
               ]

    let pkg = Pkg pkgNameStr (takeDirectory cabalFile) gpd

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
        t _                             = Nothing

    filterLastMajor = map maximum . groupBy ((==) `on` ghcMajVer)

genTravisFromConfigs
    :: Monad m
    => [String]
    -> Options
    -> Maybe FilePath
    -> Config
    -> Project Package
    -> Set Version
    -> YamlWriter m ()
genTravisFromConfigs argv opts isCabalProject config prj@Project { prjPackages = pkgs } versions' = do
    let folds = cfgFolds config

    putStrLnInfo $
        "Generating Travis-CI config for testing for GHC versions: " ++ ghcVersions

    unless (null $ cfgOsx config) $  do
        putStrLnInfo $ "Also OSX jobs for: " ++ ghcOsxVersions
        unless (S.null omittedOsxVersions) $
            putStrLnWarn $ "Not all GHC versions specified with --osx are generated: " ++ ghcOmittedOsxVersions

    ---------------------------------------------------------------------------
    -- travis.yml generation starts here

    tellStrLns
        [ "# This Travis job script has been generated by a script via"
        , "#"
        , "#   haskell-ci " ++ unwords [ "'" ++ a ++ "'" | a <- argv ]
        , "#"
        , "# For more information, see https://github.com/haskell-CI/haskell-ci"
        , "#"
        , "language: c"
        , "dist: xenial"
        , ""
        , "git:"
        , "  submodules: false  # whether to recursively clone submodules"
        , ""
        ]

    let projectName = fromMaybe (pkgName $ head pkgs) (cfgProjectName config)
    unless (null $ cfgIrcChannels config) $ tellStrLns $
        [ "notifications:"
        , "  irc:"
        , "    channels:"
        ] ++
        [ "      - \"" ++ chan ++ "\"" | chan <- cfgIrcChannels config ] ++
        [ "    skip_join: true"
        , "    template:"
        , "      - \"\\x0313" ++ projectName ++ "\\x03/\\x0306%{branch}\\x03 \\x0314%{commit}\\x03 %{build_url} %{message}\""
        , ""
        ]

    unless (null $ cfgOnlyBranches config) $ tellStrLns $
        [ "branches:"
        , "  only:"
        ] ++
        [ "    - " ++ branch
        | branch <- cfgOnlyBranches config
        ] ++
        [ ""
        ]

    when (cfgCache config) $ tellStrLns
        [ "cache:"
        , "  directories:"
        , "    - $HOME/.cabal/packages"
        , "    - $HOME/.cabal/store"
        ]

    -- on OSX ghc is installed in $HOME so we can cache it
    -- independently of linux
    when (cfgCache config && not (null (cfgOsx config))) $ tellStrLns
        [ "    - $HOME/.ghc-install"
        ]

    when (cfgCache config) $ tellStrLns
        [ ""
        , "before_cache:"
        , "  - rm -fv $HOME/.cabal/packages/hackage.haskell.org/build-reports.log"
        , "  # remove files that are regenerated by 'cabal update'"
        , "  - rm -fv $HOME/.cabal/packages/hackage.haskell.org/00-index.*" -- legacy
        , "  - rm -fv $HOME/.cabal/packages/hackage.haskell.org/*.json" -- TUF meta-data
        , "  - rm -fv $HOME/.cabal/packages/hackage.haskell.org/01-index.cache"
        , "  - rm -fv $HOME/.cabal/packages/hackage.haskell.org/01-index.tar"
        , "  - rm -fv $HOME/.cabal/packages/hackage.haskell.org/01-index.tar.idx"
        , ""
        , "  - rm -rfv $HOME/.cabal/packages/head.hackage" -- if we cache, it will break builds.
        , ""
        ]

    tellStrLn "matrix:"
    tellStrLn "  include:"

    let colls = [ (collToGhcVer cid,cid) | cid <- reverse $ optCollections opts ]

    let tellJob :: Monad m => Bool -> Maybe Version -> YamlWriter m ()
        tellJob osx gv = do
            let cvs = dispGhcVersion $ gv >> cfgCabalInstallVersion config
                gvs = dispGhcVersion gv

                xpkgs' = concatMap (',':) (S.toList $ cfgApt config)

                colls' = [ cid | (v,cid) <- colls, Just v == gv ]

            tellStrLns
                [ "    - compiler: \"ghc-" <> gvs <> "\""
                , if | Just e <- gv >>= \v -> M.lookup v (cfgEnv config)
                                     -> "      env: " ++ e
                     | previewGHC gv -> "      env: GHCHEAD=true"
                     | null colls'   -> "    # env: TEST=--disable-tests BENCH=--disable-benchmarks"
                     | otherwise     -> "      env: 'COLLECTIONS=" ++ intercalate "," colls' ++ "'"
                , "      addons: {apt: {packages: [ghc-ppa-tools,cabal-install-" <> cvs <> ",ghc-" <> gvs <> xpkgs' <> "], sources: [hvr-ghc]}}"
                ]

            when osx $ tellStrLns
                [ "      os: osx"
                ]

    -- newer GHC first, -head last (which is great).
    -- Alpha release would go first though.
    F.forM_ (reverse $ S.toList versions) $ tellJob False
    F.forM_ (reverse $ S.toList osxVersions) $ tellJob True . Just

    let allowFailures = headGhcVers `S.union` S.map Just (cfgAllowFailures config)
    unless (S.null allowFailures) $ do
        tellStrLn ""
        tellStrLn "  allow_failures:"

        F.forM_ allowFailures $ \gv -> do
            let gvs = dispGhcVersion gv
            tellStrLn $ concat [ "    - compiler: \"ghc-", gvs, "\"" ]

    tellStrLns
        [ ""
        , "before_install:"
        , sh "HC=${CC}"
        , sh' [2034,2039] "HCPKG=${HC/ghc/ghc-pkg}" -- SC2039. In POSIX sh, string replacement is undefined.
        , sh "unset CC"
        -- rootdir is useful for manual script additions
        , sh "ROOTDIR=$(pwd)"
        , sh "mkdir -p $HOME/.local/bin"
        ]

    let haskellOnMacos = "https://haskell.futurice.com/haskell-on-macos.py"

    if null (cfgOsx config)
    then tellStrLns
        [ sh "PATH=/opt/ghc/bin:/opt/ghc-ppa-tools/bin:$HOME/local/bin:$PATH"
        ]
    else tellStrLns
        [ sh $ "if [ \"$(uname)\" = \"Darwin\" ]; then brew update; brew upgrade python@3; curl " ++ haskellOnMacos ++ " | python3 - --make-dirs --install-dir=$HOME/.ghc-install --cabal-alias=head install cabal-install-head ${HC}; fi"
        , sh $ "if [ \"$(uname)\" = \"Darwin\" ]; then PATH=$HOME/.ghc-install/ghc/bin:$HOME/local/bin:$PATH; else PATH=/opt/ghc/bin:/opt/ghc-ppa-tools/bin:$HOME/local/bin:$PATH; fi"
        ]

    -- HCNUMVER, numeric HC version, e.g. ghc 7.8.4 is 70804 and 7.10.3 is 71003
    tellStrLns
        [ sh $ "HCNUMVER=$(( $(${HC} --numeric-version|sed -E 's/([0-9]+)\\.([0-9]+)\\.([0-9]+).*/\\1 * 10000 + \\2 * 100 + \\3/') ))"
        , sh "echo $HCNUMVER"
        ]

    unless (null colls) $
       tellStrLn " - IFS=', ' read -a COLLS <<< \"$COLLECTIONS\""

    tellStrLns
        [ ""
        , "install:"
        , sh "cabal --version"
        , sh "echo \"$(${HC} --version) [$(${HC} --print-project-git-commit-id 2> /dev/null || echo '?')]\""
        , sh "BENCH=${BENCH---enable-benchmarks}"
        , sh "TEST=${TEST---enable-tests}"
        , sh "UNCONSTRAINED=${UNCONSTRAINED-true}"
        , sh "GHCHEAD=${GHCHEAD-false}"
        ]

    -- Update hackage index. Side-effect: ~/.cabal.config is created.
    tellStrLns
        [ sh "travis_retry cabal update -v"
        , sh "sed -i.bak 's/^jobs:/-- jobs:/' ${HOME}/.cabal/config"
        , sh "rm -fv cabal.project cabal.project.local"
        ]

    -- Cabal jobs
    case cfgJobs config >>= cabalJobs of
        Just n -> tellStrLns
            [ sh $ "sed -i.bak 's/^-- jobs:.*/jobs: " ++ show n ++ "/' ${HOME}/.cabal/config"
            ]
        _ -> return ()

    -- GHC jobs
    case cfgJobs config >>= ghcJobs of
        Just m -> tellStrLns
            [ shForJob versions' (orLaterVersion (mkVersion [7,8])) $
              "sed -i.bak 's/-- ghc-options:.*/ghc-options: -j" ++ show m ++ "/' ${HOME}/.cabal/config"
            ]
        _ -> return ()

    -- Add head.hackage repository to ~/.cabal/config
    -- (locally you want to add it to cabal.project)
    unless (S.null headGhcVers) $ tellStrLns
        [ "  # Overlay Hackage Package Index for GHC HEAD: https://github.com/hvr/head.hackage"
        , "  - |"
        , "    if $GHCHEAD; then"
        , "      sed -i 's/-- allow-newer: .*/allow-newer: *:base/' ${HOME}/.cabal/config"
        , "      for pkg in $($HCPKG list --simple-output); do pkg=$(echo $pkg | sed 's/-[^-]*$//'); sed -i \"s/allow-newer: /allow-newer: *:$pkg, /\" ${HOME}/.cabal/config; done"
        , ""
        , "      echo 'repository head.hackage'                                                        >> ${HOME}/.cabal/config"
        , "      echo '   url: http://head.hackage.haskell.org/'                                       >> ${HOME}/.cabal/config"
        , "      echo '   secure: True'                                                                >> ${HOME}/.cabal/config"
        , "      echo '   root-keys: 07c59cb65787dedfaef5bd5f987ceb5f7e5ebf88b904bbd4c5cbdeb2ff71b740' >> ${HOME}/.cabal/config"
        , "      echo '              2e8555dde16ebd8df076f1a8ef13b8f14c66bad8eafefd7d9e37d0ed711821fb' >> ${HOME}/.cabal/config"
        , "      echo '              8f79fd2389ab2967354407ec852cbe73f2e8635793ac446d09461ffb99527f6e' >> ${HOME}/.cabal/config"
        , "      echo '   key-threshold: 3'                                                            >> ${HOME}/.cabal.config"
        , ""
        , "      grep -Ev -- '^\\s*--' ${HOME}/.cabal/config | grep -Ev '^\\s*$'"
        , ""
        , "      cabal new-update head.hackage -v"
        , "    fi"
        ]

    -- Output cabal.config
    tellStrLns
        [ sh "grep -Ev -- '^\\s*--' ${HOME}/.cabal/config | grep -Ev '^\\s*$'"
        ]

    -- Install doctest
    let doctestVersionConstraint
            | isAnyVersion (cfgDoctestVersion doctestConfig) = ""
            | otherwise = " --constraint='doctest " ++ display (cfgDoctestVersion doctestConfig) ++ "'"
    when (cfgDoctestEnabled doctestConfig) $ tellStrLns
        [ shForJob versions' doctestJobVersionRange $ "cabal new-install -w ${HC} -j2 --symlink-bindir=$HOME/.local/bin doctest" ++ doctestVersionConstraint
        ]

    -- Install hlint
    let hlintVersionConstraint
            | isAnyVersion (cfgHLintVersion hlintConfig) = ""
            | otherwise = " --constraint='hlint " ++ display (cfgHLintVersion hlintConfig) ++ "'"
    when (cfgHLintEnabled hlintConfig) $ tellStrLns
        [ shForJob versions' (hlintJobVersionRange versions (cfgHLintJob hlintConfig)) $
          "cabal new-install -w ${HC} -j2 --symlink-bindir=$HOME/.local/bin hlint" ++ hlintVersionConstraint
        ]

    -- create cabal.project file
    generateCabalProject False

    let pkgFilter = intercalate " | " $ map (wrap.pkgName) pkgs
        wrap s = "grep -Fv \"" ++ s ++ " ==\""
    unless (null colls) $ tellStrLns
        [ "  - for COLL in \"${COLLS[@]}\"; do"
        , "      echo \"== collection $COLL ==\";"
        , "      ghc-travis collection ${COLL} > /dev/null || break;"
        , "      ghc-travis collection ${COLL} | " ++ pkgFilter ++ " > cabal.project.freeze;"
        , "      grep ' collection-id' cabal.project.freeze;"
        , "      rm -rf dist-newstyle/;"
        , "      cabal new-build -w ${HC} ${TEST} ${BENCH} --project-file=\"" ++ projectFile ++ "\" --dep -j2 all;"
        , "    done"
        , ""
        ]

    forM_ pkgs $ \Pkg{pkgDir} -> tellStrLns
        [ "  - if [ -f \"" ++ pkgDir ++ "/configure.ac\" ]; then"
        , "      (cd \"" ++ pkgDir ++ "\" && autoreconf -i);"
        , "    fi"
        ]

    let quotedRmPaths =
          ".ghc.environment.*"
          ++ " " ++
          quotedPaths (\Pkg{pkgDir} -> pkgDir ++ "/dist")

    tellStrLns
        [ sh $ "rm -f cabal.project.freeze"
        ]

    -- Install dependencies
    when (cfgInstallDeps config) $ do
        tellStrLns
            -- dump install plan
            [ sh $ "cabal new-freeze -w ${HC} ${TEST} ${BENCH} --project-file=\"" ++ projectFile ++"\" --dry"
            , sh $ "cat \"" ++ projectFile ++ ".freeze\" | sed -E 's/^(constraints: *| *)//' | sed 's/any.//'"
            , sh $ "rm  \"" ++ projectFile ++ ".freeze\""
            
            -- install dependencies
            , sh $ "cabal new-build -w ${HC} ${TEST} ${BENCH} --project-file=\"" ++ projectFile ++"\" --dep -j2 all"
            ]
        when (cfgNoTestsNoBench config) $ tellStrLns
            [ sh $ "cabal new-build -w ${HC} --disable-tests --disable-benchmarks --project-file=\"" ++ projectFile ++ "\" --dep -j2 all"
            ]

    tellStrLns
        [ sh $ "rm -rf " ++ quotedRmPaths
        , sh $  "DISTDIR=$(mktemp -d /tmp/dist-test.XXXX)"
        ]

    tellStrLns
        [ ""
        , "# Here starts the actual work to be performed for the package under test;"
        , "# any command which exits with a non-zero exit code causes the build to fail."
        , "script:"
        , "  # test that source-distributions can be generated"
        ]

    foldedTellStrLns FoldSDist "Packaging..." folds $ do
        tellStrLns
            [ sh $ "cabal new-sdist all"
            ]

    let tarFiles = "dist-newstyle" </> "sdist" </> "*.tar.gz"


    foldedTellStrLns FoldUnpack "Unpacking..." folds $ do
        tellStrLns
            [ sh $ "mv " ++ tarFiles ++ " ${DISTDIR}/"
            , sh $ "cd ${DISTDIR} || false" -- fail explicitly, makes SC happier
            , sh $ "find . -maxdepth 1 -name '*.tar.gz' -exec tar -xvf '{}' \\;"
            ]
        generateCabalProject True

    when (cfgNoTestsNoBench config) $ foldedTellStrLns FoldBuild "Building..." folds $ tellStrLns
        [ comment "this builds all libraries and executables (without tests/benchmarks)"
        , sh "cabal new-build -w ${HC} --disable-tests --disable-benchmarks all"
        ]

    tellStrLns [""]


    foldedTellStrLns FoldBuildEverything
        "Building with tests and benchmarks..." folds $ tellStrLns
        [ comment "build & run tests, build benchmarks"
        , sh "cabal new-build -w ${HC} ${TEST} ${BENCH} all"
        ]

    -- cabal new-test fails if there are no test-suites.
    when hasTests $
        foldedTellStrLns FoldTest "Testing..." folds $ tellStrLns
            [ sh $ mconcat
                [ "if [ \"x$TEST\" = \"x--enable-tests\" ]; then "
                , if cfgNoise config
                     then "cabal "
                     else "(set -o pipefail; cabal -vnormal+nowrap+markoutput "
                , "new-test -w ${HC} ${TEST} ${BENCH} all"
                , if cfgNoise config
                     then ""
                     else " 2>&1 | sed '/^-----BEGIN CABAL OUTPUT-----$/,/^-----END CABAL OUTPUT-----$/d' )"
                , "; fi"
                ]
            ]

    tellStrLns [""]

    when (cfgDoctestEnabled doctestConfig) $ do
        let doctestOptions = unwords $ cfgDoctestOptions doctestConfig
        tellStrLns [ comment "doctest" ]
        foldedTellStrLns FoldDoctest "Doctest..." folds $ do
            forM_ pkgs $ \Pkg{pkgName,pkgGpd} -> do
                let args = doctestArgs pkgGpd
                    args' = unwords args
                unless (null args) $ tellStrLns
                    [ shForJob versions' doctestJobVersionRange $
                      "(cd " ++ pkgName ++ "-* && doctest " ++ doctestOptions ++ " " ++ args' ++ ")"
                    ]
        tellStrLns [ "" ]

    when (cfgHLintEnabled hlintConfig) $ do
        let "" <+> ys = ys
            xs <+> "" = xs
            xs <+> ys = xs ++ " " ++ ys

            prependSpace "" = ""
            prependSpace xs = " " ++ xs

        let hlintOptions = prependSpace $ maybe "" ("-h ${ROOTDIR}/" ++) (cfgHLintYaml hlintConfig) <+> unwords (cfgHLintOptions hlintConfig)

        tellStrLns [ comment "hlint" ]
        foldedTellStrLns FoldHLint "HLint.." folds $ do
            forM_ pkgs $ \Pkg{pkgName,pkgGpd} -> do
                -- note: same arguments work so far for doctest and hlint
                let args = doctestArgs pkgGpd
                    args' = unwords args
                unless (null args) $ tellStrLns
                    [ shForJob versions' (hlintJobVersionRange versions (cfgHLintJob hlintConfig)) $
                      "(cd " ++ pkgName ++ "-* && hlint" ++ hlintOptions ++ " " ++ args' ++ ")"
                    ]
        tellStrLns [ "" ]

    when (cfgCheck config) $
        foldedTellStrLns FoldCheck "cabal check..." folds $ do
            tellStrLns [ comment "cabal check" ]
            forM_ pkgs $ \Pkg{pkgName} -> tellStrLns
                [ sh $ "(cd " ++ pkgName ++ "-* && cabal check)"

                ]
            tellStrLns [ "" ]

    when hasLibrary $
        foldedTellStrLns FoldHaddock "Haddock..." folds $ tellStrLns
            [ comment "haddock"
            , shForJob versions' (cfgHaddock config) "cabal new-haddock -w ${HC} ${TEST} ${BENCH} all"
            , ""
            ]

    unless (null colls) $
        foldedTellStrLns FoldStackage "Stackage builds..." folds $ tellStrLns
            [ "  # try building & testing for package collections"
            , "  - for COLL in \"${COLLS[@]}\"; do"
            , "      echo \"== collection $COLL ==\";"
            , "      ghc-travis collection ${COLL} > /dev/null || break;"
            , "      ghc-travis collection ${COLL} | " ++ pkgFilter ++ " > cabal.project.freeze;"
            , "      grep ' collection-id' cabal.project.freeze;"
            , "      rm -rf dist-newstyle/;"
            , "      cabal new-build -w ${HC} ${TEST} ${BENCH} all || break;"
            , "      if [ \"x$TEST\" = \"x--enable-tests\" ]; then cabal new-test -w ${HC} ${TEST} ${BENCH} all || break; fi;"
            , "    done"
            , ""
            ]

    -- Have to build last, as we remove cabal.project.local
    when (cfgUnconstrainted config) $ foldedTellStrLns FoldBuildInstalled
        "Building without installed constraints for packages in global-db..." folds $ tellStrLns
        [ comment "Build without installed constraints for packages in global-db"
        -- SC2046: Quote this to prevent word splitting.
        -- here we split on purpose!
        , sh' [2046, 2086] $ unwords
            [ "if $UNCONSTRAINED;"
            , "then rm -f cabal.project.local; cabal new-build -w ${HC} --disable-tests --disable-benchmarks all;"
            , "else echo \"Not building without installed constraints\"; fi"
            ]
        , ""
        ]

    -- and now, as we don't have cabal.project.local;
    -- we can test with other constraint sets
    let constraintSets = cfgConstraintSets config
    unless (null constraintSets) $ do
        tellStrLns
            [ comment "Constraint sets"
            , sh "rm -rf cabal.project.local"
            , ""
            ]
        forM_ constraintSets $ \cs -> do
            let name = csName cs
            let constraintFlags = concatMap (\x ->  " --constraint='" ++ x ++ "'") (csConstraints cs)
            let cmd | csRunTests cs = "cabal new-test  -w ${HC} --enable-tests  --enable-benchmarks"
                    | otherwise     = "cabal new-build -w ${HC} --disable-tests --disable-benchmarks"
            tellStrLns [ comment  "Constraint set " ++ name ]
            foldedTellStrLns' FoldConstraintSets name ("Constraint set " ++ name) folds $ tellStrLns
                [ shForJob versions' (csGhcVersions cs) $
                  cmd ++ " " ++ constraintFlags ++ " all"
                , ""
                ]
        tellStrLns [""]

    tellStrLns
        [ "# REGENDATA " ++ show argv
        , "# EOF"
        ]

    return ()
  where
    doctestConfig = cfgDoctest config
    hlintConfig   = cfgHLint config

    hasTests   = F.any (\Pkg{pkgGpd} -> not . null $ condTestSuites pkgGpd) pkgs
    hasLibrary = F.any (\Pkg{pkgGpd} -> isJust $ condLibrary pkgGpd) pkgs

    -- GHC versions which need head.hackage
    headGhcVers = S.filter previewGHC versions

    generateCabalProject dist = do
        tellStrLns
            [ sh $ "printf 'packages: " ++ cabalPaths ++ "\\n' > cabal.project"
            , sh $ "printf 'write-ghc-environment-files: always\\n' >> cabal.project"
            ]
        F.forM_ (prjConstraints prj) $ \xs -> do
            let s = concat (lines xs)
            tellStrLns
                [ sh $ "echo 'constraints: " ++ s ++ "' >> cabal.project"
                ]
        F.forM_ (prjAllowNewer prj) $ \xs -> do
            let s = concat (lines xs)
            tellStrLns
                [ sh $ "echo 'allow-newer: " ++ s ++ "' >> cabal.project"
                ]
        unless (null (cfgLocalGhcOptions config)) $ forM_ pkgs $ \Pkg{pkgName} -> do
            let s = unwords $ map (show . PU.showToken) $ cfgLocalGhcOptions config
            tellStrLns
                [ sh $ "echo 'package " ++ pkgName ++ "' >> cabal.project"
                , sh $ "echo '  ghc-options: " ++ s ++ "' >> cabal.project"
                ]

        when (prjReorderGoals prj) $
            tellStrLns
                [ sh $ "echo 'reorder-goals: True' >> cabal.project"
                ]

        F.forM_ (prjMaxBackjumps prj) $ \bj ->
            tellStrLns
                [ sh $ "echo 'max-backjumps: " ++ show bj ++ "' >> cabal.project"
                ]

        case prjOptimization prj of
            OptimizationOn      -> return ()
            OptimizationOff     -> tellStrLns [ sh $ "echo 'optimization: False' >> cabal.project " ]
            OptimizationLevel l -> tellStrLns [ sh $ "echo 'optimization: " ++ show l ++ "' >> cabal.project " ]

        -- also write cabal.project.local file with
        -- @
        -- constraints: base installed
        -- constraints: array installed
        -- ...
        --
        -- omitting any local package names
        case normaliseInstalled (cfgInstalled config) of
            InstalledDiff pns -> tellStrLns
                [ sh $ "touch cabal.project.local"
                , sh $ unwords
                    [ "for pkg in $($HCPKG list --simple-output); do"
                    , "echo $pkg"
                    , "| sed 's/-[^-]*$//'"
                    , "| grep -vE -- " ++ re
                    , "| sed 's/^/constraints: /'"
                    , "| sed 's/$/ installed/'"
                    , ">> cabal.project.local; done"
                    ]
                ]
              where
                pns' = S.map unPackageName pns `S.union` foldMap (S.singleton . pkgName) pkgs
                re = "'^(" ++ intercalate "|" (S.toList pns') ++ ")$'"

            InstalledOnly pns | not (null pns') -> tellStrLns
                [ sh $ "touch cabal.project.local"
                , sh' [2043] $ unwords
                    [ "for pkg in " ++ unwords (S.toList pns') ++ "; do"
                    , "echo \"constraints: $pkg installed\" >> cabal.project"
                    , ">> cabal.project.local; done"
                    ]
                ]
              where
                pns' = S.map unPackageName pns `S.difference` foldMap (S.singleton . pkgName) pkgs

            -- otherwise: nothing
            _ -> pure ()

        tellStrLns
            [ sh $ "cat cabal.project || true"
            , sh $ "cat cabal.project.local || true"
            ]
      where
        cabalPaths
            | dist      = quotedPaths $ \Pkg{pkgName} -> pkgName ++ "-*/*.cabal"
            | otherwise = quotedPaths $ \Pkg{pkgDir}  -> pkgDir

    projectFile :: FilePath
    projectFile = fromMaybe "cabal.project" isCabalProject

    quotedPaths :: (Package -> FilePath) -> String
    quotedPaths f = unwords $ map (f . quote) pkgs
      where
        quote pkg = pkg{ pkgDir = "\"" ++ pkgDir pkg ++ "\"" }

    showVersions :: Set (Maybe Version) -> String
    showVersions = unwords . map dispGhcVersion . S.toList

    -- specified ersions
    osxVersions' :: Set Version
    osxVersions' = cfgOsx config

    versions :: Set (Maybe Version)
    versions
        | cfgGhcHead config = S.insert Nothing $ S.map Just versions'
        | otherwise         = S.map Just versions'

    ghcVersions :: String
    ghcVersions = showVersions versions

    osxVersions, omittedOsxVersions :: Set Version
    (osxVersions, omittedOsxVersions) = S.partition (`S.member` versions') osxVersions'

    ghcOsxVersions :: String
    ghcOsxVersions = showVersions $ S.map Just osxVersions

    ghcOmittedOsxVersions :: String
    ghcOmittedOsxVersions = showVersions $ S.map Just omittedOsxVersions


collToGhcVer :: String -> Version
collToGhcVer cid = case simpleParse cid of
  Nothing -> error ("invalid collection-id syntax " ++ show cid)
  Just (PackageIdentifier n (versionNumbers -> v))
    | display n /= "lts" -> error ("unknown collection " ++ show cid)
    | isPrefixOf [0] v -> mkVersion [7,8,3]
    | isPrefixOf [1] v -> mkVersion [7,8,4]
    | isPrefixOf [2] v -> mkVersion [7,8,4]
    | isPrefixOf [3] v -> mkVersion [7,10,2]
    | isPrefixOf [4] v -> mkVersion [7,10,3]
    | isPrefixOf [5] v -> mkVersion [7,10,3]
    | isPrefixOf [6] v -> mkVersion [7,10,3]
    | isPrefixOf [7] v -> mkVersion [8,0,1]
    | otherwise -> error ("unknown collection " ++ show cid)

-------------------------------------------------------------------------------
-- Doctest
-------------------------------------------------------------------------------

doctestJobVersionRange :: VersionRange
doctestJobVersionRange = orLaterVersion $ mkVersion [8,0]

-- | Modules arguments to the library
--
-- * We check the library component
--
-- * If there are hs-source-dirs, use them
--
-- * otherwise use exposed + other modules
--
-- * Also add default-extensions
--
-- /Note:/ same argument work for hlint too!
--
doctestArgs :: GenericPackageDescription -> [String]
doctestArgs gpd = case PD.library $ flattenPackageDescription gpd of
    Nothing -> []
    Just l  -> exts ++ dirsOrMods
      where
        bi = PD.libBuildInfo l

        dirsOrMods
            | null (PD.hsSourceDirs bi) = map display (PD.exposedModules l)
            | otherwise = PD.hsSourceDirs bi

        exts = map (("-X" ++) . display) (PD.defaultExtensions bi)

-------------------------------------------------------------------------------
-- HLint
-------------------------------------------------------------------------------

hlintJobVersionRange :: Set (Maybe Version) -> HLintJob -> VersionRange
hlintJobVersionRange vs HLintJobLatest = case S.maxView vs of
    Just (Just v, _) -> thisVersion v
    _                -> thisVersion $ mkVersion [8,6,3]
hlintJobVersionRange _ (HLintJob v)   = thisVersion v

ghcVersionToString :: Version -> String
ghcVersionToString v =  case versionNumbers v of
    []        -> "0"
    [x]       -> show (x * 10000)
    [x,y]     -> show (x * 10000 + y * 100)
    (x:y:z:_) -> show (x * 10000 + y * 100 + z)

-------------------------------------------------------------------------------
-- Result
-------------------------------------------------------------------------------

data Result e a
    = Success [e] a
    | Failure [e]
    deriving (Eq, Show, Functor)

success :: a -> Result e a
success = Success []

instance Monoid a => Monoid (Result e a) where
    mempty = success mempty
    mappend = (<>)

instance Monoid a => Semigroup (Result e a) where
    Failure err1   <> Failure err2   = Failure $ err1 <> err2
    Failure err1   <> Success err2 _ = Failure $ err1 <> err2
    Success err1 _ <> Failure err2   = Failure $ err1 <> err2
    Success l1 o1  <> Success l2 o2  = Success (mappend l1 l2) (mappend o1 o2)

-------------------------------------------------------------------------------
-- ConstraintSet
-------------------------------------------------------------------------------

ghcVersionPredicate :: VersionRange -> String
ghcVersionPredicate = conj . asVersionIntervals
  where
    conj = intercalate "  ||  " . map disj

    disj :: VersionInterval -> String
    disj (LowerBound v InclusiveBound, UpperBound u InclusiveBound)
        | v == u              = "[ $HCNUMVER -eq " ++ f v ++ " ]"
    disj (lb, NoUpperBound)   = lower lb
    disj (lb, UpperBound v b) = lower lb ++ " && " ++ upper v b

    lower (LowerBound v InclusiveBound) = "[ $HCNUMVER -ge " ++ f v ++ " ]"
    lower (LowerBound v ExclusiveBound) = "[ $HCNUMVER -gt " ++ f v ++ " ]"

    upper v InclusiveBound = "[ $HCNUMVER -le " ++ f v ++ " ]"
    upper v ExclusiveBound = "[ $HCNUMVER -lt " ++ f v ++ " ]"

    f = ghcVersionToString
