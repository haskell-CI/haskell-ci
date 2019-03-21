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

import qualified Distribution.FieldGrammar                    as C
import qualified Distribution.Fields.Pretty                   as C
import qualified Distribution.PackageDescription.FieldGrammar as C
import qualified Distribution.Types.SourceRepo                as C
import qualified Distribution.Types.VersionRange              as C
import qualified Text.PrettyPrint                             as PP



#if MIN_VERSION_base(4,9,0)
import Data.Semigroup (Semigroup (..))
#else
import Data.Monoid ((<>))
#endif

-- lens
import Lens.Micro
import Data.Generics.Labels () -- IsLabel (->) ...

import qualified Distribution.Types.BuildInfo.Lens          as L
import qualified Distribution.Types.PackageDescription.Lens as L

import HaskellCI.Cli
import HaskellCI.Config
import HaskellCI.Config.CopyFields
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
import HaskellCI.MakeTravisOutput
import HaskellCI.Optimization
import HaskellCI.Package
import HaskellCI.Project
import HaskellCI.TestedWith
import HaskellCI.Version

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
    pkgs <- T.mapM (configFromCabalFile config) cabalFiles
    (ghcs, prj) <- case checkVersions (cfgTestedWith config) pkgs of
        Right x     -> return x
        Left errors -> putStrLnErrs errors >> mzero
    genTravisFromConfigs args config prj ghcs
  where
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
    :: MonadIO m => Config -> FilePath -> YamlWriter m (Package, Set Version)
configFromCabalFile cfg cabalFile = do
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
        t _                             = Nothing

    filterLastMajor = map maximum . groupBy ((==) `on` ghcMajVer)

genTravisFromConfigs
    :: Monad m
    => [String]
    -> Config
    -> Project Package
    -> Set Version
    -> YamlWriter m ()
genTravisFromConfigs argv config prj@Project { prjPackages = pkgs } versions' = do
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
        , rawRow $ "#   haskell-ci " ++ unwords [ "'" ++ a ++ "'" | a <- argv ]
        , "#"
        , "# For more information, see https://github.com/haskell-CI/haskell-ci"
        , "#"
        , rawRow $ "# version: " ++ haskellCIVerStr
        , "#"
        , "language: c"
        , "dist: xenial"
        , ""
        , "git:"
        , "  submodules: false  # whether to recursively clone submodules"
        , ""
        ]

    let projectName = fromMaybe (pkgName $ head pkgs) (cfgProjectName config)
    unless (null $ cfgIrcChannels config) $ tellStrLnsRaw $
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

    unless (null $ cfgOnlyBranches config) $ tellStrLnsRaw $
        [ "branches:"
        , "  only:"
        ] ++
        [ "    - " ++ branch
        | branch <- cfgOnlyBranches config
        ] ++
        [ ""
        ]

    -- cache directories
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

    tellStrLn ""

    -- postgresql
    when (cfgPostgres config) $ tellStrLns
        [ "services:"
        , "- postgresql"
        , "addons:"
        , "  postgresql: '10'"
        , ""
        ]

    -- before caching: clear some redundant stuff
    when (cfgCache config) $ tellStrLns
        [ "before_cache:"
        , "  - rm -fv $CABALHOME/packages/hackage.haskell.org/build-reports.log"
        , "  # remove files that are regenerated by 'cabal update'"
        , "  - rm -fv $CABALHOME/packages/hackage.haskell.org/00-index.*" -- legacy
        , "  - rm -fv $CABALHOME/packages/hackage.haskell.org/*.json" -- TUF meta-data
        , "  - rm -fv $CABALHOME/packages/hackage.haskell.org/01-index.cache"
        , "  - rm -fv $CABALHOME/packages/hackage.haskell.org/01-index.tar"
        , "  - rm -fv $CABALHOME/packages/hackage.haskell.org/01-index.tar.idx"
        , ""
        , "  - rm -rfv $CABALHOME/packages/head.hackage" -- if we cache, it will break builds.
        , ""
        ]

    tellStrLn "matrix:"
    tellStrLn "  include:"

    let tellJob :: Monad m => Bool -> Maybe Version -> YamlWriter m ()
        tellJob osx gv = do
            let cvs = dispGhcVersion $ gv >> cfgCabalInstallVersion config
                gvs = dispGhcVersion gv

                xpkgs' = concatMap (',':) (S.toList $ cfgApt config)

            tellStrLnsRaw $ catMaybes
                [ Just $ "    - compiler: \"ghc-" <> gvs <> "\""
                , if | Just e <- gv >>= \v -> M.lookup v (cfgEnv config)
                                     -> Just $ "      env: " ++ e
                     | previewGHC gv -> Just $ "      env: GHCHEAD=true"
                     | otherwise     -> Nothing
                , Just $ "      addons: {apt: {packages: [ghc-ppa-tools,cabal-install-" <> cvs <> ",ghc-" <> gvs <> xpkgs' <> "], sources: [hvr-ghc]}}"
                ]

            when osx $ tellStrLnsRaw
                [ "      os: osx"
                ]

    -- newer GHC first, -head last (which is great).
    -- Alpha release would go first though.
    F.forM_ (reverse $ S.toList versions) $ tellJob False
    F.forM_ (reverse $ S.toList osxVersions) $ tellJob True . Just

    let allowFailures = headGhcVers `S.union` S.map Just (S.filter (`C.withinRange` cfgAllowFailures config) versions')
    unless (S.null allowFailures) $ do
        tellStrLn ""
        tellStrLn "  allow_failures:"

        F.forM_ allowFailures $ \gv -> do
            let gvs = dispGhcVersion gv
            tellStrLn $ concat [ "    - compiler: \"ghc-", gvs, "\"" ]

    tellStrLns
        [ ""
        , "before_install:"
        , sh "HC=$(echo \"/opt/$CC/bin/ghc\" | sed 's/-/\\//')"
        , sh "HCPKG=\"$HC-pkg\""
        , sh "unset CC"
        -- cabal
        , sh "CABAL=/opt/ghc/bin/cabal"
        , sh "CABALHOME=$HOME/.cabal"
        -- PATH
        , sh "export PATH=\"$CABALHOME/bin:$PATH\""
        -- rootdir is useful for manual script additions
        , sh "ROOTDIR=$(pwd)"
        ]

    -- macOS installing
    let haskellOnMacos = "https://haskell.futurice.com/haskell-on-macos.py"
    unless (null (cfgOsx config)) $ tellStrLns
        [ sh $ "if [ \"$TRAVIS_OS_NAME\" = \"osx\" ]; then brew update; brew upgrade python@3; curl " ++ haskellOnMacos ++ " | python3 - --make-dirs --install-dir=$HOME/.ghc-install --cabal-alias=head install cabal-install-head ${TRAVIS_COMPILER}; fi"
        , sh' [2034,2039] "if [ \"$TRAVIS_OS_NAME\" = \"osx\" ]; then HC=$HOME/.ghc-install/ghc/bin/$TRAVIS_COMPILER; HCPKG=${HC/ghc/ghc-pkg}; CABAL=$HOME/.ghc-install/ghc/bin/cabal; fi"
        ]

    -- HCNUMVER, numeric HC version, e.g. ghc 7.8.4 is 70804 and 7.10.3 is 71003
    tellStrLns
        [ sh $ "HCNUMVER=$(( $(${HC} --numeric-version|sed -E 's/([0-9]+)\\.([0-9]+)\\.([0-9]+).*/\\1 * 10000 + \\2 * 100 + \\3/') ))"
        , sh "echo $HCNUMVER"
        ]

    tellStrLns
        [ ""
        , "install:"
        , sh "${CABAL} --version"
        , sh "echo \"$(${HC} --version) [$(${HC} --print-project-git-commit-id 2> /dev/null || echo '?')]\""
        , sh "TEST=--enable-tests"
        , shForJob versions' (invertVersionRange $ cfgTests config) "TEST=--disable-tests"
        , sh "BENCH=--enable-benchmarks"
        , shForJob versions' (invertVersionRange $ cfgBenchmarks config) "BENCH=--disable-benchmarks"
        , sh "GHCHEAD=${GHCHEAD-false}"
        ]

    -- Update hackage index. Side-effect: ~/.cabal.config is created.
    tellStrLns
        [ sh "travis_retry ${CABAL} update -v"
        , sh "sed -i.bak 's/^jobs:/-- jobs:/' $CABALHOME/config"
        , sh "rm -fv cabal.project cabal.project.local"
        ]

    -- Cabal jobs
    case cfgJobs config >>= cabalJobs of
        Just n -> tellStrLns
            [ sh $ "sed -i.bak 's/^-- jobs:.*/jobs: " ++ show n ++ "/' $CABALHOME/config"
            ]
        _ -> return ()

    -- GHC jobs
    case cfgJobs config >>= ghcJobs of
        Just m -> tellStrLns
            [ shForJob versions' (orLaterVersion (mkVersion [7,8])) $
              "sed -i.bak 's/-- ghc-options:.*/ghc-options: -j" ++ show m ++ "/' $CABALHOME/config"
            ]
        _ -> return ()

    -- Add head.hackage repository to ~/.cabal/config
    -- (locally you want to add it to cabal.project)
    unless (S.null headGhcVers) $ tellStrLns
        [ "  # Overlay Hackage Package Index for GHC HEAD: https://github.com/hvr/head.hackage"
        , "  - |"
        , "    if $GHCHEAD; then"
        , "      sed -i 's/-- allow-newer: .*/allow-newer: *:base/' $CABALHOME/config"
        , "      for pkg in $($HCPKG list --simple-output); do pkg=$(echo $pkg | sed 's/-[^-]*$//'); sed -i \"s/allow-newer: /allow-newer: *:$pkg, /\" $CABALHOME/config; done"
        , ""
        , "      echo 'repository head.hackage'                                                        >> $CABALHOME/config"
        , "      echo '   url: http://head.hackage.haskell.org/'                                       >> $CABALHOME/config"
        , "      echo '   secure: True'                                                                >> $CABALHOME/config"
        , "      echo '   root-keys: 07c59cb65787dedfaef5bd5f987ceb5f7e5ebf88b904bbd4c5cbdeb2ff71b740' >> $CABALHOME/config"
        , "      echo '              2e8555dde16ebd8df076f1a8ef13b8f14c66bad8eafefd7d9e37d0ed711821fb' >> $CABALHOME/config"
        , "      echo '              8f79fd2389ab2967354407ec852cbe73f2e8635793ac446d09461ffb99527f6e' >> $CABALHOME/config"
        , "      echo '   key-threshold: 3'                                                            >> $CABALHOME.config"
        , ""
        , "      grep -Ev -- '^\\s*--' $CABALHOME/config | grep -Ev '^\\s*$'"
        , ""
        , "      ${CABAL} new-update head.hackage -v"
        , "    fi"
        ]

    -- Output cabal.config
    tellStrLns
        [ sh "grep -Ev -- '^\\s*--' $CABALHOME/config | grep -Ev '^\\s*$'"
        ]

    -- Install doctest
    let doctestVersionConstraint
            | isAnyVersion (cfgDoctestVersion doctestConfig) = ""
            | otherwise = " --constraint='doctest " ++ display (cfgDoctestVersion doctestConfig) ++ "'"
    when doctestEnabled $ tellStrLns
        [ shForJob versions' doctestJobVersionRange $ "${CABAL} new-install -w ${HC} -j2 doctest" ++ doctestVersionConstraint
        ]

    -- Install hlint
    let hlintVersionConstraint
            | isAnyVersion (cfgHLintVersion hlintConfig) = ""
            | otherwise = " --constraint='hlint " ++ display (cfgHLintVersion hlintConfig) ++ "'"
    when (cfgHLintEnabled hlintConfig) $ tellStrLns
        [ shForJob versions' (hlintJobVersionRange versions (cfgHLintJob hlintConfig)) $
          "${CABAL} new-install -w ${HC} -j2 hlint" ++ hlintVersionConstraint
        ]

    -- create cabal.project file
    generateCabalProject False

    forM_ pkgs $ \Pkg{pkgDir} -> tellStrLns
        [ sh $ "if [ -f \"" ++ pkgDir ++ "/configure.ac\" ]; then (cd \"" ++ pkgDir ++ "\" && autoreconf -i); fi"
        ]

    let quotedRmPaths =
          ".ghc.environment.*"
          ++ " " ++
          unwords (quotedPaths (\Pkg{pkgDir} -> pkgDir ++ "/dist"))

    tellStrLns
        [ sh $ "rm -f cabal.project.freeze"
        ]

    -- Install dependencies
    when (cfgInstallDeps config) $ do
        tellStrLns
            -- dump install plan
            [ sh $ "${CABAL} new-freeze -w ${HC} ${TEST} ${BENCH} --dry"
            , sh $ "cat cabal.project.freeze | sed -E 's/^(constraints: *| *)//' | sed 's/any.//'"
            , sh $ "rm  cabal.project.freeze"

            -- install dependencies
            , sh $ "${CABAL} new-build -w ${HC} ${TEST} ${BENCH} --dep -j2 all"
            ]
        tellStrLns
            [ shForJob versions' (cfgNoTestsNoBench config) $ "${CABAL} new-build -w ${HC} --disable-tests --disable-benchmarks --dep -j2 all"
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
            [ sh $ "${CABAL} new-sdist all"
            ]

    let tarFiles = "dist-newstyle" </> "sdist" </> "*.tar.gz"


    foldedTellStrLns FoldUnpack "Unpacking..." folds $ do
        tellStrLns
            [ sh $ "mv " ++ tarFiles ++ " ${DISTDIR}/"
            , sh $ "cd ${DISTDIR} || false" -- fail explicitly, makes SC happier
            , sh $ "find . -maxdepth 1 -name '*.tar.gz' -exec tar -xvf '{}' \\;"
            ]
        generateCabalProject True

    unless (equivVersionRanges noVersion $ cfgNoTestsNoBench config) $ foldedTellStrLns FoldBuild "Building..." folds $ tellStrLns
        [ comment "this builds all libraries and executables (without tests/benchmarks)"
        , shForJob versions' (cfgNoTestsNoBench config) $ "${CABAL} new-build -w ${HC} --disable-tests --disable-benchmarks all"
        ]

    tellStrLns [""]


    foldedTellStrLns FoldBuildEverything
        "Building with tests and benchmarks..." folds $ tellStrLns
        [ comment "build & run tests, build benchmarks"
        , sh "${CABAL} new-build -w ${HC} ${TEST} ${BENCH} all"
        ]

    -- cabal new-test fails if there are no test-suites.
    when hasTests $ foldedTellStrLns FoldTest "Testing..." folds $ tellStrLns
        [ shForJob versions' (C.intersectVersionRanges (cfgTests config) (cfgRunTests config)) $ mconcat
            [ if cfgNoise config
                 then "${CABAL} "
                 else "(set -o pipefail; ${CABAL} -vnormal+nowrap+markoutput "
            , "new-test -w ${HC} ${TEST} ${BENCH} all"
            , if cfgNoise config
                 then ""
                 else " 2>&1 | sed '/^-----BEGIN CABAL OUTPUT-----$/,/^-----END CABAL OUTPUT-----$/d' )"
            ]
        ]

    tellStrLns [""]

    when doctestEnabled $ do
        let doctestOptions = unwords $ cfgDoctestOptions doctestConfig
        tellStrLns [ comment "doctest" ]
        foldedTellStrLns FoldDoctest "Doctest..." folds $ do
            forM_ pkgs $ \Pkg{pkgName,pkgGpd,pkgJobs} -> do
                forM_ (doctestArgs pkgGpd) $ \args -> do
                    let args' = unwords args
                    let vr = cfgDoctestEnabled doctestConfig `intersectVersionRanges` doctestJobVersionRange `intersectVersionRanges` pkgJobs
                    unless (null args) $ tellStrLns
                        [ shForJob versions' vr $
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
            forM_ pkgs $ \Pkg{pkgName,pkgGpd,pkgJobs} -> do
                -- note: similar arguments work so far for doctest and hlint
                forM_ (hlintArgs pkgGpd) $ \args -> do
                    let args' = unwords args
                    unless (null args) $ tellStrLns
                        [ shForJob versions' (hlintJobVersionRange versions (cfgHLintJob hlintConfig) `intersectVersionRanges` pkgJobs) $
                          "(cd " ++ pkgName ++ "-* && hlint" ++ hlintOptions ++ " " ++ args' ++ ")"
                        ]
        tellStrLns [ "" ]

    when (cfgCheck config) $
        foldedTellStrLns FoldCheck "cabal check..." folds $ do
            tellStrLns [ comment "cabal check" ]
            forM_ pkgs $ \Pkg{pkgName,pkgJobs} -> tellStrLns
                [ shForJob versions' pkgJobs $
                  "(cd " ++ pkgName ++ "-* && ${CABAL} check)"
                ]
            tellStrLns [ "" ]

    when (hasLibrary && not (equivVersionRanges noVersion $ cfgHaddock config)) $
        foldedTellStrLns FoldHaddock "Haddock..." folds $ tellStrLns
            [ comment "haddock"
            , shForJob versions' (cfgHaddock config) "${CABAL} new-haddock -w ${HC} ${TEST} ${BENCH} all"
            , ""
            ]

    -- Have to build last, as we remove cabal.project.local
    unless (equivVersionRanges noVersion $ cfgUnconstrainted config) $ foldedTellStrLns FoldBuildInstalled
        "Building without installed constraints for packages in global-db..." folds $ tellStrLns
        [ comment "Build without installed constraints for packages in global-db"
        , shForJob versions' (cfgUnconstrainted config) "rm -f cabal.project.local; ${CABAL} new-build -w ${HC} --disable-tests --disable-benchmarks all"
        , ""
        ]

    -- and now, as we don't have cabal.project.local;
    -- we can test with other constraint sets
    let constraintSets = cfgConstraintSets config
    unless (null constraintSets) $ do
        tellStrLns
            [ comment "Constraint sets"
            , sh "rm -rf cabal.project.local"
            ]
        forM_ constraintSets $ \cs -> do
            let name = csName cs
            tellStrLns [ comment $ "Constraint set " ++ name ]

            let shForCs = shForJob versions' (csGhcVersions cs)
            let testFlag = if csTests cs then "--enable-tests" else "--disable-tests"
            let benchFlag = if csBenchmarks cs then "--enable-benchmarks" else "--disable-benchmarks"
            let constraintFlags = map (\x ->  "--constraint='" ++ x ++ "'") (csConstraints cs)
            let allFlags = unwords (testFlag : benchFlag : constraintFlags)

            foldedTellStrLns' FoldConstraintSets name ("Constraint set " ++ name) folds $ tellStrLns
                [ shForCs $ "${CABAL} v2-build -w ${HC} " ++ allFlags ++ " all"
                , if csRunTests cs
                  then shForCs $ "${CABAL} v2-test -w ${HC} " ++ allFlags ++ " all"
                  else RowSkip
                , if csHaddock cs
                  then shForCs $ "${CABAL} v2-haddock -w ${HC} " ++ allFlags ++ " all"
                  else RowSkip
                ]
        tellStrLns [""]

    tellStrLnsRaw
        [ "# REGENDATA " ++ show argv
        , "# EOF"
        ]

    return ()
  where
    doctestConfig = cfgDoctest config
    doctestEnabled = any (`withinRange` cfgDoctestEnabled doctestConfig) versions'

    hlintConfig   = cfgHLint config


    hasTests   = F.any (\Pkg{pkgGpd} -> not . null $ condTestSuites pkgGpd) pkgs
    hasLibrary = F.any (\Pkg{pkgGpd} -> isJust $ condLibrary pkgGpd) pkgs

    -- GHC versions which need head.hackage
    headGhcVers = S.filter previewGHC versions

    generateCabalProject dist = do
        tellStrLns
            [ sh "rm -f cabal.project"
            , sh "touch cabal.project"
            ]
        F.forM_  pkgs $ \pkg -> do
            let p | dist      = pkgName pkg ++ "-*/*.cabal"
                  | otherwise = pkgDir pkg
            tellStrLns $
                [ shForJob versions' (pkgJobs pkg) $ "printf 'packages: \"" ++ p ++ "\"\\n' >> cabal.project"
                ]

        case cfgCopyFields config of
            CopyFieldsNone -> return ()
            CopyFieldsAll  -> unless (null (prjOrigFields prj)) $ tellStrLns
                [ sh $ "echo '" ++ l ++ "' >> cabal.project"
                | l <- lines $ C.showFields' 2 $ prjOrigFields prj
                , not (null l)
                ]
            CopyFieldsSome -> do
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

                F.forM_ (prjSourceRepos prj) $ \repo -> do
                    let repo' = PP.render $ C.prettyFieldGrammar (C.sourceRepoFieldGrammar $ C.RepoKindUnknown "unused") repo
                    tellStrLns [ sh $ "echo 'source-repository-package' >> cabal.project" ]
                    tellStrLns [ sh $ "echo '  " ++ l ++ "' >> cabal.project" | l <- lines repo' ]


        unless (null (cfgLocalGhcOptions config)) $ forM_ pkgs $ \Pkg{pkgName} -> do
            let s = unwords $ map (show . PU.showToken) $ cfgLocalGhcOptions config
            tellStrLns
                [ sh $ "echo 'package " ++ pkgName ++ "' >> cabal.project"
                , sh $ "echo '  ghc-options: " ++ s ++ "' >> cabal.project"
                ]

        -- raw-project is after local-ghc-options so we can override per package.
        unless (null (cfgRawProject config)) $ tellStrLns
            [ sh $ "echo '" ++ l ++ "' >> cabal.project"
            | l <- lines $ C.showFields' 2 $ cfgRawProject config
            , not (null l)
            ]

        -- mandatory cabal.project setup
        -- this have to be last, so configuration doesn't override it.
        tellStrLns
            [ sh $ "printf 'write-ghc-environment-files: always\\n' >> cabal.project"
            ]

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

    quotedPaths :: (Package -> FilePath) -> [String]
    quotedPaths f = map (f . quote) pkgs
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
-- /Note:/ same argument work for hlint too, but not exactly
--
doctestArgs :: GenericPackageDescription -> [[String]]
doctestArgs gpd = nub $
    [ libraryModuleArgs c
    | c <- flattenPackageDescription gpd ^.. L.library . traverse
    ] ++
    [ libraryModuleArgs c
    | c <- flattenPackageDescription gpd ^.. L.subLibraries . traverse
    ]

libraryModuleArgs :: PD.Library -> [String]
libraryModuleArgs l
    | null dirsOrMods = []
    | otherwise       = exts ++ dirsOrMods
  where
    bi = l ^. L.buildInfo

    dirsOrMods
        | null (PD.hsSourceDirs bi) = map display (PD.exposedModules l)
        | otherwise                 = PD.hsSourceDirs bi

    exts = map (("-X" ++) . display) (PD.defaultExtensions bi)

executableModuleArgs :: PD.Executable -> [String]
executableModuleArgs e
    | null dirsOrMods = []
    | otherwise       = exts ++ dirsOrMods
  where
    bi = e ^. L.buildInfo

    dirsOrMods
        -- note: we don't try to find main_is location, if hsSourceDirs is empty.
        | null (PD.hsSourceDirs bi) = map display (PD.otherModules bi)
        | otherwise                 = PD.hsSourceDirs bi

    exts = map (("-X" ++) . display) (PD.defaultExtensions bi)

-------------------------------------------------------------------------------
-- HLint
-------------------------------------------------------------------------------

hlintJobVersionRange :: Set (Maybe Version) -> HLintJob -> VersionRange
hlintJobVersionRange vs HLintJobLatest = case S.maxView vs of
    Just (Just v, _) -> thisVersion v
    _                -> thisVersion $ mkVersion [8,6,3]
hlintJobVersionRange _ (HLintJob v)   = thisVersion v

hlintArgs :: GenericPackageDescription -> [[String]]
hlintArgs gpd = nub $
    [ libraryModuleArgs c
    | c <- flattenPackageDescription gpd ^.. L.library . traverse
    ] ++
    [ libraryModuleArgs c
    | c <- flattenPackageDescription gpd ^.. L.subLibraries . traverse
    ] ++
    [ executableModuleArgs c
    | c <- flattenPackageDescription gpd ^.. L.executables . traverse
    ]
