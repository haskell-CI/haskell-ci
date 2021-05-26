{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module HaskellCI.Bash (
    makeBash
) where

import HaskellCI.Prelude

import qualified Data.Set                        as S
import qualified Distribution.Fields.Pretty      as C
import qualified Distribution.Package            as C
import qualified Distribution.Types.VersionRange as C
import qualified Distribution.Version            as C

import Cabal.Project
import HaskellCI.Auxiliary
import HaskellCI.Bash.Template
import HaskellCI.Compiler
import HaskellCI.Config
import HaskellCI.Config.ConstraintSet
import HaskellCI.Config.Doctest
import HaskellCI.Config.Installed
import HaskellCI.Config.PackageScope
import HaskellCI.Config.Validity
import HaskellCI.Jobs
import HaskellCI.List
import HaskellCI.Package
import HaskellCI.Sh
import HaskellCI.ShVersionRange
import HaskellCI.Tools

{-
Bash-specific notes:

* We use -j for parallelism, as the exact number of cores depends on the
  particular machine the script is being run on.
-}

makeBash
    :: [String]
    -> Config
    -> Project URI Void Package
    -> JobVersions
    -> Either HsCiError Z -- TODO: writer
makeBash _argv config@Config {..} prj jobs@JobVersions {..} = do
    -- Validity checks
    checkConfigValidity config jobs

    blocks <- traverse (fmap shsToList) $ buildList $ do
        -- install doctest
        when doctestEnabled $ step "install doctest" $ do
            let range = Range (cfgDoctestEnabled cfgDoctest) /\ doctestJobVersionRange
            comment "install doctest"
            run_cmd_if range "$CABAL v2-install $ARG_COMPILER --ignore-project -j doctest --constraint='doctest ^>=0.17'"
            run_cmd_if range "doctest --version"

        -- install hlint
        -- TODO

        -- autoreconf ...
        -- hmm, source is read-only...

        step "initial cabal.project for sdist" $ do
            change_dir "$BUILDDIR"
            run_cmd "touch cabal.project"
            for_ pkgs $ \pkg ->
                echo_if_to (RangePoints $ pkgJobs pkg) "cabal.project" $ "packages: $SRCDIR/" ++ pkgDir pkg
            run_cmd "cat cabal.project"

        -- sdist
        step "sdist" $ do
            run_cmd "mkdir -p \"$BUILDDIR/sdist\""
            -- TODO: check if cabal-install-3.4 can be run on read only system
            run_cmd "$CABAL sdist all --output-dir \"$BUILDDIR/sdist\""

        -- find and unpack sdist
        step "unpack" $ do
            change_dir "$BUILDDIR"
            run_cmd "mkdir -p \"$BUILDDIR/unpacked\""
            run_cmd "find \"$BUILDDIR/sdist\" -maxdepth 1 -type f -name '*.tar.gz' -exec tar -C \"$BUILDDIR/unpacked\" -xzvf {} \\;"

        -- write cabal.project
        step "generate cabal.project" $ do
            for_ pkgs $ \Pkg{pkgName} -> do
                sh $ pkgNameDirVariable' pkgName ++ "=\"$(find \"$BUILDDIR/unpacked\" -maxdepth 1 -type d -regex '.*/" ++ pkgName ++ "-[0-9.]*')\""

            run_cmd "touch cabal.project"
            run_cmd "touch cabal.project.local"

            for_ pkgs $ \pkg ->
                echo_if_to (RangePoints $ pkgJobs pkg) "cabal.project" $ "packages: " ++ pkgNameDirVariable (pkgName pkg)

            -- per package options
            case cfgErrorMissingMethods of
                PackageScopeNone  -> pure ()
                PackageScopeLocal -> for_ pkgs $ \Pkg{pkgName,pkgJobs} -> do
                    let range = Range (C.orLaterVersion (C.mkVersion [8,2])) /\ RangePoints pkgJobs
                    echo_if_to range "cabal.project" $ "package " ++ pkgName
                    echo_if_to range "cabal.project" $ "    ghc-options: -Werror=missing-methods"
                PackageScopeAll   -> cat "cabal.project" $ unlines
                    [ "package *"
                    , "  ghc-options: -Werror=missing-methods"
                    ]

            -- extra cabal.project fields
            cat "cabal.project" $ C.showFields' (const []) (const id) 2 extraCabalProjectFields

            -- also write cabal.project.local file with
            -- @
            -- constraints: base installed
            -- constraints: array installed
            -- ...
            --
            -- omitting any local package names
            case normaliseInstalled cfgInstalled of
                InstalledDiff pns -> sh $ unwords
                    [ "$HCPKG list --simple-output --names-only"
                    , "| perl -ne 'for (split /\\s+/) { print \"constraints: $_ installed\\n\" unless /" ++ re ++ "/; }'"
                    , ">> cabal.project.local"
                    ]
                  where
                    pns' = S.map C.unPackageName pns `S.union` foldMap (S.singleton . pkgName) pkgs
                    re = "^(" ++ intercalate "|" (S.toList pns') ++ ")$"

                InstalledOnly pns | not (null pns') -> cat "cabal.project.local" $ unlines
                    [ "constraints: " ++ pkg ++ " installed"
                    | pkg <- S.toList pns'
                    ]
                  where
                    pns' = S.map C.unPackageName pns `S.difference` foldMap (S.singleton . pkgName) pkgs

                -- otherwise: nothing
                _ -> pure ()

            run_cmd "cat cabal.project"
            run_cmd "cat cabal.project.local"

        -- dump install plan
        step "dump install plan" $ do
            run_cmd "$CABAL v2-build $ARG_COMPILER $ARG_TESTS $ARG_BENCH --dry-run all"
            run_cmd "cabal-plan"

        -- install dependencies
        when cfgInstallDeps $ step "install dependencies" $ do
            run_cmd "$CABAL v2-build $ARG_COMPILER --disable-tests --disable-benchmarks --dependencies-only -j all"
            run_cmd "$CABAL v2-build $ARG_COMPILER $ARG_TESTS $ARG_BENCH --dependencies-only -j all"

        unless (equivVersionRanges C.noVersion cfgNoTestsNoBench) $ step "build w/o tests" $ do
            run_cmd "$CABAL v2-build $ARG_COMPILER --disable-tests --disable-benchmarks all"

        -- build
        step "build" $ do
            run_cmd "$CABAL v2-build $ARG_COMPILER $ARG_TESTS $ARG_BENCH all"

        -- tests
        step "tests" $ do
            let range = RangeGHC /\ Range (cfgTests /\ cfgRunTests) /\ hasTests
            run_cmd_if range $ "$CABAL v2-test $ARG_COMPILER $ARG_TESTS $ARG_BENCH all" ++ testShowDetails

        -- doctest
        when doctestEnabled $ step "doctest" $ do
            let doctestOptions = unwords $ cfgDoctestOptions cfgDoctest

            unless (null $ cfgDoctestFilterEnvPkgs cfgDoctest) $ do
                -- cabal-install mangles unit ids on the OSX,
                -- removing the vowels to make filepaths shorter
                let manglePkgNames :: String -> [String]
                    manglePkgNames n
                        | null macosVersions = [n]
                        | otherwise          = [n, filter notVowel n]
                      where
                        notVowel c = notElem c ("aeiou" :: String)
                let filterPkgs = intercalate "|" $ concatMap (manglePkgNames . C.unPackageName) $ cfgDoctestFilterEnvPkgs cfgDoctest
                run_cmd $ "perl -i -e 'while (<ARGV>) { print unless /package-id\\s+(" ++ filterPkgs ++ ")-\\d+(\\.\\d+)*/; }' .ghc.environment.*"

            for_ pkgs $ \Pkg{pkgName,pkgGpd,pkgJobs} ->
                when (C.mkPackageName pkgName `notElem` cfgDoctestFilterSrcPkgs cfgDoctest) $ do
                    for_ (doctestArgs pkgGpd) $ \args -> do
                        let args' = unwords args
                        let vr = Range (cfgDoctestEnabled cfgDoctest)
                              /\ doctestJobVersionRange
                              /\ RangePoints pkgJobs

                        unless (null args) $ do
                            change_dir_if vr $ pkgNameDirVariable pkgName
                            run_cmd_if vr $ "doctest " ++ doctestOptions ++ " " ++ args'

        -- hlint
        -- TODO

        -- cabal check
        when cfgCheck $ step "cabal check" $ do
            for_ pkgs $ \Pkg{pkgName,pkgJobs} -> do
                let range = RangePoints pkgJobs
                change_dir_if range $ pkgNameDirVariable pkgName
                run_cmd_if range "${CABAL} -vnormal check"
            change_dir "$BUILDDIR"

        -- haddock
        when (hasLibrary && not (equivVersionRanges C.noVersion cfgHaddock)) $ step "haddock" $ do
            let range = RangeGHC /\ Range cfgHaddock
            run_cmd_if range "$CABAL v2-haddock $ARG_COMPILER --with-haddock $HADDOCK $ARG_TESTS $ARG_BENCH all"

        -- unconstrained build
        unless (equivVersionRanges C.noVersion cfgUnconstrainted) $ step "unconstrained build" $ do
            let range = Range cfgUnconstrainted
            run_cmd_if range "rm -f cabal.project.local"
            run_cmd_if range "$CABAL v2-build $ARG_COMPILER --disable-tests --disable-benchmarks all"

        -- constraint sets
        unless (null cfgConstraintSets) $ step "constraint sets" $ do
            run_cmd "rm -f cabal.project.local"

            for_ cfgConstraintSets $ \cs -> do
                let name            = csName cs
                let run_cmd_cs      = run_cmd_if (Range (csGhcVersions cs))
                let run_cmd_cs' r   = run_cmd_if (Range (csGhcVersions cs) /\ r)
                let testFlag        = if csTests cs then "--enable-tests" else "--disable-tests"
                let benchFlag       = if csBenchmarks cs then "--enable-benchmarks" else "--disable-benchmarks"
                let constraintFlags = map (\x ->  "--constraint='" ++ x ++ "'") (csConstraints cs)
                let allFlags        = unwords (testFlag : benchFlag : constraintFlags)

                put_info $ "constraint set " ++ name
                run_cmd_cs $ "$CABAL v2-build $ARG_COMPILER " ++ allFlags ++ " all"
                when (csRunTests cs) $
                    run_cmd_cs' hasTests $ "$CABAL v2-test $ARG_COMPILER " ++ allFlags ++ " all"
                when (hasLibrary && csHaddock cs) $
                    run_cmd_cs $ "$CABAL v2-haddock $ARG_COMPILER " ++ withHaddock ++ " " ++ allFlags ++ " all"

    return defaultZ
        { zJobs =
            [ prettyShow v
            | GHC v <- reverse $ toList linuxVersions
            ]
        , zBlocks = blocks
        , zApt = S.toList cfgApt
        , zTestsCond = compilerVersionArithPredicate linuxVersions $ Range cfgTests
        , zBenchCond = compilerVersionArithPredicate linuxVersions $ Range cfgBenchmarks
        }
  where
    Auxiliary {..} = auxiliary config prj jobs

    -- TODO: this can be smart and do nothing is there are only comments in [Sh]
    step :: String -> ShM () -> ListBuilder (Either HsCiError [Sh]) ()
    step name action = item $ runSh $ do
        comment name
        put_info name
        action

    put_info :: String -> ShM ()
    put_info s = sh $ "put_info " ++ show s

    change_dir :: String -> ShM ()
    change_dir dir = sh $ "change_dir " ++ show dir

    change_dir_if :: CompilerRange -> String -> ShM ()
    change_dir_if vr dir
        | all (`compilerWithinRange` vr) linuxVersions       = change_dir dir
        | not $ any (`compilerWithinRange` vr) linuxVersions = pure ()
        | otherwise = sh $ unwords
            [ "change_dir_if"
            , compilerVersionArithPredicate linuxVersions vr
            , dir
            ]

    run_cmd :: String -> ShM ()
    run_cmd cmd = sh $ unwords
        [ "run_cmd"
        , cmd
        ]

    run_cmd_if :: CompilerRange -> String -> ShM ()
    run_cmd_if vr cmd
        | all (`compilerWithinRange` vr) linuxVersions       = run_cmd cmd
        | not $ any (`compilerWithinRange` vr) linuxVersions = pure ()
        | otherwise = sh $ unwords
            [ "run_cmd_if"
            , compilerVersionArithPredicate linuxVersions vr
            , cmd
            ]

    echo_if_to :: CompilerRange -> FilePath -> String -> ShM ()
    echo_if_to vr path contents
        | all (`compilerWithinRange` vr) linuxVersions = sh $ unwords
            [ "echo_to"
            , path
            , show contents
            ]
        | not $ any (`compilerWithinRange` vr) linuxVersions = pure ()
        | otherwise = sh $ unwords
            [ "echo_if_to"
            , compilerVersionArithPredicate linuxVersions vr
            , path
            , show contents
            ]

    -- Needed to work around haskell/cabal#6214
    withHaddock :: String
    withHaddock = "--with-haddock $HADDOCK"

shsToList :: [Sh] -> String
shsToList = unlines . map f where
    f (Sh x)      = x
    f (Comment c) = "# " ++ c

cat :: FilePath -> String -> ShM ()
cat path contents = sh $ concat
    [ "cat >> " ++ path ++ " <<EOF\n"
    , contents
    , "EOF"
    ]
