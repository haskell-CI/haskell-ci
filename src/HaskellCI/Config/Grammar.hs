{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
module HaskellCI.Config.Grammar where

import HaskellCI.Prelude

import qualified Data.Map                        as M
import qualified Distribution.Compat.CharParsing as C
import qualified Distribution.Compat.Newtype     as C
import qualified Distribution.FieldGrammar       as C
import qualified Distribution.Parsec             as C
import qualified Distribution.Pretty             as C
import qualified Text.PrettyPrint                as PP

import HaskellCI.Config.Components
import HaskellCI.Config.CopyFields
import HaskellCI.Config.Docspec
import HaskellCI.Config.Doctest
import HaskellCI.Config.History
import HaskellCI.Config.Installed
import HaskellCI.Config.Jobs
import HaskellCI.Config.PackageScope
import HaskellCI.Config.Type
import HaskellCI.Config.Ubuntu
import HaskellCI.GrammarDefault
import HaskellCI.Newtypes
import HaskellCI.OptionsGrammar
import HaskellCI.TestedWith

-------------------------------------------------------------------------------
-- Grammar
-------------------------------------------------------------------------------

configGrammar
    :: ( OptionsGrammar c g
       , c Components
       , c CopyFields
       , c CopyFields
       , c Env
       , c HeadVersion
       , c Jobs
       , c Natural
       , c PackageScope
       , c TestedWithJobs
       , c Ubuntu
       , c Version
       , c (C.List C.FSep (Identity Installed) Installed)
       , Applicative (g DoctestConfig)
       , Applicative (g DocspecConfig)
       )
    => g Config Config
configGrammar = Config
    <$> optionalFieldDefAla "cabal-install-version"     HeadVersion                         (field @"cfgCabalInstallVersion") defaultConfig
        ^^^ metahelp "VERSION" "cabal-install version for all jobs"
    <*> optionalField       "jobs"                                                          (field @"cfgJobs")
        ^^^ metahelp "JOBS" "jobs (N:M - cabal:ghc)"
    <*> optionalFieldDef    "distribution"                                                  (field @"cfgUbuntu") defaultConfig
        ^^^ metahelp "DIST" (concat
              [ "distribution version ("
              , intercalate ", " $ map showUbuntu [minBound..maxBound]
              , ")"
              ])
    <*> optionalFieldDef    "jobs-selection"                                                (field @"cfgTestedWith") defaultConfig
        ^^^ metahelp "uniform|any" "Jobs selection across packages"
    <*> rangeField            "enabled"                                                     (field @"cfgEnabledJobs") defaultConfig
        ^^^ metahelp "RANGE" "Restrict jobs selection futher from per package tested-with"
    <*> optionalFieldDef    "copy-fields"                                                   (field @"cfgCopyFields") defaultConfig
        ^^^ metahelp "none|some|all" "Copy ? fields from cabal.project fields"
    <*> monoidalFieldAla    "local-ghc-options"         (C.alaList' C.NoCommaFSep C.Token') (field @"cfgLocalGhcOptions")
        ^^^ metahelp "OPTS" "--ghc-options for local packages"
    <*> booleanFieldDef     "submodules"                                                    (field @"cfgSubmodules") defaultConfig
        ^^^ help "Clone submodules, i.e. recursively"
    <*> booleanFieldDef     "cache"                                                         (field @"cfgCache") defaultConfig
        ^^^ help "Disable caching"
    <*> booleanFieldDef     "install-dependencies"                                          (field @"cfgInstallDeps") defaultConfig
        ^^^ help "Skip separate dependency installation step"
    <*> monoidalFieldAla    "installed"                 (C.alaList C.FSep)                  (field @"cfgInstalled")
        ^^^ metahelp "+/-PKG" "Specify 'constraint: ... installed' packages"
    <*> rangeField            "tests"                                                       (field @"cfgTests") defaultConfig
        ^^^ metahelp "RANGE" "Build tests with"
    <*> rangeField            "run-tests"                                                   (field @"cfgRunTests") defaultConfig
        ^^^ metahelp "RANGE" "Run tests with (note: only built tests are run)"
    <*> rangeField           "benchmarks"                                                   (field @"cfgBenchmarks") defaultConfig
        ^^^ metahelp "RANGE" "Build benchmarks"
    <*> rangeField           "haddock"                                                      (field @"cfgHaddock") defaultConfig
        ^^^ metahelp "RANGE" "Haddock step"
    <*> optionalFieldDef   "haddock-components"                                             (field @"cfgHaddockComponents") defaultConfig
        ^^^ metahelp "all|libs" "Haddock components"
    <*> rangeField           "no-tests-no-benchmarks"                                       (field @"cfgNoTestsNoBench") defaultConfig
        ^^^ metahelp "RANGE" "Build without tests and benchmarks"
    <*> rangeField            "unconstrained"                                               (field @"cfgUnconstrainted") defaultConfig
        ^^^ metahelp "RANGE" "Make unconstrained build"
    <*> rangeField            "head-hackage"                                                (field @"cfgHeadHackage") defaultConfig
        ^^^ metahelp "RANGE" "Use head.hackage repository. Also marks as allow-failures"
    <*> booleanFieldDef     "head-hackage-override"                                         (field @"cfgHeadHackageOverride") defaultConfig
        ^^^ help "Use :override for head.hackage repository"
    <*> booleanFieldDef     "ghcjs-tests"                                                   (field @"cfgGhcjsTests") defaultConfig
        ^^^ help "Run tests with GHCJS (experimental, relies on cabal-plan finding test-suites)"
    <*> monoidalFieldAla    "ghcjs-tools"               (C.alaList C.FSep)                  (field @"cfgGhcjsTools")
--        ^^^ metahelp "TOOL" "Additional host tools to install with GHCJS"
    <*> booleanFieldDef "test-output-direct"                                                (field @"cfgTestOutputDirect") defaultConfig
        ^^^ help "Use --test-show-details=direct, may cause problems with build-type: Custom"
    <*> booleanFieldDef "cabal-check"                                                       (field @"cfgCheck") defaultConfig
        ^^^ help "Disable cabal check run"
    <*> monoidalFieldAla    "branches"                  (C.alaList' C.FSep C.Token')        (field @"cfgOnlyBranches")
        ^^^ metahelp "BRANCH" "Enable builds only for specific branches"
    <*> monoidalFieldAla    "irc-channels"              (C.alaList' C.FSep C.Token')        (field @"cfgIrcChannels")
        ^^^ metahelp "IRC" "Enable IRC notifications to given channel (e.g. 'irc.libera.chat#haskell-lens')"
    <*> freeTextField       "irc-nickname"                                                  (field @"cfgIrcNickname")
        ^^^ metahelp "NICKNAME" "Nickname with which to authenticate to an IRC server. Only used if `irc-channels` are set."
    <*> freeTextField       "irc-password"                                                  (field @"cfgIrcPassword")
        ^^^ metahelp "PASSWORD" "Password with which to authenticate to an IRC server. Only used if `irc-channels` are set."
    <*> booleanFieldDef     "irc-if-in-origin-repo"                                         (field @"cfgIrcIfInOriginRepo") defaultConfig
        ^^^ help "Only send IRC notifications if run from the original remote (GitHub Actions only)"
    <*> booleanFieldDef "email-notifications"                                               (field @"cfgEmailNotifications") defaultConfig
        ^^^ help "Disable email notifications"
    <*> optionalFieldAla    "project-name"              C.Token'                            (field @"cfgProjectName")
        ^^^ metahelp "NAME" "Project name (used for IRC notifications), defaults to package name or name of first package listed in cabal.project file"
    <*> booleanFieldDef     "ghc-head"                                                      (field @"cfgGhcHead") defaultConfig
        ^^^ help "Add ghc-head job"
    <*> booleanFieldDef     "postgresql"                                                    (field @"cfgPostgres") defaultConfig
        ^^^ help "Add postgresql service"
    <*> booleanFieldDef     "google-chrome"                                                 (field @"cfgGoogleChrome") defaultConfig
        ^^^ help "Add google-chrome service"
    <*> monoidalFieldAla    "env"                       Env                                 (field @"cfgEnv")
        ^^^ metahelp "ENV" "Environment variables per job (e.g. `8.0.2:HADDOCK=false`)"
    <*> optionalFieldDefAla "allow-failures"            Range                               (field @"cfgAllowFailures") defaultConfig
        ^^^ metahelp "JOB" "Allow failures of particular GHC version"
    <*> booleanFieldDef     "last-in-series"                                                (field @"cfgLastInSeries") defaultConfig
        ^^^ help "[Discouraged] Assume there are only GHCs last in major series: 8.2.* will match only 8.2.2"
    <*> rangeField            "linux-jobs"                                                  (field @"cfgLinuxJobs") defaultConfig
        ^^^ metahelp "RANGE" "Jobs to build on Linux"
    <*> rangeField            "macos-jobs"                                                  (field @"cfgMacosJobs") defaultConfig
        ^^^ metahelp "RANGE" "Jobs to additionally build with OSX"
    <*> booleanFieldDef     "ghcup-cabal"                                                   (field @"cfgGhcupCabal") defaultConfig
        ^^^ help "Use (or don't) ghcup to install cabal"
    <*> rangeField            "hvr-ppa-jobs"                                                (field @"cfgHvrPpaJobs") defaultConfig
        ^^^ metahelp "RANGE" "(Linux) jobs to use hvr-ppa to install ghc"
    <*> rangeField            "ghcup-jobs"                                                  (field @"cfgGhcupJobs") defaultConfig
        ^^^ metahelp "RANGE" "(Linux) jobs to use ghcup to install ghc"
    <*> rangeField            "ghcup-vanilla-jobs"                                          (field @"cfgGhcupVanillaJobs") defaultConfig
        ^^^ metahelp "RANGE" "(Linux) jobs to use ghcup-vanilla to install ghc"
    <*> rangeField            "ghcup-prerelease-jobs"                                       (field @"cfgGhcupPrereleaseJobs") defaultConfig
        ^^^ metahelp "RANGE" "(Linux) jobs to use ghcup-prerelease to install ghc"
    <*> optionalFieldDef    "ghcup-version"                                                 (field @"cfgGhcupVersion") defaultConfig 
        ^^^ metahelp "VERSION" "ghcup version"
    <*> monoidalFieldAla    "apt"                       (alaSet' C.NoCommaFSep C.Token')    (field @"cfgApt")
        ^^^ metahelp "PKG" "Additional apt packages to install"
    <*> monoidalFieldAla    "travis-patches"            (C.alaList' C.NoCommaFSep C.Token') (field @"cfgTravisPatches")
        ^^^ metaActionHelp "PATCH" "file" ".patch files to apply to the generated Travis YAML file"
    <*> monoidalFieldAla    "github-patches"            (C.alaList' C.NoCommaFSep C.Token') (field @"cfgGitHubPatches")
        ^^^ metaActionHelp "PATCH" "file" ".patch files to apply to the generated GitHub Actions YAML file"
    <*> booleanFieldDef "insert-version"                                                    (field @"cfgInsertVersion") defaultConfig
        ^^^ help "Don't insert the haskell-ci version into the generated Travis YAML file"
    <*> optionalFieldDef "error-missing-methods"                                            (field @"cfgErrorMissingMethods") defaultConfig
        ^^^ metahelp "PKGSCOPE" "Insert -Werror=missing-methods for package scope (none, local, all)"
    <*> blurFieldGrammar (field @"cfgDoctest") doctestConfigGrammar defaultConfig
    <*> blurFieldGrammar (field @"cfgDocspec") docspecConfigGrammar defaultConfig
    <*> pure [] -- constraint sets
    <*> pure [] -- raw project fields
    <*> freeTextFieldDef "raw-travis"                                                       (field @"cfgRawTravis")
        ^^^ help "Raw travis commands which will be run at the very end of the script"
    <*> freeTextField "github-action-name"                                                  (field @"cfgGitHubActionName")
        ^^^ help "The name of GitHub Action"
    <*> optionalFieldDef    "timeout-minutes"                                              (field @"cfgTimeoutMinutes") defaultConfig
        ^^^ metahelp "MINUTES" "The maximum number of minutes to let a job run"

-------------------------------------------------------------------------------
-- Env
-------------------------------------------------------------------------------

newtype Env = Env (M.Map Version String)
  deriving anyclass (C.Newtype (M.Map Version String))

instance C.Parsec Env where
    parsec = Env . M.fromList <$> C.parsecLeadingCommaList p where
        p = do
            v <- C.parsec
            _ <- C.char ':'
            s <- C.munch1 $ \c -> c /= ','
            return (v, s)

instance C.Pretty Env where
    pretty (Env m) = PP.fsep . PP.punctuate PP.comma . map p . M.toList $ m where
        p (v, s) = C.pretty v PP.<> PP.colon PP.<> PP.text s
