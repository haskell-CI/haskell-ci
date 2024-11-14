{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
module HaskellCI.Config.Initial where

import HaskellCI.Prelude

import qualified Distribution.Version as C

import HaskellCI.Config.Components
import HaskellCI.Config.CopyFields
import HaskellCI.Config.Docspec
import HaskellCI.Config.Doctest
import HaskellCI.Config.PackageScope
import HaskellCI.Config.Type
import HaskellCI.Config.Ubuntu
import HaskellCI.Ghcup
import HaskellCI.HeadHackage
import HaskellCI.SetupMethod
import HaskellCI.TestedWith

-- | This is an "initial" configuration. It's meant to stay immutable.
-- All changes to defaults should be done in History.
initialConfig :: Config
initialConfig = Config
    { cfgCabalInstallVersion = Just (C.mkVersion [3,10,2,0])
    , cfgJobs                = Nothing
    , cfgUbuntu              = Bionic
    , cfgTestedWith          = TestedWithUniform
    , cfgEnabledJobs         = anyVersion
    , cfgCopyFields          = CopyFieldsSome
    , cfgLocalGhcOptions     = []
    , cfgSubmodules          = False
    , cfgCache               = True
    , cfgInstallDeps         = True
    , cfgInstalled           = []
    , cfgTests               = anyVersion
    , cfgRunTests            = anyVersion
    , cfgBenchmarks          = anyVersion
    , cfgHaddock             = anyVersion
    , cfgHaddockComponents   = ComponentsAll
    , cfgNoTestsNoBench      = anyVersion
    , cfgUnconstrainted      = anyVersion
    , cfgHeadHackage         = defaultHeadHackage
    , cfgHeadHackageOverride = True
    , cfgGhcjsTests          = False
    , cfgGhcjsTools          = []
    , cfgTestOutputDirect    = True
    , cfgCheck               = True
    , cfgOnlyBranches        = []
    , cfgIrcChannels         = []
    , cfgIrcNickname         = Nothing
    , cfgIrcPassword         = Nothing
    , cfgIrcIfInOriginRepo   = False
    , cfgEmailNotifications  = True
    , cfgProjectName         = Nothing
    , cfgGhcHead             = False
    , cfgPostgres            = False
    , cfgGoogleChrome        = False
    , cfgEnv                 = mempty
    , cfgAllowFailures       = noVersion
    , cfgLastInSeries        = False
    , cfgLinuxJobs           = anyVersion
    , cfgMacosJobs           = noVersion
    , cfgGhcupCabal          = True
    , cfgSetupMethods = PerSetupMethod
        { hvrPpa          = noVersion
        , ghcup           = C.unionVersionRanges (C.intersectVersionRanges (C.laterVersion (mkVersion [8,10,4])) (C.earlierVersion (mkVersion [9]))) (C.laterVersion (mkVersion [9,0,1]))
        , ghcupVanilla    = noVersion -- TODO -- include GHC-9.8.3
        , ghcupPrerelease = C.orLaterVersion (mkVersion ([9,11,0]))
        }
    , cfgGhcupVersion        = initialGhcupVersion
    , cfgApt                 = mempty
    , cfgTravisPatches       = []
    , cfgGitHubPatches       = []
    , cfgInsertVersion       = True
    , cfgErrorMissingMethods = PackageScopeLocal
    , cfgDoctest             = initialDoctestConfig
    , cfgDocspec             = initialDocspecConfig
    , cfgConstraintSets      = []
    , cfgRawProject          = []
    , cfgRawTravis           = ""
    , cfgGitHubActionName    = Nothing
    , cfgTimeoutMinutes      = 60
    }
