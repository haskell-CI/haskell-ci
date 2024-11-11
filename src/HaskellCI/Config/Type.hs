{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module HaskellCI.Config.Type where

import HaskellCI.Prelude

import qualified Data.Map                       as M
import qualified Data.Set                       as S
import qualified Distribution.Fields            as C
import qualified Distribution.Types.PackageName as C

import HaskellCI.Config.Components
import HaskellCI.Config.ConstraintSet
import HaskellCI.Config.CopyFields
import HaskellCI.Config.Docspec
import HaskellCI.Config.Doctest
import HaskellCI.Config.Installed
import HaskellCI.Config.Jobs
import HaskellCI.Config.PackageScope
import HaskellCI.Config.Ubuntu
import HaskellCI.TestedWith

-------------------------------------------------------------------------------
-- Config
-------------------------------------------------------------------------------

-- TODO: split other blocks like DoctestConfig
data Config = Config
    { cfgCabalInstallVersion :: Maybe Version
    , cfgJobs                :: Maybe Jobs
    , cfgUbuntu              :: !Ubuntu
    , cfgTestedWith          :: !TestedWithJobs
    , cfgEnabledJobs         :: !VersionRange
    , cfgCopyFields          :: !CopyFields
    , cfgLocalGhcOptions     :: [String]
    , cfgSubmodules          :: !Bool
    , cfgCache               :: !Bool
    , cfgInstallDeps         :: !Bool
    , cfgInstalled           :: [Installed]
    , cfgTests               :: !VersionRange
    , cfgRunTests            :: !VersionRange
    , cfgBenchmarks          :: !VersionRange
    , cfgHaddock             :: !VersionRange
    , cfgHaddockComponents   :: !Components
    , cfgWeeder              :: !VersionRange
    , cfgNoTestsNoBench      :: !VersionRange
    , cfgUnconstrainted      :: !VersionRange
    , cfgHeadHackage         :: !VersionRange
    , cfgHeadHackageOverride :: !Bool
    , cfgGhcjsTests          :: !Bool
    , cfgGhcjsTools          :: ![C.PackageName]
    , cfgTestOutputDirect    :: !Bool
    , cfgCheck               :: !Bool
    , cfgOnlyBranches        :: [String]
    , cfgIrcChannels         :: [String]
    , cfgIrcNickname         :: Maybe String
    , cfgIrcPassword         :: Maybe String
    , cfgIrcIfInOriginRepo   :: Bool
    , cfgEmailNotifications  :: Bool
    , cfgProjectName         :: Maybe String
    , cfgGhcHead             :: !Bool
    , cfgPostgres            :: !Bool
    , cfgGoogleChrome        :: !Bool
    , cfgEnv                 :: M.Map Version String
    , cfgAllowFailures       :: !VersionRange
    , cfgLastInSeries        :: !Bool
    , cfgLinuxJobs           :: !VersionRange
    , cfgMacosJobs           :: !VersionRange
    , cfgGhcupCabal          :: !Bool
    , cfgGhcupJobs           :: !VersionRange
    , cfgGhcupVersion        :: !Version
    , cfgApt                 :: S.Set String
    , cfgTravisPatches       :: [FilePath]
    , cfgGitHubPatches       :: [FilePath]
    , cfgInsertVersion       :: !Bool
    , cfgErrorMissingMethods :: !PackageScope
    , cfgDoctest             :: !DoctestConfig
    , cfgDocspec             :: !DocspecConfig
    , cfgConstraintSets      :: [ConstraintSet]
    , cfgRawProject          :: [C.PrettyField ()]
    , cfgRawTravis           :: !String
    , cfgGitHubActionName    :: !(Maybe String)
    , cfgTimeoutMinutes      :: !Natural
    }
  deriving (Show, Generic)

deriving instance Show (C.PrettyField ())
