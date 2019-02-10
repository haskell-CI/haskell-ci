module HaskellCI.Config where

import Distribution.Version
import GHC.Generics (Generic)

import qualified Data.Set as S
import qualified Data.Map as M

import HaskellCI.Config.Doctest
import HaskellCI.Config.HLint

data Fold
    = FoldSDist
    | FoldUnpack
    | FoldBuild
    | FoldBuildInstalled
    | FoldBuildEverything
    | FoldTest
    | FoldHaddock
    | FoldStackage
    | FoldCheck
    | FoldDoctest
    | FoldHLint
    | FoldConstraintSets
  deriving (Eq, Ord, Show, Enum, Bounded)

data ConstraintSet = ConstraintSet
    { csName        :: String
    , csGhcVersions :: VersionRange
    , csConstraints :: [String] -- we parse these simply as strings
    }
  deriving (Show)

emptyConstraintSet :: String -> ConstraintSet
emptyConstraintSet n = ConstraintSet n anyVersion []

-- TODO: split other blocks like DoctestConfig
data Config = Config
    { cfgCabalInstallVersion :: Maybe Version
    , cfgJobs            :: (Maybe Int, Maybe Int)
    , cfgDoctest         :: !DoctestConfig
    , cfgHLint           :: !HLintConfig
    , cfgLocalGhcOptions :: [String]
    , cfgConstraintSets  :: [ConstraintSet]
    , cfgCache           :: !Bool
    , cfgCheck           :: !Bool
    , cfgNoise           :: !Bool
    , cfgNoTestsNoBench  :: !Bool
    , cfgUnconstrainted  :: !Bool
    , cfgInstallDeps     :: !Bool
    , cfgOnlyBranches    :: [String]
    , cfgIrcChannels     :: [String]
    , cfgProjectName     :: Maybe String
    , cfgFolds           :: S.Set Fold
    , cfgGhcHead         :: !Bool
    , cfgEnv             :: M.Map Version String
    , cfgAllowFailures   :: S.Set Version
    , cfgLastInSeries    :: !Bool
    }
  deriving (Show, Generic)

emptyConfig :: Config
emptyConfig = Config
    { cfgCabalInstallVersion = Nothing
    , cfgJobs            = (Nothing, Nothing)
    , cfgDoctest         = DoctestConfig
        { cfgDoctestEnabled = False
        , cfgDoctestOptions = []
        , cfgDoctestVersion = defaultDoctestVersion
        }
    , cfgHLint = HLintConfig
        { cfgHLintEnabled = False
        , cfgHLintJob     = HLintJobLatest
        , cfgHLintYaml    = Nothing
        , cfgHLintVersion = defaultHLintVersion
        , cfgHLintOptions = []
        }
    , cfgLocalGhcOptions = []
    , cfgConstraintSets  = []
    , cfgCache           = True
    , cfgCheck           = True
    , cfgNoise           = True
    , cfgNoTestsNoBench  = True
    , cfgUnconstrainted  = True
    , cfgInstallDeps     = True
    , cfgOnlyBranches    = []
    , cfgIrcChannels     = []
    , cfgProjectName     = Nothing
    , cfgFolds           = S.empty
    , cfgGhcHead         = False
    , cfgEnv             = M.empty
    , cfgAllowFailures   = S.empty
    , cfgLastInSeries    = False
    }

