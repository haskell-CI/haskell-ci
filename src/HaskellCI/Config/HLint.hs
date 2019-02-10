module HaskellCI.Config.HLint where

import Distribution.Version
import GHC.Generics (Generic)

data HLintJob
    = HLintJobLatest    -- ^ run with latest GHC
    | HLintJob Version  -- ^ run with specified GHC version
  deriving Show

data HLintConfig = HLintConfig
    { cfgHLintEnabled :: !Bool
    , cfgHLintJob     :: !HLintJob
    , cfgHLintYaml    :: !(Maybe FilePath)
    , cfgHLintVersion :: !VersionRange
    , cfgHLintOptions :: [String]
    } 
  deriving (Show, Generic)

defaultHLintVersion :: VersionRange
defaultHLintVersion = withinVersion (mkVersion [2,1])
