module HaskellCI.Config.Doctest where

import Distribution.Version
import GHC.Generics (Generic)

data DoctestConfig = DoctestConfig
    { cfgDoctestEnabled :: !Bool
    , cfgDoctestOptions :: [String]
    , cfgDoctestVersion :: !VersionRange
    }
  deriving (Show, Generic)

defaultDoctestVersion :: VersionRange
defaultDoctestVersion = withinVersion (mkVersion [0,16])
