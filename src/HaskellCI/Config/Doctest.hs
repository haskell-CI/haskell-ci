{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
module HaskellCI.Config.Doctest where

import           Data.Generics.Labels         ()
import           Distribution.Version
import           GHC.Generics                 (Generic)

import qualified Distribution.FieldGrammar    as C
import qualified Distribution.Parsec.Newtypes as C

data DoctestConfig = DoctestConfig
    { cfgDoctestEnabled :: !Bool
    , cfgDoctestOptions :: [String]
    , cfgDoctestVersion :: !VersionRange
    }
  deriving (Show, Generic)

defaultDoctestVersion :: VersionRange
defaultDoctestVersion = withinVersion (mkVersion [0,16])

-------------------------------------------------------------------------------
-- Grammar
-------------------------------------------------------------------------------

doctestConfigGrammar
    :: (C.FieldGrammar g, Applicative (g DoctestConfig))
    => g DoctestConfig DoctestConfig
doctestConfigGrammar = DoctestConfig
    <$> C.booleanFieldDef  "doctest"                                             #cfgDoctestEnabled False
    <*> C.monoidalFieldAla "doctest-options" (C.alaList' C.NoCommaFSep C.Token') #cfgDoctestOptions
    <*> C.optionalFieldDef "doctest-version"                                     #cfgDoctestVersion defaultDoctestVersion
