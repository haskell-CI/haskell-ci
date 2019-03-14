{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
module HaskellCI.Config.Doctest where

import           Data.Generics.Labels         ()
import           Distribution.Version
import           GHC.Generics                 (Generic)

import qualified Distribution.FieldGrammar    as C
import qualified Distribution.Parsec.Newtypes as C

import HaskellCI.OptionsGrammar

data DoctestConfig = DoctestConfig
    { cfgDoctestEnabled :: !VersionRange
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
    :: (OptionsGrammar g, Applicative (g DoctestConfig))
    => g DoctestConfig DoctestConfig
doctestConfigGrammar = DoctestConfig
    <$> rangeField         "doctest"                                             #cfgDoctestEnabled noVersion
        ^^^ help "Enable Doctest job"
    <*> C.monoidalFieldAla "doctest-options" (C.alaList' C.NoCommaFSep C.Token') #cfgDoctestOptions
        ^^^ metahelp "OPTS" "Additional Doctest options"
    <*> C.optionalFieldDef "doctest-version"                                     #cfgDoctestVersion defaultDoctestVersion
        ^^^ metahelp "RANGE" "Doctest version"
