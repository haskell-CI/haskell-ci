{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
module HaskellCI.Config.Doctest where

import HaskellCI.Prelude

import Distribution.Version (withinVersion)

import qualified Distribution.FieldGrammar      as C
import qualified Distribution.Parsec.Newtypes   as C
import qualified Distribution.Types.PackageName as C

import HaskellCI.OptionsGrammar

data DoctestConfig = DoctestConfig
    { cfgDoctestEnabled       :: !VersionRange
    , cfgDoctestOptions       :: [String]
    , cfgDoctestVersion       :: !VersionRange
    , cfgDoctestFilterEnvPkgs :: ![C.PackageName]
    , cfgDoctestFilterSrcPkgs :: ![C.PackageName]
    }
  deriving (Show, Generic)

defaultDoctestVersion :: VersionRange
defaultDoctestVersion = withinVersion (mkVersion [0,16,2])

-------------------------------------------------------------------------------
-- Grammar
-------------------------------------------------------------------------------

doctestConfigGrammar
    :: (OptionsGrammar g, Applicative (g DoctestConfig))
    => g DoctestConfig DoctestConfig
doctestConfigGrammar = DoctestConfig
    <$> rangeField         "doctest"                                              #cfgDoctestEnabled noVersion
        ^^^ help "Enable Doctest job"
    <*> C.monoidalFieldAla "doctest-options" (C.alaList' C.NoCommaFSep C.Token')  #cfgDoctestOptions
        ^^^ metahelp "OPTS" "Additional Doctest options"
    <*> C.optionalFieldDef "doctest-version"                                      #cfgDoctestVersion defaultDoctestVersion
        ^^^ metahelp "RANGE" "Doctest version"
    <*> C.monoidalFieldAla "doctest-filter-packages" (C.alaList C.NoCommaFSep)    #cfgDoctestFilterEnvPkgs
        ^^^ metahelp "PKGS" "Filter packages from .ghc.environment file"
    <*> C.monoidalFieldAla "doctest-skip" (C.alaList C.NoCommaFSep)               #cfgDoctestFilterSrcPkgs
        ^^^ metahelp "PKGS" "Skip doctests for these packages"
