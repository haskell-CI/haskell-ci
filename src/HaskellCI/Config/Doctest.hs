{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module HaskellCI.Config.Doctest (
    DoctestConfig (..),
    initialDoctestConfig,
    doctestConfigGrammar,
) where

import HaskellCI.Prelude

import Distribution.Version (majorBoundVersion)

import qualified Distribution.FieldGrammar      as C
import qualified Distribution.Types.PackageName as C

import HaskellCI.GrammarDefault
import HaskellCI.OptionsGrammar

data DoctestConfig = DoctestConfig
    { cfgDoctestEnabled       :: !VersionRange
    , cfgDoctestOptions       :: [String]
    , cfgDoctestVersion       :: !VersionRange
    , cfgDoctestFilterEnvPkgs :: ![C.PackageName]
    , cfgDoctestFilterSrcPkgs :: ![C.PackageName]
    }
  deriving (Show, Generic, Binary)

-------------------------------------------------------------------------------
-- Default
-------------------------------------------------------------------------------

initialDoctestConfig :: DoctestConfig
initialDoctestConfig = DoctestConfig
    { cfgDoctestEnabled       = noVersion
    , cfgDoctestOptions       = []
    , cfgDoctestVersion       = majorBoundVersion (mkVersion [0,22,0])
    , cfgDoctestFilterEnvPkgs = []
    , cfgDoctestFilterSrcPkgs = []
    }

-------------------------------------------------------------------------------
-- Grammar
-------------------------------------------------------------------------------

doctestConfigGrammar :: OptionsGrammar c g => g DoctestConfig DoctestConfig
doctestConfigGrammar = DoctestConfig
    <$> rangeField         "doctest"                                            (field @"cfgDoctestEnabled") initialDoctestConfig
        ^^^ help "Enable Doctest job"
    <*> monoidalFieldAla "doctest-options" (C.alaList' C.NoCommaFSep C.Token')  (field @"cfgDoctestOptions")
        ^^^ metahelp "OPTS" "Additional Doctest options"
    <*> optionalFieldDef "doctest-version"                                      (field @"cfgDoctestVersion") initialDoctestConfig
        ^^^ metahelp "RANGE" "Doctest version"
    <*> monoidalFieldAla "doctest-filter-packages" (C.alaList C.NoCommaFSep)    (field @"cfgDoctestFilterEnvPkgs")
        ^^^ metahelp "PKGS" "Filter packages from .ghc.environment file"
    <*> monoidalFieldAla "doctest-skip" (C.alaList C.NoCommaFSep)               (field @"cfgDoctestFilterSrcPkgs")
        ^^^ metahelp "PKGS" "Skip doctests for these packages"
