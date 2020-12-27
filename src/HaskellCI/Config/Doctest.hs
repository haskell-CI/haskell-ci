{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module HaskellCI.Config.Doctest where

import HaskellCI.Prelude

import Distribution.Version (majorBoundVersion)

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
  deriving (Show, Generic, Binary)

-------------------------------------------------------------------------------
-- Default
-------------------------------------------------------------------------------

defaultDoctestVersion :: VersionRange
defaultDoctestVersion = majorBoundVersion (mkVersion [0,17])

defaultDoctestConfig :: DoctestConfig
defaultDoctestConfig = DoctestConfig
    { cfgDoctestEnabled       = noVersion
    , cfgDoctestOptions       = []
    , cfgDoctestVersion       = defaultDoctestVersion
    , cfgDoctestFilterEnvPkgs = []
    , cfgDoctestFilterSrcPkgs = []
    }

-------------------------------------------------------------------------------
-- Grammar
-------------------------------------------------------------------------------

doctestConfigGrammar
    :: (OptionsGrammar g, Applicative (g DoctestConfig))
    => g DoctestConfig DoctestConfig
doctestConfigGrammar = DoctestConfig
    <$> rangeField         "doctest"                                              (field @"cfgDoctestEnabled") noVersion
        ^^^ help "Enable Doctest job"
    <*> C.monoidalFieldAla "doctest-options" (C.alaList' C.NoCommaFSep C.Token')  (field @"cfgDoctestOptions")
        ^^^ metahelp "OPTS" "Additional Doctest options"
    <*> C.optionalFieldDef "doctest-version"                                      (field @"cfgDoctestVersion") defaultDoctestVersion
        ^^^ metahelp "RANGE" "Doctest version"
    <*> C.monoidalFieldAla "doctest-filter-packages" (C.alaList C.NoCommaFSep)    (field @"cfgDoctestFilterEnvPkgs")
        ^^^ metahelp "PKGS" "Filter packages from .ghc.environment file"
    <*> C.monoidalFieldAla "doctest-skip" (C.alaList C.NoCommaFSep)               (field @"cfgDoctestFilterSrcPkgs")
        ^^^ metahelp "PKGS" "Skip doctests for these packages"
