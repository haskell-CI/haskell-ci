{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module HaskellCI.Config.ConstraintSet where

import HaskellCI.Prelude

import qualified Distribution.FieldGrammar as C

import HaskellCI.Newtypes
import HaskellCI.OptionsGrammar

data ConstraintSet = ConstraintSet
    { csName         :: String
    , csGhcVersions  :: VersionRange
    , csGhcjs        :: Bool
    , csConstraints  :: [String] -- we parse these simply as strings
    , csTests        :: Bool
    , csRunTests     :: Bool
    , csDocspec      :: Bool
    , csBenchmarks   :: Bool
    , csHaddock      :: Bool
    , csPreferOldest :: Bool
    }
  deriving (Show, Generic)

-------------------------------------------------------------------------------
-- Grammar
-------------------------------------------------------------------------------

constraintSetGrammar
    :: ( OptionsGrammar c g, Applicative (g ConstraintSet)
       )
    => String -> g ConstraintSet ConstraintSet
constraintSetGrammar name = ConstraintSet name
    <$> C.optionalFieldDef "ghc"                                           (field @"csGhcVersions") anyVersion
    <*> C.booleanFieldDef  "ghcjs"                                         (field @"csGhcjs") True
    <*> C.monoidalFieldAla "constraints" (C.alaList' C.CommaVCat NoCommas) (field @"csConstraints")
    <*> C.booleanFieldDef  "tests"                                         (field @"csTests") False
    <*> C.booleanFieldDef  "run-tests"                                     (field @"csRunTests") False
    <*> C.booleanFieldDef  "docspec"                                       (field @"csDocspec") False
    <*> C.booleanFieldDef  "benchmarks"                                    (field @"csBenchmarks") False
    <*> C.booleanFieldDef  "haddock"                                       (field @"csHaddock") False
    <*> C.booleanFieldDef  "prefer-oldest"                                 (field @"csPreferOldest") False
