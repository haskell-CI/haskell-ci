{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
module HaskellCI.Config.ConstraintSet where

import HaskellCI.Prelude

import qualified Distribution.FieldGrammar    as C
import qualified Distribution.Parsec.Newtypes as C

import HaskellCI.Newtypes

data ConstraintSet = ConstraintSet
    { csName        :: String
    , csGhcVersions :: VersionRange
    , csConstraints :: [String] -- we parse these simply as strings
    , csTests       :: Bool
    , csRunTests    :: Bool
    , csBenchmarks  :: Bool
    , csHaddock     :: Bool
    }
  deriving (Show, Generic)

emptyConstraintSet :: String -> ConstraintSet
emptyConstraintSet n = ConstraintSet n anyVersion [] False False False False

-------------------------------------------------------------------------------
-- Grammar
-------------------------------------------------------------------------------

constraintSetGrammar
    :: (C.FieldGrammar g, Applicative (g ConstraintSet))
    => String -> g ConstraintSet ConstraintSet
constraintSetGrammar name = ConstraintSet name
    <$> C.optionalFieldDef "ghc"                                           #csGhcVersions anyVersion
    <*> C.monoidalFieldAla "constraints" (C.alaList' C.CommaVCat NoCommas) #csConstraints
    <*> C.booleanFieldDef  "tests"                                         #csTests False
    <*> C.booleanFieldDef  "run-tests"                                     #csRunTests False
    <*> C.booleanFieldDef  "benchmarks"                                    #csBenchmarks False
    <*> C.booleanFieldDef  "haddock"                                       #csHaddock False
