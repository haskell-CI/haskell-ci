{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
module HaskellCI.Config.ConstraintSet where

import           Data.Generics.Labels         ()
import           Distribution.Version
import           GHC.Generics                 (Generic)

import qualified Distribution.FieldGrammar    as C
import qualified Distribution.Parsec.Newtypes as C

import           HaskellCI.Newtypes

data ConstraintSet = ConstraintSet
    { csName        :: String
    , csGhcVersions :: VersionRange
    , csConstraints :: [String] -- we parse these simply as strings
    }
  deriving (Show, Generic)

emptyConstraintSet :: String -> ConstraintSet
emptyConstraintSet n = ConstraintSet n anyVersion []

-------------------------------------------------------------------------------
-- Grammar
-------------------------------------------------------------------------------

constraintSetGrammar
    :: (C.FieldGrammar g, Applicative (g ConstraintSet))
    => String -> g ConstraintSet ConstraintSet
constraintSetGrammar name = ConstraintSet name
    <$> C.uniqueField      "ghc"                                           #csGhcVersions
    <*> C.monoidalFieldAla "constraints" (C.alaList' C.CommaVCat NoCommas) #csConstraints
