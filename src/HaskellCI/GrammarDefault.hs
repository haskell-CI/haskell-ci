module HaskellCI.GrammarDefault where

import HaskellCI.Prelude

import qualified Distribution.Compat.Lens    as C
import qualified Distribution.Compat.Newtype as C
import qualified Distribution.FieldGrammar   as C
import qualified Distribution.Fields         as C

import HaskellCI.OptionsGrammar

blurFieldGrammar :: OptionsGrammar c g => C.ALens' s d -> (d -> g d d) -> s -> g s d
blurFieldGrammar l sub s = C.blurFieldGrammar l (sub (C.aview l s))

monoidalFieldAla :: (OptionsGrammar c g, C.Newtype a b, c b, Monoid a) => C.FieldName -> (a -> b) -> C.ALens' s a -> g s a
monoidalFieldAla = C.monoidalFieldAla

freeTextField :: OptionsGrammar c g => C.FieldName -> C.ALens' s (Maybe String) -> g s (Maybe String)
freeTextField = C.freeTextField

freeTextFieldDef :: OptionsGrammar c g => C.FieldName -> C.ALens' s String -> g s String
freeTextFieldDef = C.freeTextFieldDef

booleanFieldDef :: (OptionsGrammar c g) => C.FieldName -> C.ALens' s Bool -> s -> g s Bool
booleanFieldDef fn l s = C.booleanFieldDef fn l (C.aview l s)

optionalField :: (OptionsGrammar c g, c a) => C.FieldName -> C.ALens' s (Maybe a) -> g s (Maybe a)
optionalField fn l = C.optionalField fn l

optionalFieldAla :: (OptionsGrammar c g, C.Newtype a b, c b) => C.FieldName -> (a -> b) -> C.ALens' s (Maybe a) -> g s (Maybe a)
optionalFieldAla fn pack l = C.optionalFieldAla fn pack l

optionalFieldDef :: (OptionsGrammar c g, c a, Eq a) => C.FieldName -> C.ALens' s a -> s -> g s a
optionalFieldDef fn l s = C.optionalFieldDef fn l (C.aview l s)

optionalFieldDefAla :: (OptionsGrammar c g, C.Newtype a b, c b, Eq a) => C.FieldName -> (a -> b) -> C.ALens' s a -> s -> g s a
optionalFieldDefAla fn pack l s = C.optionalFieldDefAla fn pack l (C.aview l s)
