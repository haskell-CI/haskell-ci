{-# LANGUAGE DeriveFunctor #-}
module Distribution.FieldGrammar.Pretty3 (
    PrettyFieldGrammar3,
    prettyFieldGrammar3,
    ) where

import Prelude ()
import Prelude.Compat

import           Distribution.Compat.Lens
import           Distribution.Compat.Newtype
import           Distribution.Fields.Pretty  (PrettyField (..))
import           Distribution.Parsec.Field   (FieldName)
import           Distribution.Pretty         (Pretty (..))
import           Distribution.Simple.Utils   (toUTF8BS)
import           Text.PrettyPrint            (Doc)
import qualified Text.PrettyPrint            as PP

import Distribution.FieldGrammar.Class

newtype PrettyFieldGrammar3 s a = PrettyFG
    { fieldGrammarPretty :: s -> [PrettyField]
    }
  deriving (Functor)

instance Applicative (PrettyFieldGrammar3 s) where
    pure _ = PrettyFG (\_ -> mempty)
    PrettyFG f <*> PrettyFG x = PrettyFG (\s -> f s <> x s)

-- | We can use 'PrettyFieldGrammar3' to pp print the @s@.
--
-- /Note:/ there is not trailing @($+$ text "")@.
prettyFieldGrammar3 :: PrettyFieldGrammar3 s a -> s -> [PrettyField]
prettyFieldGrammar3 = fieldGrammarPretty

instance FieldGrammar PrettyFieldGrammar3 where
    blurFieldGrammar f (PrettyFG pp) = PrettyFG (pp . aview f)

    uniqueFieldAla fn _pack l = PrettyFG $ \s ->
        ppField fn (pretty (pack' _pack (aview l s)))

    booleanFieldDef fn l def = PrettyFG pp
      where
        pp s
            | b == def  = mempty
            | otherwise = ppField fn (PP.text (show b))
          where
            b = aview l s

    optionalFieldAla fn _pack l = PrettyFG pp
      where
        pp s = case aview l s of
            Nothing -> mempty
            Just a  -> ppField fn (pretty (pack' _pack a))

    optionalFieldDefAla fn _pack l def = PrettyFG pp
      where
        pp s
            | x == def  = mempty
            | otherwise = ppField fn (pretty (pack' _pack x))
          where
            x = aview l s

    monoidalFieldAla fn _pack l = PrettyFG pp
      where
        pp s = ppField fn (pretty (pack' _pack (aview l s)))

    prefixedFields _fnPfx l = PrettyFG (pp . aview l)
      where
        pp xs =
            -- always print the field, even its Doc is empty.
            -- i.e. don't use ppField
            [ PrettyField (toUTF8BS n) $ PP.vcat $ map PP.text $ lines s
            | (n, s) <- xs
            -- fnPfx `isPrefixOf` n
            ]

    knownField _           = pure ()
    deprecatedSince _ _ x  = x

    availableSince _ _     = id
    hiddenField _          = PrettyFG (\_ -> mempty)

ppField :: FieldName -> Doc -> [PrettyField]
ppField name fielddoc
    | PP.isEmpty fielddoc = []
    | otherwise        = [ PrettyField name fielddoc ]
