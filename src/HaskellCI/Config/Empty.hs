{-# LANGUAGE FunctionalDependencies #-}
module HaskellCI.Config.Empty where

import HaskellCI.Prelude

import qualified Distribution.FieldGrammar as C
import qualified Distribution.Fields       as C

import HaskellCI.OptionsGrammar

newtype EmptyGrammar s a = EG { runEG :: Either (NonEmpty C.FieldName) a }
  deriving Functor

instance Applicative (EmptyGrammar s) where
    pure x = EG (Right x)
    EG f <*> EG x = EG (apVal f x) where
        apVal (Right g) (Right y) = Right (g y)
        apVal (Right _) (Left  y) = Left y
        apVal (Left  g) (Right _) = Left g
        apVal (Left  g) (Left  y) = Left (g <> y)

instance C.FieldGrammar Typeable EmptyGrammar where
    blurFieldGrammar _ = coerce

    uniqueFieldAla fn _ _         = EG (Left (pure fn))
    booleanFieldDef _ _ def       = EG (Right def)
    optionalFieldAla _ _ _        = EG (Right Nothing)
    optionalFieldDefAla _ _ _ def = EG (Right def)
    monoidalFieldAla _ _ _        = EG (Right mempty)

    freeTextField _ _      = EG (Right Nothing)
    freeTextFieldDef _ _   = EG (Right "")
    freeTextFieldDefST _ _ = EG (Right (fromString ""))

    prefixedFields _ _   = pure []
    knownField _         = pure ()
    deprecatedSince _  _ = id
    availableSince _ _   = id
    removedIn _ _        = id
    hiddenField          = id

instance OptionsGrammar Typeable EmptyGrammar
