{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module HaskellCI.Config.Diff where

import HaskellCI.Prelude

import Distribution.Fields.Field (FieldName)
import Distribution.Utils.ShortText (fromShortText)

import qualified Distribution.Compat.Lens        as L
import qualified Distribution.FieldGrammar       as C
import qualified Distribution.Pretty             as C

import HaskellCI.OptionsGrammar

newtype DiffOptions s a =
  DiffOptions { runDiffOptions :: (s, s) -> [String] }
  deriving Functor

instance Applicative (DiffOptions s) where
    pure _ = DiffOptions $ \_ -> []
    DiffOptions f <*> DiffOptions x = DiffOptions (f <> x)

diffConfigs :: DiffOptions a a -> a -> a -> [String]
diffConfigs grammar oldVal newVal =
  runDiffOptions grammar (oldVal, newVal)

diffUnique
    :: Eq b
    => (a -> b)
    -> (a -> String)
    -> FieldName
    -> L.ALens' s a
    -> (s, s)
    -> [String]
diffUnique project render fn lens (diffOld, diffNew)
    | notEqual =
    [ "-" ++ fromUTF8BS fn ++ ": " ++ render oldValue
    , "+" ++ fromUTF8BS fn ++ ": " ++ render newValue
    , ""
    ]

    | otherwise = []
  where
    notEqual = project oldValue /= project newValue
    oldValue = L.aview lens $ diffOld
    newValue = L.aview lens $ diffNew

instance C.FieldGrammar C.Pretty DiffOptions where
    blurFieldGrammar lens (DiffOptions diff) =
        DiffOptions $ diff . bimap (L.aview lens) (L.aview lens)

    uniqueFieldAla fn pack valueLens = DiffOptions $
        diffUnique (C.prettyShow . pack) (C.prettyShow . pack) fn valueLens

    booleanFieldDef fn valueLens _ = DiffOptions $
        diffUnique id C.prettyShow fn valueLens

    optionalFieldAla fn pack valueLens = DiffOptions $
        diffUnique toPretty toPretty fn valueLens
      where
        toPretty = maybe "" (C.prettyShow . pack)

    optionalFieldDefAla fn pack valueLens _ = DiffOptions $
        diffUnique id (C.prettyShow . pack) fn valueLens

    monoidalFieldAla fn pack valueLens = DiffOptions $
        diffUnique (C.prettyShow . pack) (C.prettyShow . pack) fn valueLens

    freeTextField fn valueLens = DiffOptions $
        diffUnique id (fromMaybe "") fn valueLens

    freeTextFieldDef fn valueLens = DiffOptions $
        diffUnique id id fn valueLens

    freeTextFieldDefST fn valueLens = DiffOptions $
        diffUnique id fromShortText fn valueLens

    prefixedFields _ _   = pure []
    knownField _         = pure ()
    deprecatedSince _  _ = id
    availableSince _ _   = id
    removedIn _ _        = id
    hiddenField          = id

instance OptionsGrammar C.Pretty DiffOptions where
    metahelp _ = help

    help h (DiffOptions xs) = DiffOptions $ \vals ->
        case xs vals of
            [] -> []
            diffString -> ("-- " ++ h) : diffString
