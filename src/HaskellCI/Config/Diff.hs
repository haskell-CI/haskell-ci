{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module HaskellCI.Config.Diff where

import HaskellCI.Prelude

import Distribution.Simple.Utils (fromUTF8BS)
import Distribution.Fields.Field (FieldName)
import Distribution.Utils.ShortText (fromShortText)

import qualified Distribution.Compat.Lens        as L
import qualified Distribution.Compat.CharParsing as C
import qualified Distribution.FieldGrammar       as C
import qualified Distribution.Parsec             as C
import qualified Distribution.Pretty             as C
import qualified Text.PrettyPrint                as PP

import HaskellCI.OptionsGrammar

data ShowDiffOptions = ShowAllOptions | ShowChangedOptions
    deriving (Eq, Show, Generic, Binary)

instance C.Parsec ShowDiffOptions where
    parsec = ShowAllOptions <$ C.string "all"
        <|> ShowChangedOptions <$ C.string "changed"

instance C.Pretty ShowDiffOptions where
    pretty ShowAllOptions = PP.text "all"
    pretty ShowChangedOptions = PP.text "changed"

data DiffConfig = DiffConfig
    { diffShowOptions :: ShowDiffOptions
    , diffShowOld :: Bool
    } deriving (Show, Generic, Binary)

diffConfigGrammar
    :: ( OptionsGrammar c g
       , Applicative (g DiffConfig)
       , c (Identity ShowDiffOptions))
    => g DiffConfig DiffConfig
diffConfigGrammar = DiffConfig
    <$> C.optionalFieldDef "diff-show-options" (field @"diffShowOptions") ShowChangedOptions
        ^^^ help "Which fields to show"
    <*> C.booleanFieldDef "diff-show-old" (field @"diffShowOld") False
        ^^^ help "Show the old values for every field"

newtype DiffOptions s a =
  DiffOptions { runDiffOptions :: (s, s) -> DiffConfig -> [String] }
  deriving Functor

instance Applicative (DiffOptions s) where
    pure _ = DiffOptions $ \_ _ -> []
    DiffOptions f <*> DiffOptions x = DiffOptions (f <> x)

diffConfigs :: DiffConfig -> DiffOptions a a -> a -> a -> [String]
diffConfigs config grammar oldVal newVal =
  runDiffOptions grammar (oldVal, newVal) config

diffUnique
    :: Eq b
    => (a -> b)
    -> (a -> String)
    -> FieldName
    -> L.ALens' s a
    -> (s, s)
    -> DiffConfig
    -> [String]
diffUnique project render fn lens (diffOld, diffNew) opts =
  case diffShowOptions opts of
    ShowChangedOptions | notEqual -> []
    ShowAllOptions | notEqual -> newLine
    _ -> oldLine ++ newLine
  where
    notEqual = project oldValue == project newValue
    oldValue = L.aview lens $ diffOld
    newValue = L.aview lens $ diffNew

    oldLine
        | diffShowOld opts = ["-- " ++ fromUTF8BS fn ++ ": " ++ render oldValue]
        | otherwise = []

    newLine = [ fromUTF8BS fn ++ ": " ++ render newValue, ""]


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
        toPretty = maybe "" C.prettyShow . fmap pack

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

    help h (DiffOptions xs) = DiffOptions $ \vals config ->
        case xs vals config of
            [] -> []
            diffString -> ("-- " ++ h) : diffString
