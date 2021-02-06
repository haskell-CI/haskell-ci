{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module HaskellCI.OptparseGrammar (
    OptparseGrammar,
    runOptparseGrammar,
) where

import HaskellCI.Prelude

import Control.Applicative       (many)
import Data.Foldable             (asum)
import Distribution.Simple.Utils (fromUTF8BS)

import qualified Distribution.Compat.Lens    as C
import qualified Distribution.Compat.Newtype as C
import qualified Distribution.FieldGrammar   as C
import qualified Distribution.Fields         as C
import qualified Distribution.Parsec         as C
import qualified Distribution.Pretty         as C
import qualified Distribution.Version        as C
import qualified Options.Applicative         as O

import HaskellCI.OptionsGrammar

data SomeParser s where
    SP :: (Maybe String -> Maybe String -> O.Parser (s -> s)) -> SomeParser s

newtype OptparseGrammar s a = OG [SomeParser s]
  deriving Functor

runOptparseGrammar :: OptparseGrammar s a -> O.Parser (s -> s)
runOptparseGrammar (OG ps) = fmap (foldr (flip (.)) id) $ many $ asum
    [ p Nothing Nothing
    | SP p <- ps
    ]

instance Applicative (OptparseGrammar s) where
    pure _ = OG []
    OG f <*> OG x = OG (f ++ x)

instance C.FieldGrammar ParsecPretty OptparseGrammar where
    blurFieldGrammar l (OG ps) = OG
        [ SP $ \v h -> fmap (l C.#%~) (p v h)
        | SP p <- ps
        ]

    -- we don't support unique fields atm
    uniqueFieldAla _ _ _ = OG []

    -- the non default flag has help entry
    booleanFieldDef fn l def = OG
        [ SP $ \_m h -> setOG l $ O.flag' True  $ flagMods fn (th h)
        , SP $ \_m h -> setOG l $ O.flag' False $ flagMods ("no-" <> fn) (fh h)
        ]
      where
        th h = if def then Nothing else h
        fh h = if def then h else Nothing

    optionalFieldAla fn c l = OG
        [ SP $ \m h -> setOptionalOG l $ O.option (C.unpack' c <$> readMParsec) $ optionMods fn m h ]

    optionalFieldDefAla fn c l def = OG
        [ SP $ \m h -> setOG l $ O.option (C.unpack' c <$> readMParsec) $ optionMods fn m (fmap hdef h) ]
      where
        hdef h = h ++ " (Default: " ++ C.prettyShow (C.pack' c def) ++ ")"

    monoidalFieldAla fn c l = OG
        [ SP $ \m h -> monoidOG l $ O.option (C.unpack' c <$> readMParsec) $ optionMods fn m h ]

    prefixedFields _ _   = pure []
    knownField _         = pure ()
    deprecatedSince _  _ = id
    availableSince _ _   = id
    removedIn _ _        = id
    hiddenField          = id

    freeTextField fn l = OG
        [ SP $ \m h -> setOptionalOG l $ O.strOption $ optionMods fn m h ]

    freeTextFieldDef fn l = OG
        [ SP $ \m h -> setOG l $ O.strOption $ optionMods fn m h ]

    freeTextFieldDefST fn l = OG
        [ SP $ \m h -> setOG l $ O.strOption $ optionMods fn m h ]

instance OptionsGrammar ParsecPretty OptparseGrammar where
    help h (OG ps) = OG
        [ SP $ \m _h -> p m (Just h)
        | SP p <- ps
        ]

    metahelp m h (OG ps) = OG
        [ SP $ \_m _h -> p (Just m) (Just h)
        | SP p <- ps
        ]

    -- example: @rangeField tests #cfgTests anyVersion@, generates options:
    --
    -- --tests
    -- --no-tests
    -- --tests-jobs RANGE
    --
    -- where the --no-tests has help, because it's not default.
    --
    rangeField fn l def = OG
        [ SP $ \_m  h -> setOG l $ O.flag' C.anyVersion $ flagMods fn (th h)
        , SP $ \_m  h -> setOG l $ O.flag' C.noVersion  $ flagMods ("no-" <> fn) (fh h)
        , SP $ \_m _h -> setOG l $ O.option readMParsec $ O.long (fromUTF8BS $ fn <> "-jobs") <> O.metavar "RANGE"
        ]
      where
        th h = if equivVersionRanges def C.anyVersion then Nothing else h
        fh h = if equivVersionRanges def C.anyVersion then h else Nothing

optionMods :: (O.HasName mods, O.HasMetavar mods) => C.FieldName -> Maybe String -> Maybe String -> O.Mod mods a
optionMods fn mmetavar mhelp = flagMods fn mhelp
    <> maybe mempty O.metavar mmetavar

flagMods :: O.HasName mods => C.FieldName -> Maybe String -> O.Mod mods a
flagMods fn mhelp = O.long (fromUTF8BS fn)
    <> maybe mempty O.help mhelp

readMParsec :: C.Parsec a => O.ReadM a
readMParsec = O.eitherReader C.eitherParsec

setOG :: C.ALens' s a -> O.Parser a -> O.Parser (s -> s)
setOG l = fmap (l C.#~)

setOptionalOG :: C.ALens' s (Maybe a) -> O.Parser a -> O.Parser (s -> s)
setOptionalOG l = fmap $ \x -> l C.#~ Just x

monoidOG :: Monoid a => C.ALens' s a -> O.Parser a -> O.Parser (s -> s)
monoidOG l = fmap $ \x -> l C.#%~ \y -> mappend y x
