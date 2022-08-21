{-# LANGUAGE FunctionalDependencies  #-}
module HaskellCI.Config.Dump where

import HaskellCI.Prelude

import qualified Distribution.Compat.Newtype as C
import qualified Distribution.FieldGrammar   as C
import qualified Distribution.Pretty         as C

import HaskellCI.OptionsGrammar

-- TODO: with Cabal-2.6 this can be prettier, using Pretty.Field
newtype DumpGrammar s a = DG { runDG :: [String] }
  deriving Functor

instance Applicative (DumpGrammar s) where
    pure _ = DG []
    DG f <*> DG x = DG (f ++ x)

instance C.FieldGrammar C.Pretty DumpGrammar where
    blurFieldGrammar _ = coerce

    uniqueFieldAla _ _ _ = DG []

    booleanFieldDef fn _ def = DG
        [ fromUTF8BS fn ++ ": " ++ C.prettyShow def
        , ""
        ]

    optionalFieldAla fn _ _ = DG
        [ fromUTF8BS fn ++ ":"
        , ""
        ]

    optionalFieldDefAla fn c _ def = DG
        [ fromUTF8BS fn ++ ": " ++ C.prettyShow (C.pack' c def)
        , ""
        ]

    monoidalFieldAla fn _ _ = DG
        [ fromUTF8BS fn ++ ":"
        , ""
        ]

    freeTextField fn _ = DG
        [ fromUTF8BS fn ++ ":"
        , ""
        ]

    freeTextFieldDef fn _ = DG
        [ fromUTF8BS fn ++ ":"
        , ""
        ]

    freeTextFieldDefST fn _ = DG
        [ fromUTF8BS fn ++ ":"
        , ""
        ]

    prefixedFields _ _   = pure []
    knownField _         = pure ()
    deprecatedSince _  _ = id
    availableSince _ _   = id
    removedIn _ _        = id
    hiddenField          = id

instance OptionsGrammar C.Pretty DumpGrammar where
    metahelp _ = help

    help h (DG xs) = DG $
        ("-- " ++ h) : xs
