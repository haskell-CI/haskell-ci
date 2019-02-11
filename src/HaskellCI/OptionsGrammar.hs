module HaskellCI.OptionsGrammar (
    OptionsGrammar (..),
    (C.^^^),
    )  where

import qualified Distribution.FieldGrammar   as C

class C.FieldGrammar p => OptionsGrammar p where
    metahelp :: String -> String -> p s a -> p s a
    metahelp _ _ = id

    help :: String -> p s a -> p s a
    help _ = id

instance OptionsGrammar C.ParsecFieldGrammar
