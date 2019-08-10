{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module HaskellCI.Config.CopyFields where

import HaskellCI.Prelude

import qualified Distribution.Compat.CharParsing as C
import qualified Distribution.Parsec             as C
import qualified Distribution.Pretty             as C
import qualified Text.PrettyPrint                as PP

data CopyFields
    = CopyFieldsNone
    | CopyFieldsSome
    | CopyFieldsAll
  deriving (Eq, Ord, Show, Enum, Bounded)

-------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------

showCopyFields :: CopyFields -> String
showCopyFields CopyFieldsNone = "none"
showCopyFields CopyFieldsSome = "some"
showCopyFields CopyFieldsAll  = "all"

instance C.Pretty CopyFields where
    pretty = PP.text . showCopyFields

instance C.Parsec CopyFields where
    parsec = C.choice
        [ f <$ C.string (showCopyFields f)
        | f <- [ minBound .. maxBound ]
        ]
