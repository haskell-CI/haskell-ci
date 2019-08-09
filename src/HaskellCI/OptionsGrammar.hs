module HaskellCI.OptionsGrammar (
    OptionsGrammar (..),
    (C.^^^),
    )  where

import HaskellCI.Prelude

import qualified Distribution.Compat.Lens        as C
import qualified Distribution.FieldGrammar       as C
import qualified Distribution.Fields             as C
import qualified Distribution.Types.VersionRange as C

import HaskellCI.Newtypes

class C.FieldGrammar p => OptionsGrammar p where
    metahelp :: String -> String -> p s a -> p s a
    metahelp _ _ = id

    help :: String -> p s a -> p s a
    help _ = id

    -- we treat range fields specially in options
    rangeField :: C.FieldName -> C.ALens' s C.VersionRange -> C.VersionRange -> p s C.VersionRange
    rangeField fn = C.optionalFieldDefAla fn Range

instance OptionsGrammar C.ParsecFieldGrammar
