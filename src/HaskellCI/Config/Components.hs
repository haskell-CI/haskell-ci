module HaskellCI.Config.Components where

import HaskellCI.Prelude

import qualified Distribution.Compat.CharParsing as C
import qualified Distribution.Parsec             as C
import qualified Distribution.Pretty             as C
import qualified Text.PrettyPrint                as PP

-------------------------------------------------------------------------------
-- Single action
-------------------------------------------------------------------------------

data Components
    = ComponentsAll
    | ComponentsLibs
  deriving (Eq, Show)

instance C.Pretty Components where
    pretty ComponentsAll  = PP.text "all"
    pretty ComponentsLibs = PP.text "libs"

instance C.Parsec Components where
    parsec = do
        t <- C.parsecToken
        case t of
            "all"  -> return ComponentsAll
            "libs" -> return ComponentsLibs
            _      -> C.unexpected $ "Component " ++ t
