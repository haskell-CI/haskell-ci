module HaskellCI.Config.Ubuntu where

import qualified Distribution.Parsec.Class as C
import qualified Distribution.Pretty       as C
import qualified Text.PrettyPrint          as PP

data Ubuntu = Xenial | Bionic
  deriving (Eq, Ord, Show)

instance C.Parsec Ubuntu where
    parsec = do
        t <- C.parsecToken
        case t of
            "xenial" -> return Xenial
            "bionic" -> return Bionic
            _        -> fail $ "Unknown ubuntu release " ++ t

instance C.Pretty Ubuntu where
    pretty = PP.text . showUbuntu

showUbuntu :: Ubuntu -> String
showUbuntu Xenial = "xenial"
showUbuntu Bionic = "bionic"
