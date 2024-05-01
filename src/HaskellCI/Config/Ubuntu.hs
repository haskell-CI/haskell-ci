module HaskellCI.Config.Ubuntu where

import HaskellCI.Prelude

import qualified Distribution.Parsec as C
import qualified Distribution.Pretty as C
import qualified Text.PrettyPrint    as PP

data Ubuntu = Focal | Jammy | Noble
  deriving (Eq, Ord, Show, Enum, Bounded)

instance C.Parsec Ubuntu where
    parsec = do
        t <- C.parsecToken
        case t of
            "focal"  -> return Focal
            "jammy"  -> return Jammy
            "noble"  -> return Noble
            _        -> fail $ "Unknown ubuntu release " ++ t

instance C.Pretty Ubuntu where
    pretty = PP.text . showUbuntu

showUbuntu :: Ubuntu -> String
showUbuntu Focal  = "focal"
showUbuntu Jammy  = "jammy"
showUbuntu Noble  = "noble"
