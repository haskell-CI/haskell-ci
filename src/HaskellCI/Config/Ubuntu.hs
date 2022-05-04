module HaskellCI.Config.Ubuntu where

import HaskellCI.Prelude

import qualified Distribution.Parsec as C
import qualified Distribution.Pretty as C
import qualified Text.PrettyPrint    as PP

data Ubuntu = Xenial | Bionic | Focal | Jammy
  deriving (Eq, Ord, Show, Enum, Bounded)

instance C.Parsec Ubuntu where
    parsec = do
        t <- C.parsecToken
        case t of
            "xenial" -> return Xenial
            "bionic" -> return Bionic
            "focal"  -> return Focal
            "jammy"  -> return Jammy
            _        -> fail $ "Unknown ubuntu release " ++ t

instance C.Pretty Ubuntu where
    pretty = PP.text . showUbuntu

showUbuntu :: Ubuntu -> String
showUbuntu Xenial = "xenial"
showUbuntu Bionic = "bionic"
showUbuntu Focal  = "focal"
showUbuntu Jammy  = "jammy"
