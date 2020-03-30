module HaskellCI.Config.PackageScope where

import HaskellCI.Prelude

import qualified Distribution.Compat.CharParsing as C
import qualified Distribution.Parsec             as C
import qualified Distribution.Pretty             as C
import qualified Text.PrettyPrint                as PP

data PackageScope
    = PackageScopeNone
    | PackageScopeLocal
    | PackageScopeAll
  deriving (Eq, Show)

instance C.Parsec PackageScope where
    parsec = 
            PackageScopeNone  <$ C.string "none"
        <|> PackageScopeLocal <$ C.string "local"
        <|> PackageScopeAll   <$ C.string "all"

instance C.Pretty PackageScope where
    pretty PackageScopeNone  = PP.text "none"
    pretty PackageScopeLocal = PP.text "local"
    pretty PackageScopeAll   = PP.text "all"
