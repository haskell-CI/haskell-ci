module HaskellCI.Optimization where

import HaskellCI.Prelude

import qualified Distribution.Compat.CharParsing as C
import qualified Distribution.Parsec             as C
import qualified Distribution.Pretty             as C
import qualified Text.PrettyPrint                as PP

data Optimization
    = OptimizationOn
    | OptimizationOff
    | OptimizationLevel Int
  deriving (Eq, Show)

instance C.Parsec Optimization where
    parsec = boolean <|> numeric where
        boolean = ite OptimizationOn OptimizationOff <$> C.parsec
        numeric = OptimizationLevel <$> C.integral

        ite t _ True  = t
        ite _ f False = f

instance C.Pretty Optimization where
    pretty OptimizationOn        = C.pretty True
    pretty OptimizationOff       = C.pretty False
    pretty (OptimizationLevel l) = PP.int l
