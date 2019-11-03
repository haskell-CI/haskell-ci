{-# LANGUAGE DeriveGeneric #-}
-- | License: GPL-3.0-or-later AND BSD-3-Clause
--
-- Optimization level.
module Cabal.Optimization (
    Optimization (..),
    ) where

import Control.Applicative (Alternative (..))
import GHC.Generics        (Generic)

import qualified Distribution.Compat.CharParsing as C
import qualified Distribution.Parsec             as C
import qualified Distribution.Pretty             as C
import qualified Text.PrettyPrint                as PP

-- | Optimization level, may be turned on with 'True' or off with 'False',
-- or set an explicit optimization level.
data Optimization
    = OptimizationOn
    | OptimizationOff
    | OptimizationLevel Int
  deriving (Eq, Show, Generic)

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
