{-# LANGUAGE DeriveFunctor #-}
module HaskellCI.List (
    ListBuilder,
    buildList,
    item,
    ) where

import Control.Monad (ap)

newtype ListBuilder x a = LB { unLB :: ([x] -> [x]) -> ([x] -> [x], a) }
  deriving Functor

instance Applicative (ListBuilder x) where
    pure x = LB $ \f -> (f, x)
    (<*>)  = ap

instance Monad (ListBuilder x) where
    return = pure

    m >>= k = LB $ \f0 -> let (f1, x) = unLB m f0 in unLB (k x) f1

buildList :: ListBuilder x () -> [x]
buildList (LB f) = case f id of
    (g, ()) -> g []

item :: x -> ListBuilder x ()
item x = LB $ \f -> (f . (x :), ())
