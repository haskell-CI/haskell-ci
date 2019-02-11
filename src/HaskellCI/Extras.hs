-- | Various extras, are this in @base@ ?
module HaskellCI.Extras where

import           Control.Applicative ((<|>))
import           Data.List           (isPrefixOf, tails)

-- $setup
-- >>> import Text.Read (readMaybe)

-- | Return the part after the first argument
--
-- >>> afterInfix "BAR" "FOOBAR XYZZY"
-- Just " XYZZY"
--
afterInfix :: Eq a => [a] -> [a] -> Maybe [a]
afterInfix needle haystack = findMaybe (afterPrefix needle) (tails haystack)

-- |
--
-- >>> afterPrefix "FOO" "FOOBAR"
-- Just "BAR"
--
afterPrefix :: Eq a => [a] -> [a] -> Maybe [a]
afterPrefix needle haystack
    | needle `isPrefixOf` haystack = Just (drop (length needle) haystack)
    | otherwise                    = Nothing

-- |
--
-- >>> findMaybe readMaybe ["foo", "1", "bar"] :: Maybe Int
-- Just 1
--
findMaybe :: (a -> Maybe b) -> [a] -> Maybe b
findMaybe f = foldr (\a b -> f a <|> b) Nothing
