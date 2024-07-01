{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module HaskellCI.Prelude (
    module Prelude.Compat,
    module X,
    module HaskellCI.Prelude,
) where

import Prelude.Compat hiding (head, tail)

import Algebra.Lattice         as X (BoundedJoinSemiLattice (..), BoundedLattice, BoundedMeetSemiLattice (..), Lattice (..))
import Control.Applicative     as X (liftA2, (<|>))
import Control.Exception       as X (Exception (..), IOException, handle)
import Control.Monad           as X (ap, unless, void, when)
import Control.Monad.Catch     as X (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Class  as X (MonadIO (..))
import Data.Bifoldable         as X (Bifoldable (..))
import Data.Bifunctor          as X (Bifunctor (..))
import Data.Binary             as X (Binary)
import Data.Bitraversable      as X (Bitraversable (..), bifoldMapDefault, bimapDefault)
import Data.ByteString         as X (ByteString)
import Data.Char               as X (isSpace, isUpper, toLower)
import Data.Coerce             as X (coerce)
import Data.Either             as X (partitionEithers)
import Data.Foldable           as X (for_, toList, traverse_)
import Data.Foldable.WithIndex as X (ifoldr)
import Data.Function           as X (on)
import Data.Functor.Compat     as X ((<&>))
import Data.Functor.Identity   as X (Identity (..))
import Data.List               as X (intercalate, isPrefixOf, nub, stripPrefix, tails)
import Data.List.NonEmpty      as X (NonEmpty (..), groupBy)
import Data.Maybe              as X (fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import Data.Proxy              as X (Proxy (..))
import Data.Set                as X (Set)
import Data.String             as X (IsString (fromString))
import Data.Text               as X (Text)
import Data.Typeable           as X (Typeable)
import Data.Void               as X (Void)
import GHC.Generics            as X (Generic)
import Network.URI             as X (URI, parseURI, uriToString)
import Numeric.Natural         as X (Natural)
import Text.Read               as X (readMaybe)

import Data.Generics.Lens.Lite  as X (field)
import Distribution.Compat.Lens as X (over, toListOf, (&), (.~), (^.))

import Distribution.Parsec        as X (simpleParsec)
import Distribution.Pretty        as X (prettyShow)
import Distribution.Utils.Generic as X (fromUTF8BS, toUTF8BS)
import Distribution.Version       as X (Version, VersionRange, anyVersion, mkVersion, noVersion)

import qualified Distribution.Compat.CharParsing as C
import qualified Distribution.Parsec             as C
import qualified Distribution.Pretty             as C
import qualified Distribution.Version            as C
import qualified Text.PrettyPrint                as PP

import Data.Functor.WithIndex.Instances ()

-------------------------------------------------------------------------------
-- Extras
-------------------------------------------------------------------------------

mapped :: forall f a b. Functor f => (a -> Identity b) -> f a -> Identity (f b)
mapped = coerce (fmap :: (a -> b) -> f a -> f b)

head :: NonEmpty a -> a
head (x :| _) = x

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

-- | Whether two ranges are equivalent.
equivVersionRanges :: C.VersionRange -> C.VersionRange -> Bool
equivVersionRanges = on (==) C.asVersionIntervals

-------------------------------------------------------------------------------
-- Orphans
-------------------------------------------------------------------------------

instance Lattice VersionRange where
    (/\) = C.intersectVersionRanges
    (\/) = C.unionVersionRanges

instance C.Parsec Natural where
    parsec = C.integral

instance C.Pretty Natural where
    pretty = PP.text . show

type a := b = (a, b)

pattern (:=) :: a -> b -> a := b
pattern a := b = (a, b)

infixr 0 :=
