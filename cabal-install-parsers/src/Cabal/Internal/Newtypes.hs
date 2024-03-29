{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Cabal.Internal.Newtypes where

-- hide redundant import warning
import Prelude hiding (Applicative (..))

import Control.Applicative   (Alternative (..), liftA2)
import Data.Char             (isSpace)
import Data.Function         (on)
import Data.Functor.Identity (Identity (..))
import Data.Proxy            (Proxy (..))
import Network.URI           (URI, parseURI, uriToString)

import qualified Data.Set                           as S
import qualified Distribution.Compat.CharParsing    as C
import qualified Distribution.Compat.Newtype        as C
import qualified Distribution.FieldGrammar.Newtypes as C
import qualified Distribution.Parsec                as C
import qualified Distribution.Pretty                as C
import qualified Distribution.Version               as C
import qualified Text.PrettyPrint                   as PP

-------------------------------------------------------------------------------
-- PackageLocation
-------------------------------------------------------------------------------

newtype PackageLocation = PackageLocation String
  deriving anyclass (C.Newtype String)

-- | This is a bit tricky since it has to cover globs which have embedded @,@
-- chars. But we don't just want to parse strictly as a glob since we want to
-- allow http urls which don't parse as globs, and possibly some
-- system-dependent file paths. So we parse fairly liberally as a token, but
-- we allow @,@ inside matched @{}@ braces.
instance C.Parsec PackageLocation where
    parsec = PackageLocation <$> outerTerm
      where
        outerTerm = ($ "") <$> outerChars

        outerChars, outerChar, innerChars, innerChar :: C.CabalParsing m => m ShowS
        outerChars = foldr (.) id <$> C.some outerChar
        innerChars = foldr (.) id <$> C.many innerChar

        outerChar = do
            c <- C.satisfy $ \c -> not (isSpace c || c == '}' || c == ',')
            kont c

        innerChar = do
            c <- C.satisfy $ \c -> not (isSpace c || c == '}')
            kont c

        kont :: C.CabalParsing m => Char -> m ShowS
        kont c = case c of
           '{' -> do
               cs <- innerChars
               c' <- C.char '}'
               return (showChar c . cs . showChar c')
           _   -> return $ showChar c


instance C.Pretty PackageLocation where
    pretty (PackageLocation p) = PP.text p

-------------------------------------------------------------------------------
-- NoCommas: something which can be comma separated
-------------------------------------------------------------------------------

newtype NoCommas = NoCommas String
  deriving anyclass (C.Newtype String)

instance C.Parsec NoCommas where
    parsec = NoCommas <$> liftA2 (:) (C.satisfy (not . isSpace)) (C.munch (/= ','))

instance C.Pretty NoCommas where
    pretty (NoCommas p) = PP.text p

-------------------------------------------------------------------------------
-- Head version
-------------------------------------------------------------------------------

newtype HeadVersion = HeadVersion { getHeadVersion :: Maybe C.Version }
  deriving anyclass (C.Newtype (Maybe C.Version))

instance C.Parsec HeadVersion where
    parsec = HeadVersion Nothing <$ C.string "head" <|>
        HeadVersion . Just <$> C.parsec

instance C.Pretty HeadVersion where
    pretty (HeadVersion Nothing)  = PP.text "head"
    pretty (HeadVersion (Just v)) = C.pretty v

-------------------------------------------------------------------------------
-- Newtype
-------------------------------------------------------------------------------

newtype Int' = Int' Int
  deriving anyclass (C.Newtype Int)

instance C.Parsec Int' where
    parsec = Int' <$> C.integral

instance C.Pretty Int' where
    pretty (Int' i) = PP.int i

-------------------------------------------------------------------------------
-- Range
-------------------------------------------------------------------------------

newtype Range = Range C.VersionRange
  deriving anyclass (C.Newtype C.VersionRange)

instance C.Parsec Range where
    parsec = fmap Range $ C.parsec <|> fromBool <$> C.parsec where
        fromBool True  = C.anyVersion
        fromBool False = C.noVersion

instance C.Pretty Range where
    pretty (Range r)
        | equivVersionRanges r C.anyVersion = C.pretty True
        | equivVersionRanges r C.noVersion  = C.pretty False
        | otherwise                         = C.pretty r

-------------------------------------------------------------------------------
-- AlaSet
-------------------------------------------------------------------------------

newtype AlaSet sep b a = AlaSet { getAlaSet :: S.Set a }
  deriving anyclass (C.Newtype (S.Set a))

alaSet :: sep -> S.Set a -> AlaSet sep (Identity a) a
alaSet _ = AlaSet

-- | More general version of 'alaSet'.
alaSet' :: sep -> (a -> b) -> S.Set a -> AlaSet sep b a
alaSet' _ _ = AlaSet

instance (C.Newtype a b, Ord a, C.Sep sep, C.Parsec b) => C.Parsec (AlaSet sep b a) where
    parsec   = C.pack . S.fromList . map (C.unpack :: b -> a) <$> C.parseSep (hack (Proxy :: Proxy sep)) C.parsec

instance (C.Newtype a b, C.Sep sep, C.Pretty b) => C.Pretty (AlaSet sep b a) where
    pretty = C.prettySep (hack (Proxy :: Proxy sep)) . map (C.pretty . (C.pack :: a -> b)) . S.toList . C.unpack

-- Someone (= me) forgot to export Distribution.Parsec.Newtypes.P
hack :: Proxy a -> proxy a
hack _ = undefined

-------------------------------------------------------------------------------
-- WrapURI
-------------------------------------------------------------------------------

newtype WrappedURI = WrapURI URI
  deriving anyclass (C.Newtype URI)

instance C.Parsec WrappedURI where
    parsec = do
        t <- C.parsecToken
        case parseURI t of
            Just x  -> return (WrapURI x)
            Nothing -> C.unexpected $ "Not an URI: " ++ t

instance C.Pretty WrappedURI where
    pretty (WrapURI uri) = PP.text (uriToString id uri "")

-------------------------------------------------------------------------------
-- extras
-------------------------------------------------------------------------------

-- | Whether two ranges are equivalent.
equivVersionRanges :: C.VersionRange -> C.VersionRange -> Bool
equivVersionRanges = on (==) C.asVersionIntervals
