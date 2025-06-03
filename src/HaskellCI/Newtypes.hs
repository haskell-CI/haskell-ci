{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module HaskellCI.Newtypes where

import HaskellCI.Prelude

import qualified Data.Set                           as S
import qualified Data.Map                           as Map
import qualified Distribution.Compat.CharParsing    as C
import qualified Distribution.Compat.Newtype        as C
import qualified Distribution.FieldGrammar.Newtypes as C
import qualified Distribution.Parsec                as C
import qualified Distribution.Pretty                as C
import qualified Distribution.Types.Version         as C
import qualified Distribution.Types.VersionRange    as C
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
    parsec = NoCommas . mergeSpaces <$> liftA2 (:) (C.satisfy (not . isSpace)) (C.munch (/= ','))

mergeSpaces :: String -> String
mergeSpaces []        = []
mergeSpaces (' ' : s) = ' ' : go s where
    go []         = []
    go (' ' : s') = go s'
    go (c   : s') = c : mergeSpaces s'
mergeSpaces (c   : s) = c   : mergeSpaces s

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

-------------------------------------------------------------------------------
-- AlaMap
-------------------------------------------------------------------------------

newtype AlaMap sep b k v = AlaMap { getAlaMap :: Map k v }
  deriving anyclass (C.Newtype (Map k v))

alaMap' :: sep -> ((k, v) -> b) -> Map k v -> AlaMap sep b k v
alaMap' _ _ = AlaMap

instance (C.Newtype (k, v) b, Ord k, C.Sep sep, C.Parsec b) => C.Parsec (AlaMap sep b k v) where
    parsec   = C.pack . Map.fromList . map (C.unpack :: b -> (k, v)) <$> C.parseSep (hack (Proxy :: Proxy sep)) C.parsec

instance (C.Newtype (k, v) b, C.Sep sep, C.Pretty b) => C.Pretty (AlaMap sep b k v) where
    pretty = C.prettySep (hack (Proxy :: Proxy sep)) . map (C.pretty . (C.pack :: (k, v) -> b)) . Map.toList . C.unpack

-------------------------------------------------------------------------------
-- VersionPair
-------------------------------------------------------------------------------

newtype VersionPair = VersionPair (C.Version, C.Version)
  deriving anyclass (C.Newtype (C.Version, C.Version))

instance C.Parsec VersionPair where
    parsec = do
        a <- C.parsec
        _ <- C.char ':'
        b <- C.parsec
        return (VersionPair (a, b))

instance C.Pretty VersionPair where
    pretty (VersionPair (a, b)) = C.pretty a <> PP.text ":" <> C.pretty b

-------------------------------------------------------------------------------
-- AlaMap
-------------------------------------------------------------------------------

-- Someone (= me) forgot to export Distribution.Parsec.Newtypes.P
hack :: Proxy a -> proxy a
hack _ = undefined
