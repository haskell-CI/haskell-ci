{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module HaskellCI.Newtypes where

import           Control.Applicative             (liftA2, (<|>))
import           Data.Char                       (isSpace)
import           Data.Coerce                     (coerce)
import           Data.Functor.Identity           (Identity (..))
import           Data.Proxy                      (Proxy (..))

import qualified Data.Set                        as S
import qualified Distribution.Compat.CharParsing as C
import qualified Distribution.Compat.Newtype     as C
import qualified Distribution.Parsec.Class       as C
import qualified Distribution.Parsec.Newtypes    as C
import qualified Distribution.Pretty             as C
import qualified Distribution.Types.Version      as C
import qualified Text.PrettyPrint                as PP

-------------------------------------------------------------------------------
-- PackageLocation
-------------------------------------------------------------------------------

newtype PackageLocation = PackageLocation String

instance C.Newtype PackageLocation [Char] where
    pack = coerce
    unpack = coerce

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

instance C.Newtype NoCommas [Char] where
    pack = coerce
    unpack = coerce

instance C.Parsec NoCommas where
    parsec = NoCommas <$> liftA2 (:) (C.satisfy (not . isSpace)) (C.munch (/= ','))

instance C.Pretty NoCommas where
    pretty (NoCommas p) = PP.text p

-------------------------------------------------------------------------------
-- Head version
-------------------------------------------------------------------------------

newtype HeadVersion = HeadVersion { getHeadVersion :: Maybe C.Version }

instance C.Newtype HeadVersion (Maybe C.Version) where
    pack = coerce
    unpack = coerce

instance C.Parsec HeadVersion where
    parsec = HeadVersion Nothing <$ C.string "head" <|>
        HeadVersion . Just <$> C.parsec

instance C.Pretty HeadVersion where
    pretty (HeadVersion Nothing)  = PP.text "head"
    pretty (HeadVersion (Just v)) = C.pretty v

-------------------------------------------------------------------------------
-- AlaSet
-------------------------------------------------------------------------------

newtype AlaSet sep b a = AlaSet { getAlaSet :: S.Set a }

alaSet :: sep -> S.Set a -> AlaSet sep (Identity a) a
alaSet _ = AlaSet

-- | More general version of 'alaSet'.
alaSet' :: sep -> (a -> b) -> S.Set a -> AlaSet sep b a
alaSet' _ _ = AlaSet

instance C.Newtype (AlaSet sep wrapper a) (S.Set a) where
    pack = AlaSet
    unpack = getAlaSet

instance (C.Newtype b a, Ord a, Sep sep, C.Parsec b) => C.Parsec (AlaSet sep b a) where
    parsec   = C.pack . S.fromList . map (C.unpack :: b -> a) <$> parseSep (Proxy :: Proxy sep) C.parsec

instance (C.Newtype b a, Sep sep, C.Pretty b) => C.Pretty (AlaSet sep b a) where
    pretty = prettySep (Proxy :: Proxy sep) . map (C.pretty . (C.pack :: a -> b)) . S.toList . C.unpack

-------------------------------------------------------------------------------
-- From Cabal
-------------------------------------------------------------------------------

class    Sep sep  where
    prettySep :: Proxy sep -> [PP.Doc] -> PP.Doc
    parseSep :: C.CabalParsing m => Proxy sep -> m a -> m [a]

instance Sep C.CommaVCat where
    prettySep  _ = PP.vcat . PP.punctuate PP.comma
    parseSep   _ = C.parsecLeadingCommaList
instance Sep C.CommaFSep where
    prettySep _ = PP.fsep . PP.punctuate PP.comma
    parseSep   _ = C.parsecLeadingCommaList
instance Sep C.NoCommaFSep where
    prettySep _   = PP.fsep
    parseSep  _ p = C.many (p <* C.spaces)
