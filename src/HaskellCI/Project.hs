{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
-- | Handling of @cabal.project@ file
module HaskellCI.Project where

import           Control.Applicative             (liftA2)
import           Data.ByteString                 (ByteString)
import           Data.Char                       (isSpace)
import           Data.Coerce                     (coerce)
import           Data.Generics.Labels            ()
import           GHC.Generics                    (Generic)

import qualified Data.Map.Strict                 as M
import qualified Distribution.CabalSpecVersion   as C
import qualified Distribution.Compat.CharParsing as C
import qualified Distribution.Compat.Newtype     as C
import qualified Distribution.FieldGrammar       as C
import qualified Distribution.Parsec.Class       as C
import qualified Distribution.Parsec.Common      as C
import qualified Distribution.Parsec.Newtypes    as C
import qualified Distribution.Parsec.Parser      as C
import qualified Distribution.Parsec.ParseResult as C
import qualified Distribution.Pretty             as C
import qualified Text.PrettyPrint                as PP

-- $setup
-- >>> :seti -XOverloadedStrings

data Project a = Project
    { prjPackages    :: [a]
    , prjConstraints :: [String]
    , prjAllowNewer  :: [String]
    }
  deriving (Show, Functor, Foldable, Traversable, Generic)

emptyProject :: Project [a]
emptyProject = Project [] [] []

-- | Parse project file. Extracts only few fields.
--
-- >>> fmap prjPackages $ parseProjectFile "cabal.project" "packages: foo bar/*.cabal"
-- Right ["foo","bar/*.cabal"]
--
parseProjectFile :: FilePath -> ByteString -> Either String (Project String)
parseProjectFile fp bs = do
    fields0 <- either (Left . show) Right $ C.readFields bs
    let fields1 = fst $ C.partitionFields fields0
    let fields2 = M.filterWithKey (\k _ -> k `elem` knownFields) fields1
    case C.runParseResult $ C.parseFieldGrammar C.cabalSpecLatest fields2 grammar of
        (_, Right x)        -> return x
        (_, Left (_, errs)) -> Left $ unlines $ map (C.showPError fp) errs
  where
    knownFields = C.fieldGrammarKnownFieldList grammar

grammar :: C.ParsecFieldGrammar (Project String) (Project String)
grammar = Project
    <$> C.monoidalFieldAla "packages"    (C.alaList' C.FSep PackageLocation) #prjPackages
    <*> C.monoidalFieldAla "constraints" (C.alaList' C.CommaVCat NoCommas)   #prjConstraints
    <*> C.monoidalFieldAla "allow-newer" (C.alaList' C.CommaVCat NoCommas)    #prjAllowNewer

-------------------------------------------------------------------------------
-- newtypes
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

-- lousy parsing
newtype NoCommas = NoCommas String

instance C.Newtype NoCommas [Char] where
    pack = coerce
    unpack = coerce

instance C.Parsec NoCommas where
    parsec = NoCommas <$> liftA2 (:) (C.satisfy (not . isSpace)) (C.munch (/= ','))

instance C.Pretty NoCommas where
    pretty (NoCommas p) = PP.text p
