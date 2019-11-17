module Cabal.Package (
    readPackage,
    parsePackage,
    ) where

import Control.Exception            (throwIO)
import Data.ByteString              (ByteString)

import qualified Data.ByteString                        as BS
import qualified Distribution.Fields                    as C
import qualified Distribution.PackageDescription        as C
import qualified Distribution.PackageDescription.Parsec as C

import Cabal.Parse

-- | High level convinience function to read package definitons, @.cabal@ files.
--
-- May throw 'IOException' when file doesn't exist, and 'ParseError'
-- on parse error.
readPackage :: FilePath -> IO C.GenericPackageDescription
readPackage fp = do
    contents <- BS.readFile fp
    either throwIO return (parsePackage fp contents)

-- | Parse @.cabal@ file.
parsePackage :: FilePath -> ByteString -> Either ParseError C.GenericPackageDescription
parsePackage fp contents = case C.runParseResult $ C.parseGenericPackageDescription contents of
    (ws, Left (_mv, errs)) -> Left $ ParseError fp contents errs ws
    (_, Right gpd)         -> Right gpd
