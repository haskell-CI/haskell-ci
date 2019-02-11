{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
-- | Handling of @cabal.project@ file
module HaskellCI.Project where

import           Data.ByteString                 (ByteString)
import           Data.Generics.Labels            ()
import           GHC.Generics                    (Generic)

import qualified Data.Map.Strict                 as M
import qualified Distribution.CabalSpecVersion   as C
import qualified Distribution.FieldGrammar       as C
import qualified Distribution.Parsec.Newtypes    as C
import qualified Distribution.Parsec.Parser      as C
import qualified Distribution.Parsec.ParseResult as C

import           HaskellCI.Newtypes
import           HaskellCI.ParsecError

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
        (_, Right x)       -> return x
        (ws, Left (_, es)) -> Left $ renderParseError fp bs es ws
  where
    knownFields = C.fieldGrammarKnownFieldList grammar

grammar :: C.ParsecFieldGrammar (Project String) (Project String)
grammar = Project
    <$> C.monoidalFieldAla "packages"    (C.alaList' C.FSep PackageLocation) #prjPackages
    <*> C.monoidalFieldAla "constraints" (C.alaList' C.CommaVCat NoCommas)   #prjConstraints
    <*> C.monoidalFieldAla "allow-newer" (C.alaList' C.CommaVCat NoCommas)    #prjAllowNewer
