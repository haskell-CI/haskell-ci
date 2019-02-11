{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module HaskellCI.Config.Folds where

import           Data.Char                       (isUpper, toLower)
import           Data.Coerce                     (coerce)
import           Data.List                       (intercalate)

import qualified Data.Map.Strict                 as M
import qualified Data.Set                        as S
import qualified Distribution.Compat.Newtype     as C
import qualified Distribution.Parsec.Class       as C
import qualified Distribution.Pretty             as C
import qualified Text.PrettyPrint                as PP

data Fold
    = FoldSDist
    | FoldUnpack
    | FoldBuild
    | FoldBuildInstalled
    | FoldBuildEverything
    | FoldTest
    | FoldHaddock
    | FoldStackage
    | FoldCheck
    | FoldDoctest
    | FoldHLint
    | FoldConstraintSets
  deriving (Eq, Ord, Show, Enum, Bounded)

-------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------

showFold :: Fold -> String
showFold = dashise . drop 4 . show
  where
    dashise = intercalate "-" . map (map toLower) . split

    split [] = []
    split xs0 =
        let (ys, xs1) = span isUpper xs0
            (zs, xs2) = break isUpper xs1
        in (ys ++ zs) : split xs2

possibleFolds :: [Fold]
possibleFolds = [minBound .. maxBound]

-------------------------------------------------------------------------------
-- Folds
-------------------------------------------------------------------------------

newtype Folds = Folds { getFolds :: S.Set Fold }

instance C.Newtype Folds (S.Set Fold) where
    pack = coerce
    unpack = coerce

instance C.Parsec Folds where
    parsec = do
        t <- C.parsecToken'
        case t of
            "all"          -> return $ Folds $ S.fromList possibleFolds
            "all-but-test" -> return $ Folds $ S.delete FoldTest $ S.fromList possibleFolds
            n -> case M.lookup n ps of
                Just n' -> return $ Folds $ S.singleton n'
                Nothing -> fail $ "Illegal fold name: " ++ n
      where
        ps = M.fromList $ map (\x -> (showFold x, x)) possibleFolds

instance C.Pretty Folds where
    pretty = PP.fsep . map (PP.text . showFold) . S.toList . getFolds
