{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module HaskellCI.Config.Folds where

import HaskellCI.Prelude

import qualified Data.Map.Strict                 as M
import qualified Data.Set                        as S
import qualified Distribution.Compat.CharParsing as C
import qualified Distribution.Compat.Newtype     as C
import qualified Distribution.Parsec             as C
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
  deriving anyclass (C.Newtype (S.Set Fold))

instance C.Parsec Folds where
    parsec = fmap (Folds . S.unions) $ manySpaces $ do
        t <- C.parsecToken'
        case t of
            "all"          -> return $ S.fromList possibleFolds
            "all-but-test" -> return $  S.delete FoldTest $ S.fromList possibleFolds
            n -> case M.lookup n ps of
                Just n' -> return $ S.singleton n'
                Nothing -> fail $ "Illegal fold name: " ++ n
      where
        ps = M.fromList $ map (\x -> (showFold x, x)) possibleFolds
        manySpaces p = C.many (p <* C.spaces)

instance C.Pretty Folds where
    pretty = PP.fsep . map (PP.text . showFold) . S.toList . getFolds
