module HaskellCI.Config.Installed where

import HaskellCI.Prelude

import qualified Data.Set                        as S
import qualified Distribution.Compat.CharParsing as C
import qualified Distribution.Parsec             as C
import qualified Distribution.Pretty             as C
import qualified Distribution.Types.PackageName  as C
import qualified Text.PrettyPrint                as PP

-------------------------------------------------------------------------------
-- Single action
-------------------------------------------------------------------------------

data Installed
    = InstalledAll
    | InstalledNone
    | Add C.PackageName
    | Remove C.PackageName
  deriving (Eq, Show)

instance C.Pretty Installed where
    pretty InstalledAll  = PP.text "+all"
    pretty InstalledNone = PP.text "-all"
    pretty (Add pn)      = PP.char '+' PP.<> C.pretty pn
    pretty (Remove pn)   = PP.char '-' PP.<> C.pretty pn

instance C.Parsec Installed where
    parsec = do
        s <- True <$ C.char '+' <|> False <$ C.char '-'
        pn <- C.parsec
        return $ case (s, pn == C.mkPackageName "all") of
            (True,  True)  -> InstalledAll
            (True,  False) -> Add pn
            (False, True)  -> InstalledNone
            (False, False) -> Remove pn

-------------------------------------------------------------------------------
-- Normalised
-------------------------------------------------------------------------------

data InstalledNormalised
    = InstalledDiff (S.Set C.PackageName)
    | InstalledOnly (S.Set C.PackageName)
  deriving Show

-- | Normalised
--
-- >>> parseI = maybe (error "foo") id . traverse C.simpleParsec
-- >>> normaliseInstalled $ parseI ["-Cabal"]
-- InstalledDiff (fromList [PackageName "Cabal"])
--
-- >>> normaliseInstalled $ parseI ["-all"]
-- InstalledOnly (fromList [])
--
-- >>> normaliseInstalled $ parseI ["-all", "+transformers"]
-- InstalledOnly (fromList [PackageName "transformers"])
--
normaliseInstalled :: [Installed] -> InstalledNormalised
normaliseInstalled = foldl' f (InstalledDiff S.empty) where
    f :: InstalledNormalised -> Installed -> InstalledNormalised
    f _ InstalledNone = InstalledOnly S.empty
    f _ InstalledAll  = InstalledDiff S.empty

    f (InstalledDiff s) (Remove p) = InstalledDiff (S.insert p s)
    f (InstalledDiff s) (Add p)    = InstalledDiff (S.delete p s)

    f (InstalledOnly s) (Remove p) = InstalledOnly (S.delete p s)
    f (InstalledOnly s) (Add p)    = InstalledOnly (S.insert p s)
