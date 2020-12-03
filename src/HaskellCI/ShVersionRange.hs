module HaskellCI.ShVersionRange (
    compilerVersionPredicate,
    compilerVersionArithPredicate,
    ) where

import HaskellCI.Prelude

import Algebra.Lattice (joins)
import Algebra.Heyting.Free (Free (..))

import qualified Algebra.Heyting.Free as F
import qualified Data.Set             as S
import qualified Distribution.Version as C

import HaskellCI.Compiler

-- $setup
-- >>> import Distribution.Pretty (prettyShow)

compilerVersionPredicate :: Set CompilerVersion -> CompilerRange -> String
compilerVersionPredicate = compilerVersionPredicateImpl (toTest . freeToArith) where
    toTest expr = "[ " ++ expr ++ " -ne 0 ]"

compilerVersionArithPredicate :: Set CompilerVersion -> CompilerRange -> String
compilerVersionArithPredicate = compilerVersionPredicateImpl freeToArith

compilerVersionPredicateImpl
    :: (Free String -> String)
    -> Set CompilerVersion -> CompilerRange -> String
compilerVersionPredicateImpl conv cvs cr
    | S.null ghcjsS = conv ghcFree
    | otherwise     = conv $
        (Var "GHCJSARITH" /\ ghcjsFree) \/ (Var "! GHCJSARITH" /\ ghcFree)
  where
    R hdS ghcS ghcjsS = partitionCompilerVersions cvs
    R hdR ghcR ghcjsR = simplifyCompilerRange cr

    -- GHCJS

    ghcjsS' = S.filter (`C.withinRange` ghcjsR) ghcjsS

    ghcjsFree :: Free String
    ghcjsFree = ghcVersionPredicate ghcjsRange

    ghcjsRange = case S.toList ghcjsS' of
        []  -> C.noVersion
        [_] -> C.anyVersion
        _   -> error "multiple GHCJS versions unsupported"

    -- GHC + GHC HEAD

    ghcFree :: Free String
    ghcFree = ghcVersionPredicate (ghcHeadRange \/ ghcRange)

    -- GHC

    ghcD = roundDown ghcS
    ghcS' = S.filter (`C.withinRange` ghcR) ghcS

    isMinGHC u = Just u == fmap fst (S.minView ghcD)

    -- if we build with GHC HEAD, than none of known versions is maxGHC.
    isMaxGHC u | hdS       = False
               | otherwise = Just u == fmap fst (S.maxView ghcD)

    findGhc :: Version -> VersionRange
    findGhc v = case (S.lookupLE v ghcD, S.lookupGT v ghcD) of
        (Nothing, _)      -> C.noVersion
        (Just u, Nothing) -> orLater u
        (Just u, Just w)  -> orLater u /\ earlier w
      where
        orLater u | isMinGHC u = C.anyVersion
                  | otherwise  = C.orLaterVersion u

        earlier u | isMaxGHC u = C.anyVersion
                  | otherwise  = C.earlierVersion u


    ghcRange :: VersionRange
    ghcRange = foldr (\/) C.noVersion $ map findGhc $ S.toList ghcS'

    -- GHC HEAD

    ghcHeadRange :: VersionRange
    ghcHeadRange
        | hdR && hdS = C.laterVersion (S.findMax ghcS)
        | otherwise  = C.noVersion

data R a = R Bool a a
  deriving (Show)

partitionCompilerVersions :: Set CompilerVersion -> R (Set Version)
partitionCompilerVersions = foldr f (R False S.empty S.empty) where
    f (GHC v)   (R hd ghc ghcjs) = R hd (S.insert v ghc) ghcjs
    f (GHCJS v) (R hd ghc ghcjs) = R hd ghc (S.insert v ghcjs)
    f GHCHead   (R _ ghc ghcjs)  = R True ghc ghcjs

simplifyCompilerRange :: CompilerRange -> R VersionRange
simplifyCompilerRange RangeGHC   = R True C.anyVersion C.noVersion
simplifyCompilerRange RangeGHCJS = R False C.noVersion C.anyVersion
simplifyCompilerRange (Range vr) = R (not $ C.hasUpperBound vr) vr vr
simplifyCompilerRange (RangeUnion a b) =
    case (simplifyCompilerRange a, simplifyCompilerRange b) of
        (R x y z, R u v w) -> R (x \/ u) (y \/ v) (z \/ w)
simplifyCompilerRange (RangeInter a b) =
    case (simplifyCompilerRange a, simplifyCompilerRange b) of
        (R x y z, R u v w) -> R (x /\ u) (y /\ v) (z /\ w)
simplifyCompilerRange (RangePoints vs) = foldr f (R False C.noVersion C.noVersion) vs where
    f (GHC v)   (R hd ghc ghcjs) = R hd (C.thisVersion v \/ ghc) ghcjs
    f (GHCJS v) (R hd ghc ghcjs) = R hd ghc (C.thisVersion v \/ ghcjs)
    f GHCHead   (R _  ghc ghcjs) = R True ghc ghcjs

ghcVersionPredicate :: C.VersionRange -> Free String
ghcVersionPredicate vr
    | equivVersionRanges C.noVersion vr  = bottom
    | equivVersionRanges C.anyVersion vr = top
    | otherwise                          = ghcVersionPredicate' vr

ghcVersionPredicate' :: C.VersionRange -> Free String
ghcVersionPredicate' = conj . C.asVersionIntervals
  where
    conj = joins . map disj

    disj :: C.VersionInterval -> Free String
    disj (C.LowerBound v C.InclusiveBound, C.UpperBound u C.InclusiveBound)
        | v == u                = Var ("HCNUMVER == " ++ f v)
    disj (lb, C.NoUpperBound)
        | isInclZero lb         = top
        | otherwise             = Var (lower lb)
    disj (lb, C.UpperBound v b)
        | isInclZero lb         = Var (upper v b)
        | otherwise             = Var (lower lb) /\ Var (upper v b)

    isInclZero (C.LowerBound v C.InclusiveBound) = v == C.mkVersion [0]
    isInclZero (C.LowerBound _ C.ExclusiveBound) = False

    lower (C.LowerBound v C.InclusiveBound) = "HCNUMVER >= " ++ f v
    lower (C.LowerBound v C.ExclusiveBound) = "HCNUMVER > " ++ f v

    upper v C.InclusiveBound = "HCNUMVER <= " ++ f v
    upper v C.ExclusiveBound = "HCNUMVER < " ++ f v

    f = ghcVersionToString

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

ghcVersionToString :: C.Version -> String
ghcVersionToString v =  case C.versionNumbers v of
    []        -> "0"
    [x]       -> show (x * 10000)
    [x,y]     -> show (x * 10000 + y * 100)
    (x:y:z:_) -> show (x * 10000 + y * 100 + z)

-- | Round down a first version in major series.
--
-- >>> let rd = map prettyShow . S.toList . roundDown . S.fromList . map C.mkVersion
--
-- >>> rd []
-- []
--
-- >>> rd [ [8,0,2] ]
-- ["8.0","8.0.3"]
--
-- >>> rd [ [8,0,2], [8,2,2], [8,4,4], [8,6,5], [8,8,1] ]
-- ["8.0","8.2","8.4","8.6","8.8","8.8.2"]
--
-- >>> rd [ [8,6,1], [8,6,2], [8,6,3], [8,6,4], [8,6,5] ]
-- ["8.6","8.6.2","8.6.3","8.6.4","8.6.5","8.6.6"]
--
roundDown :: Set Version -> Set Version
roundDown = go S.empty . S.toList where
    go !acc []      = acc
    go !acc [v]
        | S.member m acc = S.insert v $ S.insert (up v) acc
        | otherwise      = S.insert m $ S.insert (up v) acc
      where
        m = let (x,y) = ghcMajVer v in C.mkVersion [x,y]
    go !acc (v:vs)
        | S.member m acc = go (S.insert v acc) vs
        | otherwise      = go (S.insert m acc) vs
      where
        m = let (x,y) = ghcMajVer v in C.mkVersion [x,y]

    up v = C.mkVersion $ case C.versionNumbers v of
        []     -> [1]
        (x:xs) -> up' x xs

    up' x []     = [x + 1]
    up' x (y:ys) = x : up' y ys

-------------------------------------------------------------------------------
-- Arithmetic expression
-------------------------------------------------------------------------------

freeToArith :: Free String -> String
freeToArith z
    | z == top    = "1"
    | z == bottom = "0"
    | otherwise   = "$((" ++ go 0 z ++ "))"
  where
    go :: Int -> Free String -> String
    go _ (Var x)  = x
    go _ F.Bottom = "1"
    go _ F.Top    = "0"

    go d (x :/\: y) = parens (d > 3)
        $ go 4 x ++ " && " ++ go 3 y
    go d (x :\/: y) = parens (d > 2)
        $ go 3 x ++ " || " ++ go 2 y

    go d (x :=>: y) = parens (d > 2)
        $ "! (" ++ go 0 x ++ ") || " ++ go 2 y

    parens :: Bool -> String -> String
    parens True  s = "{ " ++ s ++ "; }"
    parens False s = s

-------------------------------------------------------------------------------
-- PosNeg
-------------------------------------------------------------------------------

{-
data PosNeg a = Pos a | Neg a
  deriving (Eq, Ord, Show, Functor)

neg :: PosNeg a -> PosNeg a
neg (Pos x) = Neg x
neg (Neg x) = Pos x

instance Applicative PosNeg where
    pure  = Pos
    (<*>) = ap

instance Monad PosNeg where
    return = pure

    Pos x >>= f = f x
    Neg x >>= f = neg (f x)
-}
