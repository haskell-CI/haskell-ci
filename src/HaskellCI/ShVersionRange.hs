module HaskellCI.ShVersionRange (
    compilerVersionPredicate,
    ) where

import HaskellCI.Prelude

import qualified Data.Set             as S
import qualified Distribution.Version as C

import HaskellCI.Compiler

compilerVersionPredicate :: Set CompilerVersion -> CompilerRange -> String
compilerVersionPredicate cvs cr
    | S.null ghcjsS                         = ghcString
    | S.null ghcjsS'                        = "! $GHCJS && { " ++ ghcString ++ "; }"
    | all (`C.withinRange` ghcRange) ghcjsS = ghcString
    -- TODO: This assumes there's only one GHCJS version :)
    | otherwise = "$GHCJS || { " ++ ghcString ++ "; }"
  where
    R hdS ghcS ghcjsS = partitionCompilerVersions cvs
    R hdR ghcR ghcjsR = simplifyCompilerRange cr

    ghcjsS' = S.filter (`C.withinRange` ghcjsR) ghcjsS

    -- GHC + GHC HEAD

    ghcString = ghcVersionPredicate (ghcHeadRange \/ ghcRange)

    -- GHC

    ghcS' = S.filter (`C.withinRange` ghcR) ghcS

    findGhc :: Version -> VersionRange
    findGhc v = case (S.lookupLE v ghcS, S.lookupGT v ghcS) of
        (Nothing, _)      -> C.noVersion
        (Just u, Nothing) -> C.thisVersion u
        (Just u, Just w)  -> C.orLaterVersion u /\ C.earlierVersion w

    ghcRange :: VersionRange
    ghcRange = case S.toList ghcS of
        []                         -> C.noVersion
        [v] | C.withinRange v ghcR -> C.thisVersion v
            | otherwise            -> C.noVersion
        _                          -> foldr (\/) C.noVersion $ map findGhc $ S.toList ghcS'

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

ghcVersionPredicate :: C.VersionRange -> String
ghcVersionPredicate vr
    | equivVersionRanges C.noVersion vr  = "false"
    | equivVersionRanges C.anyVersion vr = "true"
    | otherwise                          = ghcVersionPredicate' vr

ghcVersionPredicate' :: C.VersionRange -> String
ghcVersionPredicate' = conj . C.asVersionIntervals
  where
    conj = intercalate "  ||  " . map disj

    disj :: C.VersionInterval -> String
    disj (C.LowerBound v C.InclusiveBound, C.UpperBound u C.InclusiveBound)
        | v == u                = "[ $HCNUMVER -eq " ++ f v ++ " ]"
    disj (lb, C.NoUpperBound)
        | isInclZero lb         = "true"
        | otherwise             = lower lb
    disj (lb, C.UpperBound v b)
        | isInclZero lb         = upper v b
        | otherwise             = lower lb ++ " && " ++ upper v b

    isInclZero (C.LowerBound v C.InclusiveBound) = v == C.mkVersion [0]
    isInclZero (C.LowerBound _ C.ExclusiveBound) = False

    lower (C.LowerBound v C.InclusiveBound) = "[ $HCNUMVER -ge " ++ f v ++ " ]"
    lower (C.LowerBound v C.ExclusiveBound) = "[ $HCNUMVER -gt " ++ f v ++ " ]"

    upper v C.InclusiveBound = "[ $HCNUMVER -le " ++ f v ++ " ]"
    upper v C.ExclusiveBound = "[ $HCNUMVER -lt " ++ f v ++ " ]"

    f = ghcVersionToString

ghcVersionToString :: C.Version -> String
ghcVersionToString v =  case C.versionNumbers v of
    []        -> "0"
    [x]       -> show (x * 10000)
    [x,y]     -> show (x * 10000 + y * 100)
    (x:y:z:_) -> show (x * 10000 + y * 100 + z)
