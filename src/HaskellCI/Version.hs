module HaskellCI.Version where

import           Data.Function                      (on)
import           Data.List                          (intercalate)

import qualified Distribution.Version as C

ghcVersionPredicate :: C.VersionRange -> String
ghcVersionPredicate = conj . C.asVersionIntervals
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

equivVersionRanges :: C.VersionRange -> C.VersionRange -> Bool
equivVersionRanges = on (==) C.simplifyVersionRange
