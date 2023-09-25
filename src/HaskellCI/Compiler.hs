{-# OPTIONS_GHC -Wno-deprecations #-}
-- | This module encodes what we know about GHC, including existing/supported versions.
module HaskellCI.Compiler (
    -- * Compiler version
    CompilerVersion (..),
    maybeGHC,
    isGHCJS,
    maybeGHCJS,
    previewGHC,
    -- ** Selectors
    compilerKind,
    compilerVersion,
    -- * Compiler version range
    CompilerRange (..),
    compilerWithinRange,
    invertCompilerRange,
    -- * Known versions
    knownGhcVersions,
    knownGhcjsVersions,
    -- * Showing
    dispGhcVersion,
    dispGhcVersionShort,
    dispCabalVersion,
    -- * Cabal version
    correspondingCabalVersion,
    previewCabal,
    -- * Misc
    ghcMajVer,
    translateCompilerVersion,
    ) where

import HaskellCI.Prelude

import Distribution.Types.VersionInterval.Legacy (fromVersionIntervals, invertVersionIntervals, toVersionIntervals)
import Distribution.Version                      (hasUpperBound, versionNumbers, withinRange)

import qualified Data.Set            as S
import qualified Distribution.Pretty as C

-------------------------------------------------------------------------------
-- CompilerVersion
-------------------------------------------------------------------------------

data CompilerVersion
    = GHCHead
    | GHC Version
    | GHCJS Version
  deriving (Eq, Ord, Show)

maybeGHC :: a -> (Version -> a) -> CompilerVersion -> a
maybeGHC _ f (GHC v) = f v
maybeGHC x _ _       = x

isGHCJS :: CompilerVersion -> Bool
isGHCJS (GHCJS _) = True
isGHCJS _         = False

maybeGHCJS :: CompilerVersion -> Maybe Version
maybeGHCJS (GHCJS v) = Just v
maybeGHCJS _         = Nothing

-------------------------------------------------------------------------------
-- String selectors
-------------------------------------------------------------------------------

compilerKind :: CompilerVersion -> String
compilerKind GHCHead   = "ghc"
compilerKind (GHC _)   = "ghc"
compilerKind (GHCJS _) = "ghcjs"

compilerVersion :: CompilerVersion -> String
compilerVersion GHCHead   = "head"
compilerVersion (GHC v)   = C.prettyShow v
compilerVersion (GHCJS v) = C.prettyShow v

-------------------------------------------------------------------------------
-- CompilerRange
-------------------------------------------------------------------------------

data CompilerRange
    = Range VersionRange
    | RangeGHC
    | RangeGHCJS
    | RangePoints (Set CompilerVersion)
    | RangeInter CompilerRange CompilerRange
    | RangeUnion CompilerRange CompilerRange
  deriving (Show)

instance Lattice CompilerRange where
    (/\) = RangeInter
    (\/) = RangeUnion

instance BoundedJoinSemiLattice CompilerRange where
    bottom = RangePoints S.empty

instance BoundedMeetSemiLattice CompilerRange where
    top = RangePoints allCompilerVersions

compilerWithinRange :: CompilerVersion -> CompilerRange -> Bool
compilerWithinRange v         (RangeInter a b) = compilerWithinRange v a /\ compilerWithinRange v b
compilerWithinRange v         (RangeUnion a b) = compilerWithinRange v a \/ compilerWithinRange v b
compilerWithinRange (GHC v)   (Range vr)       = withinRange v vr
compilerWithinRange (GHCJS v) (Range vr)       = withinRange v vr
compilerWithinRange GHCHead   (Range vr)       = not (hasUpperBound vr)
compilerWithinRange (GHC _)   RangeGHC         = True
compilerWithinRange GHCHead   RangeGHC         = True
compilerWithinRange (GHCJS _) RangeGHC         = False
compilerWithinRange (GHC _)   RangeGHCJS       = False
compilerWithinRange GHCHead   RangeGHCJS       = False
compilerWithinRange (GHCJS _) RangeGHCJS       = True
compilerWithinRange v         (RangePoints vs) = S.member v vs

invertCompilerRange :: CompilerRange -> CompilerRange
invertCompilerRange (Range vr)       = Range (invertVersionRange vr)
invertCompilerRange RangeGHC         = RangeGHCJS
invertCompilerRange RangeGHCJS       = RangeGHC
invertCompilerRange (RangeInter a b) = RangeUnion (invertCompilerRange a) (invertCompilerRange b)
invertCompilerRange (RangeUnion a b) = RangeInter (invertCompilerRange a) (invertCompilerRange b)
invertCompilerRange (RangePoints vs) = RangePoints (S.difference allCompilerVersions vs)

invertVersionRange :: VersionRange -> VersionRange
invertVersionRange = fromVersionIntervals . invertVersionIntervals . toVersionIntervals

-------------------------------------------------------------------------------
-- Known versions
-------------------------------------------------------------------------------

knownGhcVersions :: [Version]
knownGhcVersions = fmap mkVersion
    [ [7,0,1],  [7,0,2], [7,0,3], [7,0,4]
    , [7,2,1],  [7,2,2]
    , [7,4,1],  [7,4,2]
    , [7,6,1],  [7,6,2], [7,6,3]
    , [7,8,1],  [7,8,2], [7,8,3], [7,8,4]
    , [7,10,1], [7,10,2], [7,10,3]
    , [8,0,1],  [8,0,2]
    , [8,2,1],  [8,2,2]
    , [8,4,1],  [8,4,2], [8,4,3], [8,4,4]
    , [8,6,1],  [8,6,2], [8,6,3], [8,6,4], [8,6,5]
    , [8,8,1],  [8,8,2], [8,8,3], [8,8,4]
    , [8,10,1], [8,10,2], [8,10,3], [8,10,4], [8,10,5], [8,10,6], [8,10,7]
    , [9,0,1],  [9,0,2]
    , [9,2,1],  [9,2,2],  [9,2,3], [9,2,4], [9,2,5], [9,2,6], [9,2,7], [9,2,8]
    , [9,4,1],  [9,4,2],  [9,4,3], [9,4,4], [9,4,5], [9,4,6], [9,4,7]
    , [9,6,1],  [9,6,2],  [9,6,3]
    , [9,8,1]
    ]

knownGhcjsVersions :: [Version]
knownGhcjsVersions = fmap mkVersion
    [ [8,4]
    ]

allCompilerVersions :: Set CompilerVersion
allCompilerVersions = S.insert GHCHead $ S.fromList $
    [ GHC v | v <- knownGhcVersions ] ++
    [ GHCJS v | v <- knownGhcjsVersions ]

-------------------------------------------------------------------------------
-- Combinators
-------------------------------------------------------------------------------

-- Used by travis only?
correspondingCabalVersion
    :: Maybe Version    -- ^ Preferred Cabal Version
    -> CompilerVersion  -- ^ GHC Version
    -> Maybe Version
correspondingCabalVersion Nothing   _         = Nothing
correspondingCabalVersion (Just _)  GHCHead   = Nothing
correspondingCabalVersion (Just _)  (GHCJS _) = Just (mkVersion [3,4])
correspondingCabalVersion (Just cv) (GHC gv)
    | gv >= mkVersion [8,10] = Just $ max (mkVersion [3,2]) cv
    | otherwise              = Just $ max (mkVersion [3,0]) cv

dispGhcVersion :: CompilerVersion -> String
dispGhcVersion GHCHead   = "ghc-head"
dispGhcVersion (GHC v)   = "ghc-" ++ C.prettyShow v
dispGhcVersion (GHCJS v) = "ghcjs-" ++ C.prettyShow v

dispGhcVersionShort :: CompilerVersion -> String
dispGhcVersionShort GHCHead   = "ghc-head"
dispGhcVersionShort (GHC v)   = C.prettyShow v
dispGhcVersionShort (GHCJS v) = "ghcjs-" ++ C.prettyShow v

dispCabalVersion :: Maybe Version -> String
dispCabalVersion = maybe "head" C.prettyShow

ghcAlpha :: Maybe (Version, Version)
ghcAlpha = Just (mkVersion [9,8,1], mkVersion [9,8,0,20230919])

-- | Alphas, RCs and HEAD.
previewGHC
    :: VersionRange     -- ^ head.hackage range
    -> CompilerVersion
    -> Bool
previewGHC _vr GHCHead   = True
previewGHC  vr (GHC v)   = withinRange v vr || odd (snd (ghcMajVer v)) || maybe False (\(v', _) -> v >= v') ghcAlpha
previewGHC _vr (GHCJS _) = False

previewCabal
    :: Maybe Version
    -> Bool
previewCabal Nothing = True
previewCabal (Just v) = case versionNumbers v of
    _:y:_ -> odd y
    _     -> False

ghcMajVer :: Version -> (Int,Int)
ghcMajVer v
    | x:y:_ <- versionNumbers v = (x,y)
    | otherwise = error $ "panic: ghcMajVer called with " ++ show v

-- | Map compiler version to one available to download.
--
-- This way we can map e.g. 9.4.1 to 9.4.0.20220501 i.e. a prerelease.
translateCompilerVersion :: CompilerVersion -> CompilerVersion
translateCompilerVersion (GHC v)
    | Just (u, w) <- ghcAlpha
    , v == u
    = GHC w
translateCompilerVersion v = v
