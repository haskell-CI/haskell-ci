-- | This module encodes what we know about GHC, including existing/supported versions.
module HaskellCI.GHC where

import           Distribution.Version (Version, mkVersion, versionNumbers)

import qualified Distribution.Pretty             as C

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
    , [8,6,1],  [8,6,2], [8,6,3], [8,6,4]
    ]

ghcAlpha :: Maybe Version
ghcAlpha = Nothing
-- ghcAlpha = Just (mkVersion [8,8,1])
--
dispGhcVersion :: Maybe Version -> String
dispGhcVersion = maybe "head" C.prettyShow

-- | Alphas, RCs and HEAD.
previewGHC :: Maybe Version -> Bool
previewGHC = maybe True $ \v -> Just v == ghcAlpha || odd (snd (ghcMajVer v))

ghcMajVer :: Version -> (Int,Int)
ghcMajVer v
    | x:y:_ <- versionNumbers v = (x,y)
    | otherwise = error $ "panic: ghcMajVer called with " ++ show v
