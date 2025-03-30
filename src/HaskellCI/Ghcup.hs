module HaskellCI.Ghcup where

import HaskellCI.Prelude

import qualified Distribution.Version as C

initialGhcupVersion :: Version
initialGhcupVersion = C.mkVersion [0,1,20,0]

-- https://github.com/haskell/cabal/issues/10836#issuecomment-2757855218
-- if you interface with GHCup, you SHOULD be able to deal with non-PVP versions.
--
-- Perfectly, we'd have these mappings configurable;
-- but for now they are hardcoded.
--
translateGhcVersion :: Version -> String
translateGhcVersion = prettyShow

translateCabalVersion :: Version -> String
translateCabalVersion x
    | [3,14,1,1] <- C.versionNumbers x
    = "3.14.1.1-p1"

    | otherwise
    = prettyShow x
