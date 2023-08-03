module HaskellCI.Ghcup where

import HaskellCI.Prelude

import qualified Distribution.Version as C

defaultGhcupVersion :: Version
defaultGhcupVersion = C.mkVersion [0,1,19,5]
