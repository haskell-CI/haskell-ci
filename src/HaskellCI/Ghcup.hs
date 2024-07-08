module HaskellCI.Ghcup where

import HaskellCI.Prelude

import qualified Distribution.Version as C

initialGhcupVersion :: Version
initialGhcupVersion = C.mkVersion [0,1,20,0]
