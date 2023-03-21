module HaskellCI.Cabal where

import HaskellCI.Prelude

import qualified Distribution.Version as C

defaultCabalInstallVersion :: Maybe Version
defaultCabalInstallVersion = Just (C.mkVersion [3,10])

-- | Convert cabal-install version to a version ghcup understands.
cabalGhcupVersion :: Version -> Version
cabalGhcupVersion ver = case C.versionNumbers ver of
      [3,10] -> C.mkVersion [3,10,1,0]
      [3,9]  -> C.mkVersion [3,9,0,0]
      [3,6]  -> C.mkVersion [3,6,2,0]
      [x,y]  -> C.mkVersion [x,y,0,0]
      _      -> ver
