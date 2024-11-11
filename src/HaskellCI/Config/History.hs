{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
module HaskellCI.Config.History (
    configHistory,
    defaultConfig,
) where

import HaskellCI.Prelude

import qualified Distribution.Version as C

import HaskellCI.Config.Initial
import HaskellCI.Config.Type
import HaskellCI.Config.Ubuntu

import HaskellCI.Compiler (invertVersionRange)

ghcupNormalRange :: VersionRange
ghcupNormalRange = C.laterVersion (mkVersion [8,4,4])

configHistory :: [([Int], Config -> Config)]
configHistory =
    [ ver 0 19 20240414 := \cfg -> cfg
        & field @"cfgDocspec" . field @"cfgDocspecUrl"  .~ "https://github.com/phadej/cabal-extras/releases/download/cabal-docspec-0.0.0.20240414/cabal-docspec-0.0.0.20240414-x86_64-linux.xz"
        & field @"cfgDocspec" . field @"cfgDocspecHash" .~ "2d18a3f79619e8ec5f11870f926f6dc2616e02a6c889315b7f82044b95a1adb9"
    , ver 0 19 20240420 := \cfg -> cfg
        & field @"cfgUbuntu" .~ Jammy
    , ver 0 19 20240513 := \cfg -> cfg
        -- defaultHeadHackage = C.orLaterVersion (C.mkVersion [9,11])
    , ver 0 19 20240702 := \cfg -> cfg
        & field @"cfgCabalInstallVersion" ?~ C.mkVersion [3,12,1,0]
        & field @"cfgDocspec" . field @"cfgDocspecUrl"  .~ "https://github.com/phadej/cabal-extras/releases/download/cabal-docspec-0.0.0.20240703/cabal-docspec-0.0.0.20240703-x86_64-linux.xz"
        & field @"cfgDocspec" . field @"cfgDocspecHash" .~ "48bf3b7fd2f7f0caa6162afee57a755be8523e7f467b694900eb420f5f9a7b76"
    , ver 0 19 20240708 := \cfg -> cfg
        & field @"cfgGhcupVersion" .~ C.mkVersion [0,1,30,0]
    , ver 0 19 20241111 := \cfg -> cfg
        & field @"cfgHvrPpaJobs"          .~ C.earlierVersion (C.mkVersion [8,4])
        & field @"cfgGhcupJobs"           .~ C.simplifyVersionRange (C.intersectVersionRanges (C.intersectVersionRanges ghcupNormalRange (C.earlierVersion (C.mkVersion [9,12,0]))) (invertVersionRange (C.withinVersion (C.mkVersion [9,8,3]))))
        & field @"cfgGhcupVanillaJobs"    .~ C.withinVersion (C.mkVersion [9,8,3])
        & field @"cfgGhcupPrereleaseJobs" .~ C.orLaterVersion (C.mkVersion [9,12,0])
    ]
  where
    ver x y z = [x, y, z]

-- staticHistory [ ver 0 19 20240702 := add GHC-9.6.6

defaultConfig :: Config
defaultConfig = foldl' f initialConfig configHistory
  where
    f !cfg (_, g) = g cfg
