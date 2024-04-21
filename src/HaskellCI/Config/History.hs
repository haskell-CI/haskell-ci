{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
module HaskellCI.Config.History where

import HaskellCI.Config.Initial
import HaskellCI.Config.Type
import HaskellCI.Config.Ubuntu
import HaskellCI.Prelude

configHistory :: [([Int], Config -> Config)]
configHistory =
    [ ver 0 19 20240414 := \cfg -> cfg
        -- https://github.com/haskell-CI/haskell-ci/pull/713/files (docspec)
    , ver 0 19 20240420 := \cfg -> cfg
        & field @"cfgUbuntu" .~ Jammy
    , ver 0 19 20240513 := \cfg -> cfg
        -- defaultHeadHackage = C.orLaterVersion (C.mkVersion [9,11])
    ]
  where
    ver x y z = [x, y, z]

defaultConfig :: Config
defaultConfig = foldl' f initialConfig configHistory
  where
    f !cfg (_, g) = g cfg
