module HaskellCI.HeadHackage where

import HaskellCI.Prelude

import qualified Distribution.Version as C

defaultHeadHackage :: VersionRange
defaultHeadHackage = C.orLaterVersion (C.mkVersion [9,13])

headHackageRepoStanza :: Bool -> [String]
headHackageRepoStanza override =
    [ "repository head.hackage.ghc.haskell.org"
    , "   url: https://ghc.gitlab.haskell.org/head.hackage/"
    , "   secure: True"
    , "   root-keys: 7541f32a4ccca4f97aea3b22f5e593ba2c0267546016b992dfadcd2fe944e55d"
    , "              26021a13b401500c8eb2761ca95c61f2d625bfef951b939a8124ed12ecf07329"
    , "              f76d08be13e9a61a377a85e2fb63f4c5435d40f8feb3e12eb05905edb8cdea89"
    , "   key-threshold: 3"
    ] ++
    activeRepositories
  where
    activeRepositories
        | override
        = [ "active-repositories: hackage.haskell.org, head.hackage.ghc.haskell.org:override"
          ]

        | otherwise
        = []
