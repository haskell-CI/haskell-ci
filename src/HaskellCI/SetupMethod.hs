{-# LANGUAGE OverloadedStrings #-}
module HaskellCI.SetupMethod (
    SetupMethod (..),
    PerSetupMethod (..),
) where

import HaskellCI.Prelude
import HaskellCI.YamlSyntax

data SetupMethod = HVRPPA | GHCUP | GHCUPvanilla | GHCUPprerelease
  deriving (Eq, Ord, Enum, Bounded, Show)

instance ToYaml SetupMethod where
    toYaml HVRPPA          = "hvr-ppa"
    toYaml GHCUP           = "ghcup"
    toYaml GHCUPvanilla    = "ghcup-vanilla"
    toYaml GHCUPprerelease = "ghcup-prerelease"

data PerSetupMethod a = PerSetupMethod
    { hvrPpa          :: a
    , ghcup           :: a
    , ghcupVanilla    :: a
    , ghcupPrerelease :: a
    }
  deriving (Show, Functor, Foldable, Traversable, Generic, Binary)

instance Representable SetupMethod PerSetupMethod where
    index f HVRPPA          = hvrPpa f
    index f GHCUP           = ghcup f
    index f GHCUPvanilla    = ghcupVanilla f
    index f GHCUPprerelease = ghcupPrerelease f

    tabulate f = PerSetupMethod
        { hvrPpa          = f HVRPPA
        , ghcup           = f GHCUP
        , ghcupVanilla    = f GHCUPvanilla
        , ghcupPrerelease = f GHCUPprerelease
        }
