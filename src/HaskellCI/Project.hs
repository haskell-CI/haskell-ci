-- | Handling of @cabal.project@ file
module HaskellCI.Project where

import GHC.Generics (Generic)

data Project a = Project
    { prjPackages    :: [a]
    , prjConstraints :: Maybe String
    , prjAllowNewer  :: Maybe String
    }
  deriving (Show, Functor, Foldable, Traversable, Generic)

emptyProject :: Project [a]
emptyProject = Project [] Nothing Nothing
