module HaskellCI.Package where

import qualified Distribution.Types.GenericPackageDescription as C
import qualified Distribution.Types.VersionRange              as C

data Package = Pkg
    { pkgName :: String
    , pkgJobs :: C.VersionRange
    , pkgDir  :: FilePath
    , pkgGpd  :: C.GenericPackageDescription
    }
  deriving (Eq, Show)
