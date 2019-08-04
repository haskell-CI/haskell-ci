module HaskellCI.Package where

import HaskellCI.Prelude

import qualified Distribution.Types.GenericPackageDescription as C

import HaskellCI.Compiler

data Package = Pkg
    { pkgName :: String
    , pkgJobs :: Set CompilerVersion
    , pkgDir  :: FilePath
    , pkgGpd  :: C.GenericPackageDescription
    }
  deriving (Eq, Show, Generic)
