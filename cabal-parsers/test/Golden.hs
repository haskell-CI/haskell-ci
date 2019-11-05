{-# LANGUAGE UndecidableInstances #-}
module Main (main) where

import Data.TreeDiff
import Data.TreeDiff.Golden       (ediffGolden)
import System.FilePath            ((-<.>), (</>))
import Test.Tasty                 (TestName, TestTree, defaultMain, testGroup)
import Test.Tasty.Golden.Advanced (goldenTest)

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map

import Distribution.Types.SourceRepo (RepoKind, RepoType, SourceRepo)

import Cabal.Optimization
import Cabal.Parse
import Cabal.Project
import Cabal.SourceRepo

main :: IO ()
main = defaultMain $ testGroup "golden"
    [ golden "haskell-ci"
    ]
  where
    golden name = ediffGolden goldenTest name  goldenPath $ do
        contents <- BS.readFile projectPath
        either (fail . renderParseError) return $ parseProject projectPath contents
      where
        goldenPath = "fixtures" </> name -<.> "golden"
        projectPath = "fixtures" </> name -<.> "project"

-------------------------------------------------------------------------------
-- orphans
-------------------------------------------------------------------------------

-- skip orig fields
instance (ToExpr uri, ToExpr opt, ToExpr pkg) => ToExpr (Project uri opt pkg) where
    toExpr prj = Rec "Project" $ Map.fromList
        [ field "prjPackages"     prjPackages
        , field "prjOptPackages"  prjOptPackages
        , field "prjUriPackages"  prjUriPackages
        , field "prjConstraints"  prjConstraints
        , field "prjAllowNewer"   prjAllowNewer
        , field "prjReorderGoals" prjReorderGoals
        , field "prjMaxBackjumps" prjMaxBackjumps
        , field "prjOptimization" prjOptimization
        , field "prjSourceRepos"  prjSourceRepos
        ]
      where
        field name f = (name, toExpr (f prj))

instance ToExpr Optimization

instance ToExpr SourceRepo
instance ToExpr RepoKind
instance ToExpr RepoType
instance ToExpr (f FilePath) => ToExpr (SourceRepositoryPackage f)
