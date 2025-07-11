{-# LANGUAGE UndecidableInstances #-}
module Main (main) where

import Data.TreeDiff
import Data.TreeDiff.Golden       (ediffGolden)
import System.FilePath            ((-<.>), (</>))
import Test.Tasty                 (TestName, TestTree, defaultMain, testGroup)
import Test.Tasty.Golden.Advanced (goldenTest)
import Text.PrettyPrint           (Doc, render)

import qualified Data.ByteString as BS
import qualified Data.TreeDiff.OMap as OMap

import Distribution.Fields           (PrettyField (..))
import Distribution.Types.SourceRepo (KnownRepoType, RepoKind, RepoType, SourceRepo)

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

instance (ToExpr uri, ToExpr opt, ToExpr pkg) => ToExpr (Project uri opt pkg) where
    toExpr prj = Rec "Project" $ OMap.fromList
        [ field "prjPackages"     prjPackages
        , field "prjOptPackages"  prjOptPackages
        , field "prjUriPackages"  prjUriPackages
        , field "prjConstraints"  prjConstraints
        , field "prjAllowNewer"   prjAllowNewer
        , field "prjReorderGoals" prjReorderGoals
        , field "prjMaxBackjumps" prjMaxBackjumps
        , field "prjOptimization" prjOptimization
        , field "prjSourceRepos"  prjSourceRepos
        , field "prjOtherFields"  prjOtherFields
        ]
      where
        field name f = (name, toExpr (f prj))

instance ToExpr Optimization

instance ToExpr SourceRepo
instance ToExpr RepoKind
instance ToExpr RepoType
instance ToExpr KnownRepoType
instance ToExpr (f FilePath) => ToExpr (SourceRepositoryPackage f)

instance ToExpr Doc where
  toExpr = toExpr . render

instance ToExpr (PrettyField ann) where
  toExpr (PrettyField _ fn d)       = App "PrettyField"   [toExpr fn, toExpr d]
  toExpr (PrettySection _ fn ds ps) = App "PrettySection" [toExpr fn, toExpr ds, toExpr ps]
  toExpr PrettyEmpty                = App "PrettyEmpty"   []
