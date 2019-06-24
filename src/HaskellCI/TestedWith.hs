module HaskellCI.TestedWith (
    TestedWithJobs (..),
    checkVersions,
    ) where

import           Prelude                         ()
import           Prelude.Compat

import           Control.Applicative             ((<|>))
import           Data.Generics.Labels            ()
import           Data.List                       (intercalate)
import           Data.Void                       (Void)

import qualified Data.Foldable                   as F
import qualified Data.Set                        as S
import qualified Distribution.Compat.CharParsing as C
import qualified Distribution.Parsec.Class       as C
import qualified Distribution.Pretty             as C
import qualified Distribution.Types.Version      as C
import qualified Distribution.Types.VersionRange as C
import qualified Text.PrettyPrint                as PP

import           HaskellCI.Package
import           HaskellCI.Project

data TestedWithJobs
    = TestedWithUniform
    | TestedWithAny
  deriving (Eq, Show)

instance C.Parsec TestedWithJobs where
    parsec = TestedWithUniform <$ C.string "uniform"
        <|> TestedWithAny <$ C.string "any"

instance C.Pretty TestedWithJobs where
    pretty TestedWithUniform = PP.text "uniform"
    pretty TestedWithAny     = PP.text "any"

-------------------------------------------------------------------------------
-- Selection
-------------------------------------------------------------------------------

checkVersions
    :: TestedWithJobs
    -> Project Void (Package, S.Set C.Version)
    -> Either [String] (S.Set C.Version, Project Void Package)
checkVersions TestedWithUniform = checkVersionsUniform
checkVersions TestedWithAny     = checkVersionsAny

checkVersionsUniform
    :: Project Void (Package, S.Set C.Version)
    -> Either [String] (S.Set C.Version, Project Void Package)
checkVersionsUniform prj | null (prjPackages prj) = Left ["Error reading cabal file(s)!"]
checkVersionsUniform prj = do
    let (errors, names) = F.foldl' collectConfig mempty prj
    if not (null errors)
    then Left errors
    else Right (allVersions, prj { prjPackages = names, prjOptPackages = [] })
  where
    allVersions = F.foldMap snd prj

    collectConfig
        :: ([String], [Package])
        -> (Package, S.Set C.Version)
        -> ([String], [Package])
    collectConfig aggregate (pkg, testWith) =
        aggregate <> (errors, [pkg])
      where
        symDiff a b = S.union a b `S.difference` S.intersection a b
        diff = symDiff testWith allVersions
        missingVersions = map C.prettyShow $ S.toList diff
        errors | S.null diff = []
               | otherwise = pure $ mconcat
                    [ pkgName pkg
                    , " is missing tested-with annotations for: "
                    ] ++ intercalate "," missingVersions

checkVersionsAny
    :: Project Void (Package, S.Set C.Version)
    -> Either [String] (S.Set C.Version, Project Void Package)
checkVersionsAny prj | null (prjPackages prj) = Left ["Error reading cabal file(s)!"]
checkVersionsAny prj =
    Right (allVersions, uncurry f <$> prj)
  where
    allVersions = F.foldMap snd prj

    f :: Package -> S.Set C.Version -> Package
    f pkg vs = case S.toList vs of
        []      -> pkg { pkgJobs = C.noVersion }
        (v:vs') -> pkg { pkgJobs = foldr g (C.thisVersion v) vs' }

    g = C.unionVersionRanges . C.thisVersion
