module HaskellCI.TestedWith (
    TestedWithJobs (..),
    CheckVersionsResult(..),
    checkVersions,
    ) where

import Prelude ()
import Prelude.Compat

import Control.Applicative  ((<|>))
import Data.List            (intercalate)

import qualified Data.Foldable                   as F
import qualified Data.Set                        as S
import qualified Distribution.Compat.CharParsing as C
import qualified Distribution.Parsec             as C
import qualified Distribution.Pretty             as C
import qualified Text.PrettyPrint                as PP

import Cabal.Project
import HaskellCI.Compiler
import HaskellCI.Package

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

-- | Result of `checkVersions`.
data CheckVersionsResult a b
  = CheckVersionsSucceeded
    { compilerVersions :: S.Set CompilerVersion
    , cvProject        :: Project a b Package
    }
  | CheckVersionsFailed
    { cvErrors :: [String]
    , cvInfos  :: [String]
        -- ^ Additional hints to the user.  @null@ if @null . cvErrors@.
    }

-------------------------------------------------------------------------------
-- Selection
-------------------------------------------------------------------------------

checkVersions
    :: TestedWithJobs
    -> Project a b Package
    -> CheckVersionsResult a b
checkVersions TestedWithUniform = checkVersionsUniform
checkVersions TestedWithAny     = checkVersionsAny

checkVersionsUniform
    :: Project a b Package
    -> CheckVersionsResult a b
checkVersionsUniform prj | null (prjPackages prj) =
    CheckVersionsFailed ["Error reading cabal file(s)!"] []
checkVersionsUniform prj = do
    let (errors, names) = F.foldl' collectConfig mempty prj
    if not (null errors)
    then CheckVersionsFailed errors ["Consider option --jobs-selection any"]
    else CheckVersionsSucceeded allVersions prj{ prjPackages = names, prjOptPackages = [] }
  where
    allVersions :: S.Set CompilerVersion
    allVersions = F.foldMap pkgJobs prj

    collectConfig
        :: ([String], [Package])
        -> Package
        -> ([String], [Package])
    collectConfig aggregate pkg =
        aggregate <> (errors, [pkg])
      where
        testWith = pkgJobs pkg
        symDiff a b = S.union a b `S.difference` S.intersection a b
        diff = symDiff testWith allVersions
        missingVersions = map dispGhcVersion $ S.toList diff
        errors | S.null diff = []
               | otherwise = pure $ mconcat
                    [ pkgName pkg
                    , " is missing tested-with annotations for: "
                    ] ++ intercalate "," missingVersions

checkVersionsAny
    :: Project a b Package
    -> CheckVersionsResult a b
checkVersionsAny prj | null (prjPackages prj) =
    CheckVersionsFailed ["Error reading cabal file(s)!"] []
checkVersionsAny prj =
    CheckVersionsSucceeded allVersions prj
  where
    allVersions :: S.Set CompilerVersion
    allVersions = F.foldMap pkgJobs prj
