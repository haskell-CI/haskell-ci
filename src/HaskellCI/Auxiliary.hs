{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module HaskellCI.Auxiliary (
    Auxiliary (..),
    auxiliary,
    pkgNameDirVariable',
    pkgNameDirVariable,
) where

import HaskellCI.Prelude

import qualified Data.Set                                     as S
import qualified Distribution.CabalSpecVersion                as C
import qualified Distribution.FieldGrammar.Pretty             as C
import qualified Distribution.Fields.Pretty                   as C
import qualified Distribution.Pretty                          as C
import qualified Distribution.Types.GenericPackageDescription as C
import qualified Distribution.Types.VersionRange              as C
import qualified Network.URI                                  as URI
import qualified Text.PrettyPrint                             as PP

import Cabal.Optimization
import Cabal.Project
import Cabal.SourceRepo
import HaskellCI.Compiler
import HaskellCI.Config
import HaskellCI.Config.Components
import HaskellCI.Config.CopyFields
import HaskellCI.Config.Docspec
import HaskellCI.Config.Doctest
import HaskellCI.Jobs
import HaskellCI.List
import HaskellCI.Package

-- | Auxiliary definitions, probably useful for all backends
data Auxiliary = Auxiliary
    { pkgs                    :: [Package]
    , uris                    :: [URI]
    , projectName             :: String
    , doctestEnabled          :: Bool
    , docspecEnabled          :: Bool
    , hasTests                :: CompilerRange
    , hasLibrary              :: Bool
    , extraCabalProjectFields :: FilePath -> [C.PrettyField ()]
    , testShowDetails         :: String
    , anyJobUsesHeadHackage   :: Bool
    , anyJobUsesPreviewGHC    :: Bool
    , runHaddock              :: Bool
    , haddockFlags            :: String
    }

auxiliary :: Config -> Project URI Void Package -> JobVersions -> Auxiliary
auxiliary Config {..} prj JobVersions {..} = Auxiliary {..}
  where
    pkgs = prjPackages prj
    uris = prjUriPackages prj
    projectName = fromMaybe "ci" (cfgProjectName <|> pkgName <$> listToMaybe pkgs)

    doctestEnabled = any (maybeGHC False (`C.withinRange` cfgDoctestEnabled cfgDoctest)) linuxVersions
    docspecEnabled = any (maybeGHC False (`C.withinRange` cfgDocspecEnabled cfgDocspec)) linuxVersions

    testShowDetails
        | cfgTestOutputDirect = " --test-show-details=direct"
        | otherwise           = ""

    -- version range which has tests
    hasTests :: CompilerRange
    hasTests = RangePoints $ S.unions
        [ pkgJobs
        | Pkg{pkgGpd,pkgJobs} <- pkgs
        , not $ null $ C.condTestSuites pkgGpd
        ]

    hasLibrary = any (\Pkg{pkgGpd} -> isJust $ C.condLibrary pkgGpd) pkgs

    runHaddock = not (equivVersionRanges C.noVersion cfgHaddock)
        && case cfgHaddockComponents of
            ComponentsAll  -> True
            ComponentsLibs -> hasLibrary

    haddockFlags = case cfgHaddockComponents of
        ComponentsAll  -> " --haddock-all"
        ComponentsLibs -> ""

    extraCabalProjectFields :: FilePath -> [C.PrettyField ()]
    extraCabalProjectFields rootdir = buildList $ do
        -- generate package fields for URI packages.
        for_ uris $ \uri ->
            item $ C.PrettyField () "packages" $ PP.text $ case URI.uriScheme uri of
                "file:" -> rootdir ++ URI.uriPath uri
                _       -> uriToString id uri ""

        -- copy fields from original cabal.project
        case cfgCopyFields of
            CopyFieldsNone -> pure ()
            CopyFieldsSome -> copyFieldsSome
            CopyFieldsAll  -> copyFieldsSome *> traverse_ item (prjOtherFields prj)

        -- local ghc-options
        unless (null cfgLocalGhcOptions) $ for_ pkgs $ \Pkg{pkgName} -> do
            let s = unwords $ map (show . C.showToken) cfgLocalGhcOptions
            item $ C.PrettySection () "package" [PP.text pkgName] $ buildList $
                item $ C.PrettyField () "ghc-options" $ PP.text s

        -- raw-project is after local-ghc-options so we can override per package.
        traverse_ item cfgRawProject
      where
        copyFieldsSome :: ListBuilder (C.PrettyField ()) ()
        copyFieldsSome = do
            for_ (prjConstraints prj) $ \xs -> do
                let s = concat (lines xs)
                item $ C.PrettyField () "constraints" $ PP.text s

            for_ (prjAllowNewer prj) $ \xs -> do
                let s = concat (lines xs)
                item $ C.PrettyField () "allow-newer" $ PP.text s

            when (prjReorderGoals prj) $
                item $ C.PrettyField () "reorder-goals" $ PP.text "True"

            for_ (prjMaxBackjumps prj) $ \bj ->
                item $ C.PrettyField () "max-backjumps" $ PP.text $ show bj

            case prjOptimization prj of
                OptimizationOn      -> return ()
                OptimizationOff     -> item $ C.PrettyField () "optimization" $ PP.text "False"
                OptimizationLevel l -> item $ C.PrettyField () "optimization" $ PP.text $ show l

            for_ (prjSourceRepos prj) $ \repo ->
                item $ C.PrettySection () "source-repository-package" [] $
                    C.prettyFieldGrammar C.cabalSpecLatest sourceRepositoryPackageGrammar (srpHoist toList repo)

    -- GHC versions which need head.hackage
    headGhcVers :: Set CompilerVersion
    headGhcVers = S.filter (usesHeadHackage cfgHeadHackage) allVersions

    anyJobUsesHeadHackage :: Bool
    anyJobUsesHeadHackage = not $ null headGhcVers

    anyJobUsesPreviewGHC :: Bool
    anyJobUsesPreviewGHC = not $ null $ S.filter isPreviewGHC allVersions

pkgNameDirVariable' :: String -> String
pkgNameDirVariable' n = "PKGDIR_" ++ map f n where
    f '-' = '_'
    f c   = c

pkgNameDirVariable :: String -> String
pkgNameDirVariable n = "${" ++ pkgNameDirVariable' n ++ "}"
