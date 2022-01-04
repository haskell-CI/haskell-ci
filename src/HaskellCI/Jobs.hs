{-# LANGUAGE RecordWildCards #-}
-- | Which jobs to generate. Also helper for diagnostics output.
module HaskellCI.Jobs (
    JobVersions (..),
    describeJobs,
    makeJobVersions,
) where

import HaskellCI.Prelude

import qualified Data.Set as S

import HaskellCI.Compiler
import HaskellCI.Config
import HaskellCI.Diagnostics
import HaskellCI.Package
import HaskellCI.TestedWith

data JobVersions = JobVersions
    { allVersions   :: Set CompilerVersion  -- ^ all versions (useful for Travis)
    , linuxVersions :: Set CompilerVersion  -- ^ linux jobs
    , macosVersions :: Set CompilerVersion  -- ^ macos jobs
    }

describeJobs
    :: MonadDiagnostics m
    => String          -- ^ config
    -> TestedWithJobs
    -> JobVersions
    -> [Package] -> m ()
describeJobs typ twj JobVersions {..} pkgs = do
    putStrLnInfo $ "Generating " ++ typ ++ " for testing for GHC versions: " ++ ghcVersions
    case twj of
        TestedWithUniform -> pure ()
        TestedWithAny     -> traverse_ putStrLnInfo $ table'
            [ pkgName pkg : "": map (showPkgVersion (pkgJobs pkg)) (S.toList allVersions)
            | pkg <- pkgs
            ]

    unless (null macosVersions) $  do
        putStrLnInfo $ "Also macos jobs for: " ++ ghcmacosVersions
  where
    showPkgVersion :: Set CompilerVersion -> CompilerVersion -> String
    showPkgVersion vs v
        | S.member v vs = dispGhcVersionShort v
        | otherwise     = ""

    showVersions :: Set CompilerVersion -> String
    showVersions = unwords . map dispGhcVersionShort . S.toList

    ghcVersions :: String
    ghcVersions = showVersions linuxVersions

    ghcmacosVersions :: String
    ghcmacosVersions = showVersions macosVersions

makeJobVersions :: Config -> Set CompilerVersion -> JobVersions
makeJobVersions Config {..} versions' = JobVersions {..} where
    -- All jobs
    versions :: Set CompilerVersion
    versions
        | cfgGhcHead = S.insert GHCHead versions'
        | otherwise  = versions'

    allVersions :: Set CompilerVersion
    allVersions = S.filter (`compilerWithinRange` range)  versions

    linuxVersions :: Set CompilerVersion
    linuxVersions = S.filter (`compilerWithinRange` linuxRange) allVersions

    macosVersions :: Set CompilerVersion
    macosVersions = S.filter (`compilerWithinRange` macosRange) allVersions

    range, linuxRange, macosRange :: CompilerRange
    range      = Range cfgEnabledJobs
    linuxRange = Range cfgLinuxJobs
    macosRange = RangeGHC /\ Range cfgMacosJobs

-- https://oleg.fi/gists/posts/2019-04-28-tabular.html
table' :: [[String]] -> [String]
table' cells = rows
  where
    cols      :: Int
    rowWidths :: [Int]
    rows      :: [String]

    (cols, rowWidths, rows) = foldr go (0, repeat 0, []) cells

    go :: [String] -> (Int, [Int], [String]) -> (Int, [Int], [String])
    go xs (c, w, yss) =
        ( max c (length xs)
        , zipWith max w (map length xs ++ repeat 0)
        , unwords (take cols (zipWith fill xs rowWidths))
          : yss
        )

    fill :: String -> Int -> String
    fill s n = s ++ replicate (n - length s) ' '
