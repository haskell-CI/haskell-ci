{-# LANGUAGE RecordWildCards #-}
-- | Which jobs to generate. Also helper for diagnostics output.
module HaskellCI.Jobs where

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
        TestedWithAny     -> for_ pkgs $ \pkg -> do
            -- this omits HEAD version.
            let vr = pkgJobs pkg
            let vs = showVersions vr
            putStrLnInfo $ pkgName pkg ++ " " ++ ": " ++ vs

    unless (null macosVersions) $  do
        putStrLnInfo $ "Also macos jobs for: " ++ ghcmacosVersions
  where
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


