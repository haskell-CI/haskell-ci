{-# LANGUAGE RecordWildCards #-}
-- | Which jobs to generate. Also helper for diagnostics output.
module HaskellCI.Jobs where

import HaskellCI.Prelude

import qualified Data.Set            as S
import qualified Distribution.Pretty as C

import HaskellCI.Compiler
import HaskellCI.Config
import HaskellCI.Diagnostics
import HaskellCI.Package
import HaskellCI.TestedWith

data JobVersions = JobVersions
    { versions           :: Set CompilerVersion  -- ^ all jobs
    , osxVersions        :: Set Version          -- ^ osx jobs: GHC only
    , omittedOsxVersions :: Set Version
    }

describeJobs :: MonadDiagnostics m => TestedWithJobs -> JobVersions -> [Package] -> m ()
describeJobs twj JobVersions {..} pkgs = do
    putStrLnInfo $ "Generating Travis-CI config for testing for GHC versions: " ++ ghcVersions
    case twj of
        TestedWithUniform -> pure ()
        TestedWithAny     -> for_ pkgs $ \pkg -> do
            -- this omits HEAD version.
            let vr = pkgJobs pkg
            let vs = showVersions vr
            putStrLnInfo $ pkgName pkg ++ " " ++ ": " ++ vs

    unless (null osxVersions) $  do
        putStrLnInfo $ "Also OSX jobs for: " ++ ghcOsxVersions
        unless (S.null omittedOsxVersions) $
            putStrLnWarn $ "Not all GHC versions specified with --osx are generated: " ++ ghcOmittedOsxVersions
  where
    showVersions :: Set CompilerVersion -> String
    showVersions = unwords . map dispGhcVersionShort . S.toList

    showVersionsV :: Set Version -> String
    showVersionsV = unwords . map C.prettyShow . S.toList

    ghcVersions :: String
    ghcVersions = showVersions versions

    ghcOsxVersions :: String
    ghcOsxVersions = showVersionsV osxVersions

    ghcOmittedOsxVersions :: String
    ghcOmittedOsxVersions = showVersionsV omittedOsxVersions

makeJobVersions :: Config -> Set CompilerVersion -> JobVersions
makeJobVersions Config {..} versions' = JobVersions {..} where
    -- All jobs
    versions :: Set CompilerVersion
    versions
        | cfgGhcHead = S.insert GHCHead versions'
        | otherwise  = versions'

    osxVersions' :: Set Version
    osxVersions' = cfgOsx

    osxVersions, omittedOsxVersions :: Set Version
    (osxVersions, omittedOsxVersions) = S.partition (\x -> GHC x `S.member` versions') osxVersions'


