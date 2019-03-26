{-# LANGUAGE RecordWildCards #-}
-- | Which jobs to generate. Also helper for diagnostics output.
module HaskellCI.Jobs where

import Control.Monad        (unless)
import Data.Set             (Set)
import Distribution.Version (Version)

import qualified Data.Set as S

import HaskellCI.Config
import HaskellCI.Diagnostics
import HaskellCI.GHC

data JobVersions = JobVersions
    { versions           :: Set (Maybe Version)
    , versions'          :: Set Version
    , osxVersions        :: Set Version
    , omittedOsxVersions :: Set Version
    }

describeJobs :: MonadDiagnostics m => JobVersions -> m ()
describeJobs JobVersions {..} = do
    putStrLnInfo $ "Generating Travis-CI config for testing for GHC versions: " ++ ghcVersions
    unless (null osxVersions) $  do
        putStrLnInfo $ "Also OSX jobs for: " ++ ghcOsxVersions
        unless (S.null omittedOsxVersions) $
            putStrLnWarn $ "Not all GHC versions specified with --osx are generated: " ++ ghcOmittedOsxVersions
  where
    showVersions :: Set (Maybe Version) -> String
    showVersions = unwords . map dispGhcVersion . S.toList

    ghcVersions :: String
    ghcVersions = showVersions versions

    ghcOsxVersions :: String
    ghcOsxVersions = showVersions $ S.map Just osxVersions

    ghcOmittedOsxVersions :: String
    ghcOmittedOsxVersions = showVersions $ S.map Just omittedOsxVersions

makeJobVersions :: Config -> Set Version -> JobVersions
makeJobVersions Config {..} versions' = JobVersions {..} where
    -- All jobs
    versions :: Set (Maybe Version)
    versions
        | cfgGhcHead = S.insert Nothing $ S.map Just versions'
        | otherwise  = S.map Just versions'

    osxVersions' :: Set Version
    osxVersions' = cfgOsx

    osxVersions, omittedOsxVersions :: Set Version
    (osxVersions, omittedOsxVersions) = S.partition (`S.member` versions') osxVersions'


