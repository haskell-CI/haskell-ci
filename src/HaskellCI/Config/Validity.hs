{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module HaskellCI.Config.Validity where

import HaskellCI.Prelude

import HaskellCI.Compiler
import HaskellCI.Config
import HaskellCI.Config.Ubuntu
import HaskellCI.Error
import HaskellCI.Jobs
import HaskellCI.MonadErr

-- Validity checks shared in common among all backends.
checkConfigValidity :: MonadErr HsCiError m => Config -> JobVersions -> m ()
checkConfigValidity Config {..} JobVersions {..} = do
    when (anyGHCJS && cfgUbuntu > Bionic) $
        throwErr $ ValidationError $ "Using GHCJS requires Ubuntu 16.04 (Xenial) or 18.04 (Bionic)."
  where
    anyGHCJS = any isGHCJS versions
