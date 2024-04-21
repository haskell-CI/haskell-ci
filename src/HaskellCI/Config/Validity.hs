{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module HaskellCI.Config.Validity where

import HaskellCI.Prelude

import HaskellCI.Config
import HaskellCI.Config.Ubuntu
import HaskellCI.Error
import HaskellCI.Jobs
import HaskellCI.MonadErr

-- Validity checks shared in common among all backends.
checkConfigValidity :: MonadErr HsCiError m => Config -> JobVersions -> m ()
checkConfigValidity Config {..} _  = do
    unless (cfgUbuntu >= Focal) $
        throwErr $ ValidationError $ prettyShow cfgUbuntu ++ "distribution is not supported"
