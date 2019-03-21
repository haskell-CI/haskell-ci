{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
module HaskellCI.MonadErr where

-- | 'MonadErr' is 'MonadError' without @catch@
class Monad m => MonadErr e m | m -> e where
    throwErr :: e -> m a

instance MonadErr e (Either e) where
    throwErr = Left
