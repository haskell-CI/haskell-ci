{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module HaskellCI.Sh (
    Sh (..),
    MonadSh (..),
    sh,
    ShM,
    runSh,
    ShError (..),
    FromShError (..),
    ) where

import Control.Monad (ap)
import Control.Exception (Exception)

#ifdef MIN_VERSION_ShellCheck
import           Data.Functor.Identity (Identity (..))
import           ShellCheck.Checker    (checkScript)
import qualified ShellCheck.Interface  as SC
#endif

import qualified Control.Monad.Fail as Fail

import HaskellCI.MonadErr

-------------------------------------------------------------------------------
-- shell command
-------------------------------------------------------------------------------

data Sh
    = Sh String       -- ^ command
    | Comment String  -- ^ comment
  deriving Show

-------------------------------------------------------------------------------
-- class
-------------------------------------------------------------------------------

class Monad m => MonadSh m where
    -- | Write shell command
    sh' :: [Integer] -> String -> m ()

    -- | Write comment
    comment :: String -> m ()

    -- | Commented block.
    --
    -- If the block is empty (or comments only), nothing might be written.
    commentedBlock :: String -> m () -> m ()

sh :: MonadSh m => String -> m ()
sh = sh'
    [ 2034 -- VAR appears unused. Verify it or export it.
    , 2086 -- SC2086: Double quote to prevent globbing and word splitting.
    , 2002 -- SC2002: Useless cat. Consider 'cmd < file | ..' or 'cmd file | ..' instead.
    -- TODO: because HEREDOC doesn't work
    , 2129 -- SC2129: Consider using { cmd1; cmd2; } >> file instead of individual redirects
    ]

-------------------------------------------------------------------------------
-- Errors
-------------------------------------------------------------------------------

data ShError
    = ShellCheckError String  -- ^ @ShellCheck@ disagrees.
    | ShError String          -- ^ made by 'fail'.
  deriving (Show)

instance Exception ShError

class FromShError e where
    fromShError :: ShError -> e

instance FromShError ShError where
    fromShError = id

-------------------------------------------------------------------------------
-- implementation
-------------------------------------------------------------------------------

newtype ShM a = ShM { unShM :: ([Sh] -> [Sh]) -> Either ShError ([Sh] -> [Sh], a) }
  deriving (Functor)

runSh :: (MonadErr e m, FromShError e) => ShM () -> m [Sh]
runSh (ShM f) = case f id of
    Left err      -> throwErr (fromShError err)
    Right (g, ()) -> return (g [])

instance Applicative ShM where
    pure x = ShM $ \shs -> Right (shs, x)
    (<*>) = ap

instance Monad ShM where
    return = pure

    m >>= k = ShM $ \shs0 -> do
        (shs1, x) <- unShM m     shs0
        (shs2, y) <- unShM (k x) shs1
        return (shs2, y)

    fail = Fail.fail

instance Fail.MonadFail ShM where
    fail = throwErr . ShError

instance MonadErr ShError ShM where
    throwErr err = ShM $ \_ -> Left err

unsafeSh :: String -> ShM ()
unsafeSh x = ShM $ \shs -> Right (shs . (Sh x :),      ())

instance MonadSh ShM where
#ifndef MIN_VERSION_ShellCheck
    sh' _ = unsafeSh
#else
    sh' excl cmd
        | null (SC.crComments res) = unsafeSh cmd
        | otherwise                = throwErr $ ShellCheckError $ unlines $
            ("ShellCheck! " ++ cmd) :
            [ "SC" ++ show (SC.cCode c) ++ ": " ++ SC.cMessage c
            | pc <- SC.crComments res
            , let c = SC.pcComment pc
            ]
      where
        res = runIdentity $ checkScript iface spec
        iface = SC.SystemInterface $ \n -> return $ Left $ "cannot read file: " ++ n
        spec  = SC.emptyCheckSpec
            { SC.csFilename          = "stdin"
            , SC.csScript            = cmd
            , SC.csExcludedWarnings  = excl
            , SC.csShellTypeOverride = Just SC.Sh
            }
#endif
    comment x = ShM $ \shs -> Right (shs . (Comment x :), ())

    commentedBlock c m = case runSh m of
        Left err  -> throwErr err
        Right shs
            | all isComment shs -> pure ()
            | otherwise         -> ShM $ \shs1 -> Right
                (shs1 . (\shs2 -> Comment c : shs ++ shs2), ())

isComment :: Sh -> Bool
isComment (Comment _) = True
isComment (Sh _)      = False
