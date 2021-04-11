module HaskellCI.Error (
    HsCiError (..),
    FromHsCiError (..),
) where

import HaskellCI.Prelude

data HsCiError
    = ShellCheckError String  -- ^ @ShellCheck@ disagrees.
    | ValidationError String  -- ^ used by validations
    | FailError String        -- ^ made by 'fail'.
  deriving (Show)

instance Exception HsCiError where
    displayException (ShellCheckError s) = s
    displayException (ValidationError s) = s
    displayException (FailError s)         = "PANIC " ++ s

class FromHsCiError e where
    fromHsCiError :: HsCiError -> e

instance FromHsCiError HsCiError where
    fromHsCiError = id
