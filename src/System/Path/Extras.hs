module System.Path.Extras where

import System.Path.Unsafe (Path (..))
import System.Path (Absolute, Unrooted)

import qualified System.FilePath as Native
import qualified System.Directory as Native

makeRelative :: Path Absolute -> Path Absolute -> Path Unrooted
makeRelative (Path a) (Path b) = Path (Native.makeRelative a b)

getCurrentDirectory :: IO (Path Absolute)
getCurrentDirectory = Path <$> Native.getCurrentDirectory
