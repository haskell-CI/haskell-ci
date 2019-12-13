module Main (main) where

import Criterion.Main (defaultMain, bench, nf, env)

import qualified Cabal.Project as C
import qualified Data.ByteString as BS

main :: IO ()
main = defaultMain
    [ env (BS.readFile path ) $ \contents ->
        bench "haskell-ci.project" $ nf (C.parseProject path) contents
    ]
  where
    path = "fixtures/haskell-ci.project"
