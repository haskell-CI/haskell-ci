{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Prelude          hiding (pi)
import System.IO        (IOMode (ReadMode), withFile)
import Test.Tasty       (defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCaseSteps)

import qualified Codec.Archive.Tar.Entry as Tar
import qualified Codec.Archive.Tar.Index as Tar
import qualified Data.ByteString.Lazy    as LBS
import qualified Data.Map.Strict         as Map
import qualified Distribution.Package    as C
import qualified Distribution.Version    as C

import Cabal.Config
import Cabal.Index

main :: IO ()
main = defaultMain $ testGroup "Cabal.Index"
    [ testCaseSteps "low-level approach" $ \step -> do
        step "Read ~/.cabal/config"
        cfg <- readConfig

        step "Find hackage 01-index.tar"
        indexPath <- maybe
            (assertFailure "Cannot find hackage 01.indexTar")
            return
            (cfgRepoIndex cfg hackageHaskellOrg)

        step "Read metadata"
        meta <- indexMetadata indexPath Nothing

        step "Check aeson-1.4.4.0 metadata"
        case Map.lookup (C.mkPackageName "aeson") meta of
            Nothing -> assertFailure "Cannot find aeson on Hackage"
            Just pi -> case Map.lookup (C.mkVersion [1,4,4,0]) (piVersions pi) of
                Nothing -> assertFailure "Cannot find aeson-1.4.4.0 on Hackage"
                Just ri -> do
                    -- Note: if someone makes revision to aeson-1.4.4.0
                    -- revision and cabal hash check will start failing
                    -- tarball hash shouldn't ever change.
                    assertEqual "revision" 1 (riRevision ri)
                    assertEqual "cabal hash"
                        (unsafeMkSHA256 "a6f5eddcff9526c786a1b77bdfade54b42f67c066b379bbc4b55ffb291e6c7d6")
                        (riCabal ri)
                    assertEqual "tarball hash"
                        (unsafeMkSHA256 "17c67cdaca651e18f310b21b2b12bac6bcec5188c3ac0e4b64cc60c94d7e4d2e")
                        (riTarball ri)

                    -- check contents
                    withFile indexPath ReadMode $ \hdl -> do
                        entry <- Tar.hReadEntry hdl (riTarOffset ri)
                        case Tar.entryContent entry of
                            Tar.NormalFile bs fs -> do
                                assertEqual "entry content size"
                                    7251
                                    fs
                                assertEqual "entry content (prefix)"
                                    "name:            aeson\r\nversion:         1.4.4.0\r\nx-revision: 1\r\nlicense:       "
                                    (LBS.take 80 bs)

                            _ -> assertFailure "invalid entry content"

        step "binary (deprecated versions)"
        case Map.lookup (C.mkPackageName "binary") meta of
            Nothing -> assertFailure "Cannot find binary on Hackage"
            Just pi -> do
                assertBool "binary-0.9.0.0 should be in piVersions" $
                    Map.member (C.mkVersion [0,9,0,0]) (piVersions pi)

                assertBool "binary-0.9.0.0 should NOT be in piPreferredVersions" $
                    Map.notMember (C.mkVersion [0,9,0,0]) (piPreferredVersions pi)

    , testCaseSteps "cachedHackageMetadata" $ \step -> do
        step "First read"
        meta1 <- cachedHackageMetadata

        step "Second read"
        meta2 <- cachedHackageMetadata

        assertEqual "cached value should be the same" meta1 meta2

    ]
