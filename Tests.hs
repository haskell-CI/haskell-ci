{-# LANGUAGE ViewPatterns #-}
module Main (main) where

import MakeTravisYml
    (travisFromConfigFile, MakeTravisOutput(..), Options (..), defOptions, options)

import Control.Applicative ((<$>), (<*>))
import Control.Exception (ErrorCall(..), throwIO, try)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Writer
import Data.Algorithm.Diff (Diff (..), getGroupedDiff)
import Data.IORef
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (catMaybes)
import Data.Monoid (mconcat)
import System.Console.GetOpt (getOpt, ArgOrder(Permute))
import System.Directory (doesFileExist, removeFile, setCurrentDirectory)
import System.Exit (ExitCode(..), exitFailure)
import System.FilePath (addExtension, (</>))
import System.IO (hPutStrLn, stderr)
import Test.Tasty (TestName, TestTree, defaultMain, testGroup)
import Test.Tasty.Golden.Advanced (goldenTest)
import Text.Read (readMaybe)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8

main :: IO ()
main = do
    setCurrentDirectory "fixtures/"
    defaultMain $ testGroup "fixtures"
        [ fixtureGoldenTest "make-travis-yml.cabal"
        , fixtureGoldenTest "cabal.project.empty-line"
        , fixtureGoldenTest "cabal.project.fail-versions"
        , fixtureGoldenTest "cabal.project.messy"
        ]

linesToArgv :: String -> Maybe [String]
linesToArgv txt = case catMaybes (map lineToArgv (lines txt)) of
    [argv] -> Just argv
    _ -> Nothing
  where
    lineToArgv line
        | Just rest <- "# REGENDATA " `stripPrefix` line = readMaybe rest
        | otherwise = Nothing

-- |
-- @
-- travisFromConfigFile ::
--    ... => ([String],Options) -> FilePath -> [String] -> Writer [String] m ()
-- @
fixtureGoldenTest :: FilePath -> TestTree
fixtureGoldenTest fp = cabalGoldenTest fp outputRef errorRef $ do
    (argv, opts, xpkgs) <- makeTravisFlags
    let genConfig = travisFromConfigFile (argv, opts) fp xpkgs
    normalise <$> execWriterT (runMaybeT genConfig)
  where
    outputRef = addExtension fp "travis.yml"
    errorRef = addExtension fp "stderr"

    -- Avoid issues with multiline errors invalidating the output
    normalise :: MakeTravisOutput -> MakeTravisOutput
    normalise (Failure errs) = Failure . lines . unlines $ errs
    normalise other = other

    referenceArgv :: Bool -> IO (Maybe [String])
    referenceArgv refExists
        | refExists = (linesToArgv . BS8.unpack) `fmap` BS.readFile outputRef
        | otherwise = return $ Just [fp]

    makeTravisFlags :: IO ([String], Options, [String])
    makeTravisFlags = do
        result <- doesFileExist outputRef >>= referenceArgv
        case result of
            Nothing -> throwIO (ErrorCall "No REGENDATA in result file.")
            Just argv -> do
                (opts, _fp, xpkgs) <- parseOpts argv
                return (argv, opts, xpkgs)

parseOpts :: [String] -> IO (Options, FilePath, [String])
parseOpts argv = case getOpt Permute options argv of
    (opts,cabfn:xpkgs,[]) -> return (foldl (flip id) defOptions opts,cabfn,xpkgs)
    (_,_,[]) -> dieCli ["expected .cabal or cabal.project file as first non-option argument\n"]
    (_,_,errs) -> dieCli errs
  where
    dieCli errs = hPutStrLn stderr (unlines errs) >> exitFailure

cabalGoldenTest
    :: TestName
    -> FilePath
    -> FilePath
    -> IO MakeTravisOutput
    -> TestTree
cabalGoldenTest name outRef errRef act = goldenTest name readGolden act cmp upd
  where
    readData :: FilePath -> IO [String]
    readData fp = lines . BS8.unpack <$> BS.readFile fp

    readGolden = do
        refExists <- doesFileExist outRef
        if refExists
           then Success <$> readData errRef <*> readData outRef
           else Failure <$> readData errRef

    packData :: [String] -> BS.ByteString
    packData = BS8.pack . unlines

    upd (Failure (packData -> errs)) = BS.writeFile errRef errs
    upd (Success (packData -> warnings) (packData -> contents)) = do
        BS.writeFile outRef contents
        BS.writeFile errRef warnings

    cmp x y | x == y = return Nothing
    cmp (Failure err1) (Failure err2) = return . Just $ diff err1 err2
    cmp (Success warn1 out1) (Success warn2 out2) = return . Just . mconcat $
        [ diff warn1 warn2
        , "\n\n"
        , diff out1 out2
        ]
    cmp (Failure err) (Success warnings _) = return . Just . unlines $
        [ "Expected failure:" ] ++ err ++ ["\n\nFound success:"] ++ warnings
    cmp (Success warnings _) (Failure err) = return . Just . unlines $
        [ "Expected success:" ] ++ warnings ++ ["\n\nFound failure:"] ++ err

    diff x y = unlines $ concatMap f (getGroupedDiff x y)
    f (First xs)  = map (cons3 '-') xs
    f (Second ys) = map (cons3 '+') ys
    -- we print unchanged lines too. It shouldn't be a problem while we have
    -- reasonably small examples
    f (Both xs _) = map (cons3 ' ') xs
    -- we add three characters, so the changed lines are easier to spot
    cons3 c cs = c : c : c : ' ' : cs
