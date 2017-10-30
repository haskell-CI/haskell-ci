module Main (main) where

import MakeTravisYml (travisFromConfigFile, Options (..), defOptions, options)

import Control.Applicative ((<$>), (<*>))
import Control.Exception (ErrorCall(..), throwIO, try)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Data.Algorithm.Diff (Diff (..), getGroupedDiff)
import Data.IORef
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (catMaybes)
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
    ref <- newIORef []

    let bsUnlines = BS8.pack . unlines
        logError s = modifyIORef' ref (s:)
        genConfig = travisFromConfigFile (argv, opts) fp xpkgs

    ymlFile <- try $ execWriterT (runReaderT genConfig logError)
    stderrOutput <- reverse <$> readIORef ref
    return (bsUnlines <$> ymlFile, bsUnlines stderrOutput)
  where
    outputRef = addExtension fp "travis.yml"
    errorRef = addExtension fp "stderr"

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
    -> IO (Either ExitCode BS.ByteString, BS.ByteString)
    -> TestTree
cabalGoldenTest name outRef errRef act = goldenTest name readGolden act cmp upd
  where
    noYamlFile :: Either ExitCode a
    noYamlFile = Left $ ExitFailure 1

    readGolden = do
        refExists <- doesFileExist outRef
        if refExists
           then (,) <$> (Right <$> BS.readFile outRef) <*> BS.readFile errRef
           else (,) noYamlFile <$> BS.readFile errRef

    upd (output, errOutput) = do
        case output of
            Left _ -> return ()
            Right out -> BS.writeFile outRef out
        BS.writeFile errRef errOutput

    cmp x y | x == y = return Nothing
    cmp (x1,x2) (y1,y2) = return . Just $ mconcat
        [ cmpFail x1 y1
        , "\n\n"
        , unlines $ concatMap f (getGroupedDiff (BS8.lines x2) (BS8.lines y2))
        ]
      where
        cmpFail (Left x) (Right y) = unlines
            [ "Expected failure:", show x, "Found success:", BS8.unpack y ]
        cmpFail (Right x) (Left y) = unlines
            [ "Expected success:", BS8.unpack x, "Found failure:", show y ]
        cmpFail (Left x) (Left y) = unlines
            [ "Expected failure:", show x, "Found failure:", show y ]
        cmpFail (Right x) (Right y) = unlines $
            concatMap f (getGroupedDiff (BS8.lines x) (BS8.lines y))
        f (First xs)  = map (cons3 '-' . BS8.unpack) xs
        f (Second ys) = map (cons3 '+' . BS8.unpack) ys
        -- we print unchanged lines too. It shouldn't be a problem while we have
        -- reasonably small examples
        f (Both xs _) = map (cons3 ' ' . BS8.unpack) xs
        -- we add three characters, so the changed lines are easier to spot
        cons3 c cs = c : c : c : ' ' : cs
