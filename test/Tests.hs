{-# LANGUAGE ViewPatterns #-}
module Main (main) where

import HaskellCI             hiding (main)
import HaskellCI.Diagnostics (runDiagnosticsT)

import Control.Applicative        ((<$>), (<*>))
import Control.Exception          (ErrorCall (..), throwIO)
import Data.Algorithm.Diff        (Diff (..), getGroupedDiff)
import Data.List                  (stripPrefix)
import Data.Maybe                 (mapMaybe)
import Data.Monoid                (mconcat)
import System.Directory           (doesFileExist, setCurrentDirectory)
import System.FilePath            (addExtension)
import Test.Tasty                 (TestName, TestTree, defaultMain, testGroup)
import Test.Tasty.Golden.Advanced (goldenTest)
import Text.Read                  (readMaybe)

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS8
import qualified System.Console.ANSI   as ANSI

main :: IO ()
main = do
    setCurrentDirectory "fixtures/"
    defaultMain $ testGroup "fixtures"
        [ fixtureGoldenTest "haskell-ci.cabal"
        , fixtureGoldenTest "cabal.project.empty-line"
        , fixtureGoldenTest "cabal.project.fail-versions"
        , fixtureGoldenTest "cabal.project.messy"
        , testGroup "copy-fields"
            [ fixtureGoldenTest "cabal.project.copy-fields.all"
            , fixtureGoldenTest "cabal.project.copy-fields.some"
            , fixtureGoldenTest "cabal.project.copy-fields.none"
            ]
        ]

linesToArgv :: String -> Maybe [String]
linesToArgv txt = case mapMaybe lineToArgv (lines txt) of
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
    (argv, opts) <- makeTravisFlags
    let genConfig = travisFromConfigFile argv opts fp
    runDiagnosticsT genConfig
  where
    outputRef = addExtension fp "travis.yml"
    errorRef = addExtension fp "stderr"

    referenceArgv :: Bool -> IO (Maybe [String])
    referenceArgv refExists
        | refExists = (linesToArgv . BS8.unpack) `fmap` BS.readFile outputRef
        | otherwise = return $ Just [fp]

    makeTravisFlags :: IO ([String], Options)
    makeTravisFlags = do
        result <- doesFileExist outputRef >>= referenceArgv
        case result of
            Nothing -> throwIO (ErrorCall "No REGENDATA in result file.")
            Just argv -> do
                (opts, _fp) <- parseOpts argv
                return (argv, opts)

parseOpts :: [String] -> IO (Options, FilePath)
parseOpts argv = do
    (path, opts) <- parseTravis argv
    return (opts, path)

data Result
    = Success [String] [String]
    | Failure [String]
  deriving Eq

cabalGoldenTest
    :: TestName
    -> FilePath
    -> FilePath
    -> IO (Maybe [String], [String])
    -> TestTree
cabalGoldenTest name outRef errRef act = goldenTest name readGolden act' cmp upd
  where
    readData :: FilePath -> IO [String]
    readData fp = lines . BS8.unpack <$> BS.readFile fp

    act' = flip fmap act $ \r -> case r of
        (Nothing, diags) -> Failure diags
        (Just x, diags)  -> Success diags x

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

    diff x y =
        ansiReset -- reset tasty's red color
        ++ unlines (concatMap f (getGroupedDiff x y))

    f (First xs)  = map (withAnsiRed . cons3 '-') xs
    f (Second ys) = map (withAnsiGreen . cons3 '+') ys
    -- we print unchanged lines too.
    -- we trim the contents a little.
    f (Both xs _) = map (cons3 ' ') (shorten xs)

    -- we add three characters, so the changed lines are easier to spot
    cons3 c cs = c : c : c : ' ' : cs

    ansiReset = ANSI.setSGRCode [ ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.White] -- default color: gray
    ansiRed   = ANSI.setSGRCode [ ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red]
    ansiGreen = ANSI.setSGRCode [ ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Green]

    withAnsiRed s   = ansiRed ++ s ++ ansiReset
    withAnsiGreen s = ansiGreen ++ s ++ ansiReset

    -- for large thunks make ellipsis in between
    shorten xs
        | l < 15    = xs
        | otherwise = as ++ ["..."] ++ drop (l - 10) bs
      where
        l = length xs
        (as, bs) = splitAt 5 xs
