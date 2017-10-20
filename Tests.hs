module Main (main) where

import MakeTravisYml (travisFromCabalFile, Options (..), defOptions, options)

import Control.Monad.Trans.Writer
import Data.Algorithm.Diff (Diff (..), getGroupedDiff)
import System.Console.GetOpt (getOpt, ArgOrder(Permute))
import System.Exit (exitFailure)
import System.FilePath (replaceExtension, (</>))
import System.IO (hPutStrLn, stderr)
import Test.Tasty (TestName, TestTree, defaultMain, testGroup)
import Test.Tasty.Golden.Advanced (goldenTest)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8

main :: IO ()
main = defaultMain $ testGroup "fixtures"
    [ fixtureGoldenTest "make-travis-yml.cabal"
        ["-o",".travis.yml","make-travis-yml.cabal"]
    ]

-- | 
-- @
-- travisFromCabalFile ::
--    ... => ([String],Options) -> FilePath -> [String] -> Writer [String] m ()
-- @
fixtureGoldenTest :: FilePath ->  [String] -> TestTree
fixtureGoldenTest fp argv = cabalGoldenTest fp output $ do
    (opts, _fp, xpkgs) <- parseOpts argv
    fmap (BS8.pack . unlines) $ execWriterT $
        -- always pass empty argv
        -- TODO: make test take argv, parse them.
        travisFromCabalFile (argv, opts) fp xpkgs
  where
    input = "fixtures" </> fp
    output = replaceExtension input "travis.yml"


parseOpts :: [String] -> IO (Options, FilePath, [String])
parseOpts argv = case getOpt Permute options argv of
    (opts,cabfn:xpkgs,[]) -> return (foldl (flip id) defOptions opts,cabfn,xpkgs)
    (_,_,[]) -> dieCli ["expected .cabal fle as first non-option argument\n"]
    (_,_,errs) -> dieCli errs
  where
    dieCli errs = hPutStrLn stderr (unlines errs) >> exitFailure

cabalGoldenTest :: TestName -> FilePath -> IO BS.ByteString -> TestTree
cabalGoldenTest name ref act = goldenTest name (BS.readFile ref) act cmp upd
  where
    upd = BS.writeFile ref
    cmp x y | x == y = return Nothing
    cmp x y = return $ Just $ unlines $
        concatMap f (getGroupedDiff (BS8.lines x) (BS8.lines y))
      where
        f (First xs)  = map (cons3 '-' . BS8.unpack) xs
        f (Second ys) = map (cons3 '+' . BS8.unpack) ys
        -- we print unchanged lines too. It shouldn't be a problem while we have
        -- reasonably small examples
        f (Both xs _) = map (cons3 ' ' . BS8.unpack) xs
        -- we add three characters, so the changed lines are easier to spot
        cons3 c cs = c : c : c : ' ' : cs
