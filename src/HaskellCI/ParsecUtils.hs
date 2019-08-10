module HaskellCI.ParsecUtils where

import HaskellCI.Prelude

import System.Directory (doesFileExist)
import System.Exit      (exitFailure)
import System.IO        (hPutStr, stderr)

import qualified Data.ByteString                as BS
import qualified Distribution.Fields            as C
import qualified Distribution.Fields.LexerMonad as C (toPWarnings)
import qualified Distribution.Parsec            as C
import qualified Text.Parsec                    as P

import HaskellCI.ParsecError

readAndParseFile
    :: ([C.Field C.Position] -> C.ParseResult a)  -- ^ File fields to final value parser
    -> FilePath                                   -- ^ File to read
    -> IO a
readAndParseFile parser fpath = do
    exists <- doesFileExist fpath
    unless exists $ do
        putStrLn $ "Error Parsing: file \"" ++ fpath ++ "\" doesn't exist. Cannot continue."
        exitFailure
    bs <- BS.readFile fpath
    run bs $ case C.readFields' bs of
        Right (fs, lexWarnings) -> do
            C.parseWarnings (C.toPWarnings lexWarnings)
            parser fs
        Left perr -> C.parseFatalFailure pos (show perr) where
            ppos = P.errorPos perr
            pos  = C.Position (P.sourceLine ppos) (P.sourceColumn ppos)
  where
    run :: BS.ByteString -> C.ParseResult a -> IO a
    run bs r = case C.runParseResult r of
        (ws, Right x)      -> do
            hPutStr stderr $ renderParseError fpath bs [] ws
            return x
        (ws, Left (_, es)) -> do
            hPutStr stderr $ renderParseError fpath bs es ws
            exitFailure
