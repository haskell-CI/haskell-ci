{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}
-- | License: GPL-3.0-or-later AND BSD-3-Clause
--
-- @.cabal@ and a like file parsing helpers.
module Cabal.Parse (
    parseWith,
    ParseError (..),
    renderParseError,
    ) where

import Control.DeepSeq            (NFData (..))
import Control.Exception          (Exception (..))
import Data.ByteString            (ByteString)
import Data.Foldable              (for_)
import Data.List.NonEmpty         (NonEmpty)
import Data.Typeable              (Typeable)
import Distribution.Utils.Generic (fromUTF8BS)
import GHC.Generics               (Generic)
import System.FilePath            (normalise)

import qualified Data.ByteString.Char8          as BS8
import qualified Distribution.Fields            as C
import qualified Distribution.Fields.LexerMonad as C
import qualified Distribution.Parsec            as C
import qualified Distribution.Utils.Generic     as C
import qualified Text.Parsec                    as P

-- | Parse the contents using provided parser from 'C.Field' list.
--
-- This variant doesn't return any warnings in the successful case.
--
parseWith
    :: ([C.Field C.Position] -> C.ParseResult a)  -- ^ parse
    -> FilePath                                   -- ^ filename
    -> ByteString                                 -- ^ contents
    -> Either (ParseError NonEmpty) a
parseWith parser fp bs = case C.runParseResult result of
    (_, Right x)       -> return x
    (ws, Left (_, es)) -> Left $ ParseError fp bs es ws
  where
    result = case C.readFields' bs of
        Left perr -> C.parseFatalFailure pos (show perr) where
            ppos = P.errorPos perr
            pos  = C.Position (P.sourceLine ppos) (P.sourceColumn ppos)
        Right (fields, lexWarnings) -> do
            C.parseWarnings (C.toPWarnings lexWarnings)
            for_ (C.validateUTF8 bs) $ \pos ->
                C.parseWarning C.zeroPos C.PWTUTF $ "UTF8 encoding problem at byte offset " ++ show pos
            parser fields

-- | Parse error.
data ParseError f = ParseError
    { peFilename :: FilePath
    , peContents :: ByteString
    , peErrors   :: f C.PError
    , peWarnings :: [C.PWarning]
    }
  deriving (Generic)

deriving instance (Show (f C.PError)) => Show (ParseError f)

instance (Foldable f, Show (f C.PError), Typeable f) => Exception (ParseError f) where
    displayException = renderParseError

-- | @since 0.2.1
instance (NFData (f C.PError)) => NFData (ParseError f)

-- | Render parse error highlighting the part of the input file.
renderParseError :: Foldable f => ParseError f -> String
renderParseError (ParseError filepath contents errors warnings)
    | null errors && null warnings = ""
    | null errors = unlines $
        ("Warnings encountered when parsing  file " ++ filepath ++ ":")
        : renderedWarnings
    | otherwise = unlines $
        [ "Errors encountered when parsing file " ++ filepath ++ ":"
        ]
        ++ renderedErrors
        ++ renderedWarnings
  where
    filepath' = normalise filepath

    -- lines of the input file. 'lines' is taken, so they are called rows
    -- contents, line number, whether it's empty line
    rows :: [(String, Int, Bool)]
    rows = zipWith f (BS8.lines contents) [1..] where
        f bs i = let s = fromUTF8BS bs in (s, i, isEmptyOrComment s)

    rowsZipper = listToZipper rows

    isEmptyOrComment :: String -> Bool
    isEmptyOrComment s = case dropWhile (== ' ') s of
        ""          -> True   -- empty
        ('-':'-':_) -> True   -- comment
        _           -> False

    renderedErrors   = concatMap renderError errors
    renderedWarnings = concatMap renderWarning warnings

    renderError :: C.PError -> [String]
    renderError (C.PError pos@(C.Position row col) msg)
        -- if position is 0:0, then it doesn't make sense to show input
        -- looks like, Parsec errors have line-feed in them
        | pos == C.zeroPos = msgs
        | otherwise      = msgs ++ formatInput row col
      where
        msgs = [ "", filepath' ++ ":" ++ C.showPos pos ++ ": error:", trimLF msg, "" ]

    renderWarning :: C.PWarning -> [String]
    renderWarning (C.PWarning _ pos@(C.Position row col) msg)
        | pos == C.zeroPos = msgs
        | otherwise      = msgs ++ formatInput row col
      where
        msgs = [ "", filepath' ++ ":" ++ C.showPos pos ++ ": warning:", trimLF msg, "" ]

    -- sometimes there are (especially trailing) newlines.
    trimLF :: String -> String
    trimLF = dropWhile (== '\n') . reverse . dropWhile (== '\n') . reverse

    -- format line: prepend the given line number
    formatInput :: Int -> Int -> [String]
    formatInput row col = case advance (row - 1) rowsZipper of
        Zipper xs ys -> before ++ after where
            before = case span (\(_, _, b) -> b) xs of
                (_, [])     -> []
                (zs, z : _) -> map formatInputLine $ z : reverse zs

            after  = case ys of
                []        -> []
                (z : _zs) ->
                    [ formatInputLine z                             -- error line
                    , "      | " ++ replicate (col - 1) ' ' ++ "^"  -- pointer: ^
                    ]
                    -- do we need rows after?
                    -- ++ map formatInputLine (take 1 zs)           -- one row after

    formatInputLine :: (String, Int, Bool) -> String
    formatInputLine (str, row, _) = leftPadShow row ++ " | " ++ str

    -- hopefully we don't need to work with over 99999 lines .cabal files
    -- at that point small glitches in error messages are hopefully fine.
    leftPadShow :: Int -> String
    leftPadShow n = let s = show n in replicate (5 - length s) ' ' ++ s

data Zipper a = Zipper [a] [a]

listToZipper :: [a] -> Zipper a
listToZipper = Zipper []

advance :: Int -> Zipper a -> Zipper a
advance n z@(Zipper xs ys)
    | n <= 0 = z
    | otherwise = case ys of
        []      -> z
        (y:ys') -> advance (n - 1) $ Zipper (y:xs) ys'
