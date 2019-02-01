module Glob where

import Control.Applicative as App ((<$>))
import Control.Monad (void, filterM, liftM2)
import Control.Monad.IO.Class
import Data.List (stripPrefix)
import Distribution.Compat.ReadP
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath.Posix ((</>))

-------------------------------------------------------------------------------
-- Glob
-------------------------------------------------------------------------------

{-

Globbing code and grammar judiciously stolen from cabal-install:

FilePathGlob    ::= FilePathRoot FilePathGlobRel
FilePathRoot    ::= {- empty -}        # relative to cabal.project
                  | "/"                # Unix root
                  | [a-zA-Z] ":" [/\\] # Windows root
                  | "~"                # home directory

FilePathGlobRel ::= Glob "/"  FilePathGlobRel # Unix directory
                  | Glob "\\" FilePathGlobRel # Windows directory
                  | Glob         # file
                  | {- empty -}  # trailing slash

Glob      ::= GlobPiece *
GlobPiece ::= "*"            # wildcard
            | [^*{},/\\] *   # literal string
            | "\\" [*{},]    # escaped reserved character
            | "{" Glob "," ... "," Glob "}" # union (match any of these)
-}

data FilePathGlob = FilePathGlob FilePathRoot FilePathGlobRel
  deriving (Eq, Show)

data FilePathGlobRel
   = GlobDir  Glob FilePathGlobRel
   | GlobFile Glob
   | GlobDirTrailing -- trailing dir, a glob ending in '/'
  deriving (Eq, Show)

-- | A single directory or file component of a globbed path
type Glob = [GlobPiece]

-- | A piece of a globbing pattern
data GlobPiece = WildCard
               | Literal String
               | Union [Glob]
  deriving (Eq, Show)

data FilePathRoot
   = FilePathRelative
   | FilePathRoot FilePath -- e.g. '/', 'c:\' or result of 'takeDrive'
   | FilePathHomeDir
  deriving (Eq, Show)

parseFilePathGlobRel :: ReadP r FilePathGlobRel
parseFilePathGlobRel =
      parseGlob >>= \globpieces ->
          asDir globpieces
      <++ asTDir globpieces
      <++ asFile globpieces
  where
    asDir  glob = do dirSep
                     GlobDir glob <$> parseFilePathGlobRel
    asTDir glob = do dirSep
                     return (GlobDir glob GlobDirTrailing)
    asFile glob = return (GlobFile glob)

    dirSep = void (char '/')
         +++ (do _ <- char '\\'
                 -- check this isn't an escape code
                 following <- look
                 case following of
                   (c:_) | isGlobEscapedChar c -> pfail
                   _                           -> return ())

parseGlob :: ReadP r Glob
parseGlob = many1 parsePiece
  where
    parsePiece = literal +++ wildcard +++ union'

    wildcard = char '*' >> return WildCard

    union' = between (char '{') (char '}') $
              fmap Union (sepBy1 parseGlob (char ','))

    literal = Literal `fmap` litchars1

    litchar = normal +++ escape

    normal  = satisfy (\c -> not (isGlobEscapedChar c)
                                && c /= '/' && c /= '\\')
    escape  = char '\\' >> satisfy isGlobEscapedChar

    litchars1 :: ReadP r [Char]
    litchars1 = liftM2 (:) litchar litchars

    litchars :: ReadP r [Char]
    litchars = litchars1 <++ return []

isGlobEscapedChar :: Char -> Bool
isGlobEscapedChar '*'  = True
isGlobEscapedChar '{'  = True
isGlobEscapedChar '}'  = True
isGlobEscapedChar ','  = True
isGlobEscapedChar _    = False

expandRelGlob :: MonadIO m => FilePath -> FilePathGlobRel -> m [FilePath]
expandRelGlob root glob0 = liftIO $ go glob0 ""
  where
    go (GlobFile glob) dir = do
      entries <- getDirectoryContents (root </> dir)
      let files = filter (matchGlob glob) entries
      return (map (dir </>) files)

    go (GlobDir glob globPath) dir = do
      entries <- getDirectoryContents (root </> dir)
      subdirs <- filterM (\subdir -> doesDirectoryExist
                                       (root </> dir </> subdir))
               $ filter (matchGlob glob) entries
      concat App.<$> mapM (\subdir -> go globPath (dir </> subdir)) subdirs

    go GlobDirTrailing dir = return [dir]

matchGlob :: Glob -> FilePath -> Bool
matchGlob = goStart
  where
    -- From the man page, glob(7):
    --   "If a filename starts with a '.', this character must be
    --    matched explicitly."

    go, goStart :: [GlobPiece] -> String -> Bool

    goStart (WildCard:_) ('.':_)  = False
    goStart (Union globs:rest) cs = any (\glob -> goStart (glob ++ rest) cs)
                                        globs
    goStart rest               cs = go rest cs

    go []                 ""    = True
    go (Literal lit:rest) cs
      | Just cs' <- stripPrefix lit cs
                                = go rest cs'
      | otherwise               = False
    go [WildCard]         ""    = True
    go (WildCard:rest)   (c:cs) = go rest (c:cs) || go (WildCard:rest) cs
    go (Union globs:rest)   cs  = any (\glob -> go (glob ++ rest) cs) globs
    go []                (_:_)  = False
    go (_:_)              ""    = False
