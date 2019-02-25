{-# LANGUAGE CPP #-}
module HaskellCI.MakeTravisOutput where

import           Prelude                     ()
import           Prelude.Compat

import           Control.Monad               (mzero)
import           Control.Monad.Trans.Class   (lift)
import           Control.Monad.Trans.Maybe   (MaybeT)
import           Control.Monad.Trans.Writer  (WriterT, tell)
import           Data.Functor.Identity       (Identity (..))
import           Data.Maybe                  (mapMaybe)
import           Data.Set                    (Set)
import           Data.String                 (IsString (..))
import           Distribution.Version

import qualified Data.Set                    as S

#ifdef MIN_VERSION_ShellCheck
import           ShellCheck.Checker          (checkScript)
import qualified ShellCheck.Formatter.Format as SC
import qualified ShellCheck.Formatter.TTY    as SC.TTY
import qualified ShellCheck.Interface        as SC
import           System.IO.Unsafe            (unsafePerformIO)
#endif

import           HaskellCI.Config.Folds
import           HaskellCI.Version

-- |  Encode shell command to be YAML safe and (optionally) ShellCheck it.
sh :: String -> Row
sh = sh'
    [ 2034 -- VAR appears unused. Verify it or export it.
    , 2086 -- SC2086: Double quote to prevent globbing and word splitting.
    , 2002 -- SC2002: Useless cat. Consider 'cmd < file | ..' or 'cmd file | ..' instead.
    ]

shForJob :: Set Version -> VersionRange -> String -> Row
shForJob  versions vr cmd
    | all (`withinRange` vr) versions       = sh cmd
    | not $ any (`withinRange` vr) versions = RowSkip
    | otherwise                             = sh $ unwords
        [ "if"
        , ghcVersionPredicate vr
        , "; then"
        , cmd
        , "; fi"
        ]

-- | Like 'sh' but with explicit SC exclude codes.
sh' :: [Integer] -> String -> Row
#ifndef MIN_VERSION_ShellCheck
sh' _ cmd = rawRow (shImpl cmd)
#else
sh' excl cmd =
    if null (SC.crComments res)
    then rawRow $ shImpl cmd
    else RowErr $ unlines $
        ("ShellCheck! " ++ cmd) :
        [ "SC" ++ show (SC.cCode c) ++ ": " ++ SC.cMessage c
        | pc <- SC.crComments res
        , let c = SC.pcComment pc
        ]

  where
    res = runIdentity $ checkScript iface spec
    iface = SC.SystemInterface $ \n -> return $ Left $ "cannot read file: " ++ n
    spec  = SC.emptyCheckSpec { SC.csFilename = "stdin"
                              , SC.csScript = cmd
                              , SC.csExcludedWarnings = excl
                              , SC.csShellTypeOverride = Just SC.Sh
                              }

scFormatter :: SC.Formatter
scFormatter = unsafePerformIO (SC.TTY.format (SC.newFormatterOptions { SC.foColorOption = SC.ColorAlways }))
#endif

-- Non-ShellCheck version of sh'
shImpl :: String -> String
shImpl cmd
    | ':' `elem` cmd = "  - " ++ show cmd
    | otherwise      = "  - " ++ cmd

comment :: String -> Row
comment c = rawRow $ "  # " ++ c

blank :: Row
blank = rawRow ""

rawRow :: String -> Row
rawRow = Row

type MakeTravisOutput = Result Diagnostic [String]

data Diagnostic
    = Info String
    | Warn String
    | Error String
  deriving (Eq, Show)

formatDiagnostics :: [Diagnostic] -> String
formatDiagnostics = unlines . map formatDiagnostic

formatDiagnostic :: Diagnostic -> String
formatDiagnostic (Error s) = "*ERROR* " ++ s
formatDiagnostic (Warn  s) = "*WARNING* " ++ s
formatDiagnostic (Info  s) = "*INFO* " ++ s

-- MaybeT is used to preserve the short-circuiting semantics of 'putStrLnErr'.
type YamlWriter m a = MaybeT (WriterT MakeTravisOutput m) a

putStrLnErr :: Monad m => String -> YamlWriter m a
putStrLnErr m = do
    lift . tell $ Failure [Error m]
    mzero

putStrLnErrs :: Monad m => [String] -> YamlWriter m ()
putStrLnErrs [] = return ()
putStrLnErrs ms = do
    lift (tell (Failure (map Error ms)))
    mzero

putStrLnWarn, putStrLnInfo :: Monad m => String -> YamlWriter m ()
putStrLnWarn m = lift . tell $ Success [Warn m] []
putStrLnInfo m = lift . tell $ Success [Info m] []

tellStrLn :: Monad m => String -> YamlWriter m ()
tellStrLn str = lift . tell $ success [str]

data Row
    = Row String
    | RowErr String
    | RowSkip

instance IsString Row where
    fromString = rawRow

tellStrLns :: Monad m => [Row] -> YamlWriter m ()
tellStrLns rows = case sequenceRows rows of
    Left err    -> lift $ tell $ Failure [Error err]
    Right rows' -> lift $ tell $ success rows'

tellStrLnsRaw :: Monad m => [String] -> YamlWriter m ()
tellStrLnsRaw rows = lift $ tell $ success rows

sequenceRows :: [Row] -> Either String [String]
sequenceRows = sequenceA . mapMaybe f where
    f (Row s)      = Just (Right s)
    f (RowErr err) = Just (Left err)
    f RowSkip      = Nothing

-------------------------------------------------------------------------------
-- Folded
-------------------------------------------------------------------------------

foldedTellStrLns
    :: Monad m
    => Fold
    -> String
    -> Set Fold
    -> YamlWriter m ()
    -> YamlWriter m ()
foldedTellStrLns label = foldedTellStrLns' label ""

foldedTellStrLns'
    :: Monad m
    => Fold
    -> String
    -> String
    -> Set Fold
    -> YamlWriter m ()
    -> YamlWriter m ()
foldedTellStrLns' label pfx prettyLabel labels output
    | label `S.notMember` labels = output
    | otherwise = tellStrLns prologue >> output >> tellStrLns epilogue
  where
    prologue = [ sh' [2039] $ concat
        [ "echo ", prettyLabel
        , " && echo -en 'travis_fold:start:", showFold' label, "\\\\r'"
        ]]
    epilogue = [ sh' [2039] $ "echo -en 'travis_fold:end:" ++ showFold' label ++ "\\\\r'" ]

    showFold' l = showFold l ++ if null pfx then "" else "-" ++ pfx

-------------------------------------------------------------------------------
-- Result
-------------------------------------------------------------------------------

data Result e a
    = Success [e] a
    | Failure [e]
    deriving (Eq, Show, Functor)

success :: a -> Result e a
success = Success []

instance Monoid a => Monoid (Result e a) where
    mempty = success mempty
    mappend = (<>)

instance Monoid a => Semigroup (Result e a) where
    Failure err1   <> Failure err2   = Failure $ err1 <> err2
    Failure err1   <> Success err2 _ = Failure $ err1 <> err2
    Success err1 _ <> Failure err2   = Failure $ err1 <> err2
    Success l1 o1  <> Success l2 o2  = Success (mappend l1 l2) (mappend o1 o2)
