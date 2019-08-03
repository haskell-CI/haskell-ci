{-# LANGUAGE CPP #-}

-- | Most of client interface.
module HaskellCI.Cli where

import HaskellCI.Prelude

import System.Exit (exitFailure)
import System.IO   (hPutStrLn, stderr)

import qualified Options.Applicative as O

import HaskellCI.Config
import HaskellCI.OptparseGrammar

#ifndef CURRENT_PACKAGE_VERSION
#define CURRENT_PACKAGE_VERSION "???"
#endif

-------------------------------------------------------------------------------
-- Command
-------------------------------------------------------------------------------

data Command
    = CommandTravis FilePath
    | CommandRegenerate
    | CommandListGHC
    | CommandDumpConfig
  deriving Show

-------------------------------------------------------------------------------
-- Options
-------------------------------------------------------------------------------

data Options = Options
    { optOutput         :: Maybe Output
    , optConfig         :: Maybe FilePath
    , optConfigMorphism :: Config -> Config
    }

data Output = OutputStdout | OutputFile FilePath

instance Semigroup Options where
    Options b d e <> Options b' d' e' =
        Options (b <|> b') (d <|> d') (e' . e)

defaultOptions :: Options
defaultOptions = Options
    { optOutput         = Nothing
    , optConfig         = Nothing
    , optConfigMorphism = id
    }

optionsWithOutputFile :: FilePath -> Options
optionsWithOutputFile fp = defaultOptions
    { optOutput = Just (OutputFile fp)
    }

-------------------------------------------------------------------------------
-- Parsers
-------------------------------------------------------------------------------

optionsP :: O.Parser Options
optionsP = Options
    <$> O.optional outputP
    <*> O.optional (O.strOption (O.long "config" <> O.metavar "CONFIGFILE" <> O.help "Configuration file"))
    <*> runOptparseGrammar configGrammar

outputP :: O.Parser Output
outputP =
    OutputFile <$> O.strOption (O.long "output" <> O.short 'o' <> O.metavar "FILE" <> O.help "Output file") <|>
    O.flag' OutputStdout (O.long "stdout" <> O.help "Use stdout output")

versionP :: O.Parser (a -> a)
versionP = O.infoOption haskellCIVerStr $ mconcat
    [ O.long "version"
    , O.short 'V'
    , O.help "Print version information"
    ]

haskellCIVerStr :: String
haskellCIVerStr = CURRENT_PACKAGE_VERSION

cliParserInfo :: O.ParserInfo (Command, Options)
cliParserInfo = O.info ((,) <$> cmdP <*> optionsP O.<**> versionP O.<**> O.helper) $ mconcat
    [ O.fullDesc
    , O.header "haskell-ci - generate CI scripts for Haskell projects"
    ]
  where
    cmdP = O.subparser (mconcat
        [ O.command "regenerate"  $ O.info (pure CommandRegenerate) $ O.progDesc "Regenerate .travis.yml"
        , O.command "travis"      $ O.info travisP                  $ O.progDesc "Generate travis-ci config"
        , O.command "list-ghc"    $ O.info (pure CommandListGHC)    $ O.progDesc "List known GHC versions"
        , O.command "dump-config" $ O.info (pure CommandDumpConfig) $ O.progDesc "Dump cabal.haskell-ci config with default values"
        ]) <|> travisP

    travisP = CommandTravis
        <$> O.strArgument (O.metavar "CABAL.FILE" <> O.help "Either <pkg.cabal> or cabal.project")

-------------------------------------------------------------------------------
-- Parsing helpers
-------------------------------------------------------------------------------

parseTravis :: [String] -> IO (FilePath, Options)
parseTravis argv = case res of
    O.Success x -> return x
    O.Failure f -> case O.renderFailure f "haskell-ci" of
        (help, _) -> hPutStrLn stderr help >> exitFailure
    O.CompletionInvoked _ -> exitFailure -- unexpected
  where
    res = O.execParserPure (O.prefs mempty) (O.info ((,) <$> cmdP <*> optionsP) mempty) argv
    cmdP = O.strArgument (O.metavar "CABAL.FILE" <> O.help "Either <pkg.cabal> or cabal.project")


