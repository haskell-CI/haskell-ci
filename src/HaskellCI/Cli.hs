{-# LANGUAGE CPP #-}

-- | Most of client interface.
module HaskellCI.Cli where

import HaskellCI.Prelude

import System.Exit (exitFailure)
import System.IO   (hPutStrLn, stderr)

import qualified Options.Applicative as O

import HaskellCI.Config
import HaskellCI.OptparseGrammar
import HaskellCI.VersionInfo

-------------------------------------------------------------------------------
-- Command
-------------------------------------------------------------------------------

data Command
    = CommandTravis FilePath
    | CommandBash FilePath
    | CommandGitHub FilePath
    | CommandRegenerate
    | CommandListGHC
    | CommandDumpConfig
    | CommandVersionInfo
  deriving Show

-------------------------------------------------------------------------------
-- Options
-------------------------------------------------------------------------------

data Options = Options
    { optOutput         :: Maybe Output
    , optConfig         :: Maybe FilePath
    , optCwd            :: Maybe FilePath
    , optConfigMorphism :: Config -> Config
    }

data Output = OutputStdout | OutputFile FilePath

instance Semigroup Options where
    Options b d c e <> Options b' d' c' e' =
        Options (b <|> b') (d <|> d') (c <|> c') (e' . e)

defaultOptions :: Options
defaultOptions = Options
    { optOutput         = Nothing
    , optConfig         = Nothing
    , optCwd            = Nothing
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
    <*> O.optional (O.strOption (O.long "cwd" <> O.metavar "Dir" <> O.help "Directory to change to"))
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

cliParserInfo :: O.ParserInfo (Command, Options)
cliParserInfo = O.info ((,) <$> cmdP <*> optionsP O.<**> versionP O.<**> O.helper) $ mconcat
    [ O.fullDesc
    , O.header "haskell-ci - generate CI scripts for Haskell projects"
    ]
  where
    cmdP = O.subparser (mconcat
        [ O.command "regenerate"   $ O.info (pure CommandRegenerate)  $ O.progDesc "Regenerate outputs"
        , O.command "travis"       $ O.info travisP                   $ O.progDesc "Generate travis-ci config"
        , O.command "bash"         $ O.info bashP                     $ O.progDesc "Generate local-bash-docker script"
        , O.command "github"       $ O.info githubP                   $ O.progDesc "Generate GitHub Actions config"
        , O.command "list-ghc"     $ O.info (pure CommandListGHC)     $ O.progDesc "List known GHC versions"
        , O.command "dump-config"  $ O.info (pure CommandDumpConfig)  $ O.progDesc "Dump cabal.haskell-ci config with default values"
        , O.command "version-info" $ O.info (pure CommandVersionInfo) $ O.progDesc "Print versions info haskell-ci was compiled with"
        ]) <|> travisP

    travisP = CommandTravis
        <$> O.strArgument (O.metavar "CABAL.FILE" <> O.help "Either <pkg.cabal> or cabal.project")

    bashP = CommandBash
        <$> O.strArgument (O.metavar "CABAL.FILE" <> O.help "Either <pkg.cabal> or cabal.project")

    githubP = CommandGitHub
        <$> O.strArgument (O.metavar "CABAL.FILE" <> O.help "Either <pkg.cabal> or cabal.project")

-------------------------------------------------------------------------------
-- Parsing helpers
-------------------------------------------------------------------------------

parseOptions :: [String] -> IO (FilePath, Options)
parseOptions argv = case res of
    O.Success (cmd, opts) -> do
        path <- fromCmd cmd
        return (path, opts)
    O.Failure f -> case O.renderFailure f "haskell-ci" of
        (help, _) -> hPutStrLn stderr help >> exitFailure
    O.CompletionInvoked _ -> exitFailure -- unexpected
  where
    res = O.execParserPure (O.prefs mempty) cliParserInfo argv

    fromCmd :: Command -> IO FilePath
    fromCmd (CommandTravis fp) = return fp
    fromCmd (CommandBash fp)   = return fp
    fromCmd (CommandGitHub fp) = return fp
    fromCmd cmd                = fail $ "Command without filepath: " ++ show cmd
