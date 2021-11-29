{-# LANGUAGE CPP #-}

-- | Most of client interface.
module HaskellCI.Cli where

import HaskellCI.Prelude

import Control.Applicative   (optional)
import System.Exit           (exitFailure)
import System.FilePath.Posix (takeFileName)
import System.IO             (hPutStrLn, stderr)

import qualified Options.Applicative as O

import HaskellCI.Config
import HaskellCI.OptparseGrammar
import HaskellCI.Sourcehut (SourcehutOptions(..))
import HaskellCI.VersionInfo

-------------------------------------------------------------------------------
-- Command
-------------------------------------------------------------------------------

data Command
    = CommandTravis FilePath
    | CommandBash FilePath
    | CommandGitHub FilePath
    | CommandSourcehut (SourcehutOptions (Maybe String))
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
    , optConfig         :: ConfigOpt
    , optCwd            :: Maybe FilePath
    , optInputType      :: Maybe InputType
    , optConfigMorphism :: Config -> Config
    }

instance Semigroup Options where
    Options b d c e f <> Options b' d' c' e' f' =
        Options (b <|> b') (d <> d') (c <|> c') (e <|> e') (f' . f)

defaultOptions :: Options
defaultOptions = Options
    { optOutput         = Nothing
    , optConfig         = ConfigOptAuto
    , optCwd            = Nothing
    , optInputType      = Nothing
    , optConfigMorphism = id
    }

optionsWithOutputFile :: FilePath -> Options
optionsWithOutputFile fp = defaultOptions
    { optOutput = Just (OutputFile fp)
    }

data Output = OutputStdout | OutputFile FilePath

data ConfigOpt
    = ConfigOptAuto
    | ConfigOpt FilePath
    | ConfigOptNo
  deriving (Eq, Show)

instance Semigroup ConfigOpt where
    a <> ConfigOptAuto = a
    _ <> b             = b

-------------------------------------------------------------------------------
-- InputType
-------------------------------------------------------------------------------

data InputType
    = InputTypePackage -- ^ @.cabal@
    | InputTypeProject -- ^ @cabal.project
  deriving Show

optInputType' :: Options -> FilePath -> InputType
optInputType' opts path =
    fromMaybe def (optInputType opts)
  where
    def | "cabal.project" `isPrefixOf` takeFileName path = InputTypeProject
        | otherwise                                      = InputTypePackage

-------------------------------------------------------------------------------
-- Parsers
-------------------------------------------------------------------------------

optionsP :: O.Parser Options
optionsP = Options
    <$> O.optional outputP
    <*> configOptP
    <*> O.optional (O.strOption (O.long "cwd" <> O.metavar "Dir" <> O.action "directory" <> O.help "Directory to change to"))
    <*> O.optional inputTypeP
    <*> runOptparseGrammar configGrammar

configOptP :: O.Parser ConfigOpt
configOptP = file <|> noconfig <|> pure ConfigOptAuto
  where
    file = ConfigOpt <$> O.strOption (O.long "config" <> O.metavar "CONFIGFILE" <> O.action "file" <> O.help "Configuration file")
    noconfig = O.flag' ConfigOptNo (O.long "no-config" <> O.help "Don't read configuration file")

outputP :: O.Parser Output
outputP =
    OutputFile <$> O.strOption (O.long "output" <> O.short 'o' <> O.metavar "FILE" <> O.action "file" <> O.help "Output file") <|>
    O.flag' OutputStdout (O.long "stdout" <> O.help "Use stdout output")

versionP :: O.Parser (a -> a)
versionP = O.infoOption haskellCIVerStr $ mconcat
    [ O.long "version"
    , O.short 'V'
    , O.help "Print version information"
    ]

inputTypeP :: O.Parser InputType
inputTypeP = pkg <|> prj where
    pkg = O.flag' InputTypePackage $ O.long "package"
    prj = O.flag' InputTypeProject $ O.long "project"

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
        , O.command "sourcehut"    $ O.info sourcehutP                $ O.progDesc "Generate Sourcehut config"
        , O.command "list-ghc"     $ O.info (pure CommandListGHC)     $ O.progDesc "List known GHC versions"
        , O.command "dump-config"  $ O.info (pure CommandDumpConfig)  $ O.progDesc "Dump cabal.haskell-ci config with default values"
        , O.command "version-info" $ O.info (pure CommandVersionInfo) $ O.progDesc "Print versions info haskell-ci was compiled with"
        ]) <|> travisP

    travisP = CommandTravis
        <$> O.strArgument (O.metavar "CABAL.FILE" <> O.action "file" <> O.help "Either <pkg.cabal> or cabal.project")

    bashP = CommandBash
        <$> O.strArgument (O.metavar "CABAL.FILE" <> O.action "file" <> O.help "Either <pkg.cabal> or cabal.project")

    githubP = CommandGitHub
        <$> O.strArgument (O.metavar "CABAL.FILE" <> O.action "file" <> O.help "Either <pkg.cabal> or cabal.project")

    sourcehutP = fmap CommandSourcehut $ SourcehutOptions
        <$> O.strArgument (O.metavar "CABAL.FILE" <> O.action "file" <> O.help "Either <pkg.cabal> or cabal.project")
        <*> optional (O.strOption (O.long "source" <> O.metavar "URI" <> O.help "The source to test (default: from git remote)"))
        <*> O.switch      (O.long "sourcehut-parallel" <> O.help (
            "In Sourcehut, use many manifests to run jobs in parallel. " <>
            "Disabled by default because in the sr.ht instance a maximum of " <>
            "4 parallel jobs are allowed."
            ))

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
    res = O.execParserPure (O.prefs O.subparserInline) cliParserInfo argv

    fromCmd :: Command -> IO FilePath
    fromCmd (CommandTravis fp) = return fp
    fromCmd (CommandBash fp)   = return fp
    fromCmd (CommandGitHub fp) = return fp
    fromCmd (CommandSourcehut srhtOpts) = return $ sourcehutOptPath srhtOpts
    fromCmd cmd                = fail $ "Command without filepath: " ++ show cmd

-- TODO find a way to merge this with the above... or use global options only
parseOptionsSrht :: [String] -> IO (SourcehutOptions (Maybe String), Options)
parseOptionsSrht argv = case res of
    O.Success (CommandSourcehut cmd, opts) -> return (cmd, opts)
    O.Success _ -> fail "parseOptionsSrht on non-sourcehut command"
    O.Failure f -> case O.renderFailure f "haskell-ci" of
        (help, _) -> hPutStrLn stderr help >> exitFailure
    O.CompletionInvoked _ -> exitFailure -- unexpected
  where
    res = O.execParserPure (O.prefs O.subparserInline) cliParserInfo argv
