-- | Most of client interface.
module HaskellCI.Cli where

import           Prelude                  ()
import           Prelude.Compat

import           Control.Applicative      ((<|>))
import           System.Exit              (exitFailure)
import           System.IO                (hPutStrLn, stderr)

import qualified Options.Applicative      as O

import           HaskellCI.Config
import           HaskellCI.OptionsGrammar

-------------------------------------------------------------------------------
-- Command
-------------------------------------------------------------------------------

data Command
    = CommandTravis FilePath
    | CommandRegenerate
    | CommandListGHC
  deriving Show

-------------------------------------------------------------------------------
-- Options
-------------------------------------------------------------------------------

data Options = Options
    { optCollections    :: [String]
    , optOutput         :: Maybe FilePath
    , optConfig         :: Maybe FilePath
    , optConfigMorphism :: Config -> Config
    }

instance Semigroup Options where
    Options a b d e <> Options a' b' d' e' =
        Options (a <> a') (b <|> b') (d <|> d') (e' . e)

defaultOptions :: Options
defaultOptions = Options
    { optCollections    = []
    , optOutput         = Nothing
    , optConfig         = Nothing
    , optConfigMorphism = id
    }

-------------------------------------------------------------------------------
-- Parsers
-------------------------------------------------------------------------------

optionsP :: O.Parser Options
optionsP = Options
    <$> pure []
    <*> O.optional (O.strOption (O.long "output" <> O.short 'o' <> O.metavar "FILE" <> O.help "Optput file (stdout if omitted)"))
    <*> O.optional (O.strOption (O.long "config" <> O.metavar "CONFIGFILE" <> O.help "Configuration file"))
    <*> runOptionsGrammar configGrammar

cliParserInfo :: O.ParserInfo (Command, Options)
cliParserInfo = O.info ((,) <$> cmdP <*> optionsP O.<**> O.helper) $ mconcat
    [ O.fullDesc
    , O.header "haskell-ci - generate CI scripts for Haskell projects"
    ]
  where
    cmdP = O.subparser (mconcat
        [ O.command "regenerate" $ O.info (pure CommandRegenerate) $ O.progDesc "Regenerate .travis.yml"
        , O.command "travis"     $ O.info travisP                  $ O.progDesc "Generate travis-ci config"
        , O.command "list-ghc"   $ O.info (pure CommandListGHC)    $ O.progDesc "List known GHC versions"
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


