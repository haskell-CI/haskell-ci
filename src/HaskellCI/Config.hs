{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
module HaskellCI.Config where

import           Control.Monad.IO.Class          (MonadIO (..))
import           Data.Coerce                     (coerce)
import           Data.Generics.Labels            ()
import           Distribution.Simple.Utils       (fromUTF8BS)
import           Distribution.Types.Version      (Version)
import           Distribution.Types.VersionRange (VersionRange, anyVersion,
                                                  noVersion)
import           GHC.Generics                    (Generic)
import           Lens.Micro                      (over)

import qualified Data.ByteString                 as BS
import qualified Data.Map                        as M
import qualified Data.Set                        as S
import qualified Distribution.CabalSpecVersion   as C
import qualified Distribution.Compat.CharParsing as C
import qualified Distribution.Compat.Newtype     as C
import qualified Distribution.FieldGrammar       as C
import qualified Distribution.Fields.Pretty      as C
import qualified Distribution.Parsec.Class       as C
import qualified Distribution.Parsec.Common      as C
import qualified Distribution.Parsec.Newtypes    as C
import qualified Distribution.Parsec.Parser      as C
import qualified Distribution.Parsec.ParseResult as C
import qualified Distribution.Pretty             as C
import qualified Distribution.Types.Version      as C
import qualified Text.PrettyPrint                as PP

import           HaskellCI.Config.ConstraintSet
import           HaskellCI.Config.CopyFields
import           HaskellCI.Config.Doctest
import           HaskellCI.Config.Folds
import           HaskellCI.Config.HLint
import           HaskellCI.Config.Installed
import           HaskellCI.Config.Jobs
import           HaskellCI.Newtypes
import           HaskellCI.OptionsGrammar
import           HaskellCI.ParsecUtils
import           HaskellCI.TestedWith

-- TODO: split other blocks like DoctestConfig
data Config = Config
    { cfgCabalInstallVersion :: Maybe Version
    , cfgJobs                :: Maybe Jobs
    , cfgTestedWith          :: !TestedWithJobs
    , cfgCopyFields          :: !CopyFields
    , cfgLocalGhcOptions     :: [String]
    , cfgCache               :: !Bool
    , cfgInstallDeps         :: !Bool
    , cfgInstalled           :: [Installed]
    , cfgTests               :: !VersionRange
    , cfgRunTests            :: !VersionRange
    , cfgBenchmarks          :: !VersionRange
    , cfgHaddock             :: !VersionRange
    , cfgNoTestsNoBench      :: !VersionRange
    , cfgUnconstrainted      :: !VersionRange
    , cfgCheck               :: !Bool
    , cfgOnlyBranches        :: [String]
    , cfgIrcChannels         :: [String]
    , cfgProjectName         :: Maybe String
    , cfgFolds               :: S.Set Fold
    , cfgGhcHead             :: !Bool
    , cfgPostgres            :: !Bool
    , cfgEnv                 :: M.Map Version String
    , cfgAllowFailures       :: !VersionRange
    , cfgLastInSeries        :: !Bool
    , cfgOsx                 :: S.Set Version
    , cfgApt                 :: S.Set String
    , cfgTravisPatches       :: [FilePath]
    , cfgInsertVersion       :: !Bool
    , cfgDoctest             :: !DoctestConfig
    , cfgHLint               :: !HLintConfig
    , cfgConstraintSets      :: [ConstraintSet]
    , cfgRawProject          :: [C.PrettyField]
    }
  deriving (Show, Generic)

defaultCabalInstallVersion :: Maybe Version
defaultCabalInstallVersion = Just (C.mkVersion [2,4])

emptyConfig :: Config
emptyConfig = Config
    { cfgCabalInstallVersion = defaultCabalInstallVersion
    , cfgJobs            = Nothing
    , cfgTestedWith      = TestedWithUniform
    , cfgCopyFields      = CopyFieldsSome
    , cfgDoctest         = DoctestConfig
        { cfgDoctestEnabled    = noVersion
        , cfgDoctestOptions    = []
        , cfgDoctestVersion    = defaultDoctestVersion
        , cfgDoctestFilterPkgs = []
        }
    , cfgHLint = HLintConfig
        { cfgHLintEnabled = False
        , cfgHLintJob     = HLintJobLatest
        , cfgHLintYaml    = Nothing
        , cfgHLintVersion = defaultHLintVersion
        , cfgHLintOptions = []
        }
    , cfgLocalGhcOptions = []
    , cfgConstraintSets  = []
    , cfgCache           = True
    , cfgInstalled       = []
    , cfgInstallDeps     = True
    , cfgTests           = anyVersion
    , cfgRunTests        = anyVersion
    , cfgBenchmarks      = anyVersion
    , cfgHaddock         = anyVersion
    , cfgNoTestsNoBench  = anyVersion
    , cfgUnconstrainted  = anyVersion
    , cfgCheck           = True
    , cfgOnlyBranches    = []
    , cfgIrcChannels     = []
    , cfgProjectName     = Nothing
    , cfgFolds           = S.empty
    , cfgGhcHead         = False
    , cfgPostgres        = False
    , cfgEnv             = M.empty
    , cfgAllowFailures   = noVersion
    , cfgLastInSeries    = False
    , cfgOsx             = S.empty
    , cfgApt             = S.empty
    , cfgTravisPatches   = []
    , cfgInsertVersion   = True
    , cfgRawProject      = []
    }

-------------------------------------------------------------------------------
-- Grammar
-------------------------------------------------------------------------------

configGrammar
    :: (OptionsGrammar g, Applicative (g Config), Applicative (g DoctestConfig), Applicative (g HLintConfig))
    => g Config Config
configGrammar = Config
    <$> C.optionalFieldDefAla "cabal-install-version"     HeadVersion                         #cfgCabalInstallVersion defaultCabalInstallVersion
        ^^^ metahelp "VERSION" "cabal-install version for all jobs"
    <*> C.optionalField       "jobs"                                                          #cfgJobs
        ^^^ metahelp "JOBS" "jobs (N:M - cabal:ghc)"
    <*> C.optionalFieldDef    "jobs-selection"                                                #cfgTestedWith TestedWithUniform
        ^^^ metahelp "uniform|any" "Jobs selection across packages"
    <*> C.optionalFieldDef    "copy-fields"                                                   #cfgCopyFields CopyFieldsSome
        ^^^ metahelp "none|some|all" "Copy ? fields from cabal.project fields"
    <*> C.monoidalFieldAla    "local-ghc-options"         (C.alaList' C.NoCommaFSep C.Token') #cfgLocalGhcOptions
        ^^^ metahelp "OPTS" "--ghc-options for local packages"
    <*> C.booleanFieldDef     "cache"                                                         #cfgCache True
        ^^^ help "Disable caching"
    <*> C.booleanFieldDef     "install-dependencies"                                          #cfgInstallDeps True
        ^^^ help "Skip separate dependency installation step"
    <*> C.monoidalFieldAla    "installed"                 (C.alaList C.FSep)                  #cfgInstalled
        ^^^ metahelp "+/-PKG" "Specify 'constraint: ... installed' packages"
    <*> rangeField            "tests"                                                         #cfgTests anyVersion
        ^^^ metahelp "RANGE" "Build tests with"
    <*> rangeField            "run-tests"                                                     #cfgRunTests anyVersion
        ^^^ metahelp "RANGE" "Run tests with (note: only built tests are run)"
    <*> rangeField           "benchmarks"                                                     #cfgBenchmarks anyVersion
        ^^^ metahelp "RANGE" "Build benchmarks"
    <*> rangeField           "haddock"                                                        #cfgHaddock anyVersion
        ^^^ metahelp "RANGE" "Haddock step"
    <*> rangeField           "no-tests-no-benchmarks"                                         #cfgNoTestsNoBench anyVersion
        ^^^ metahelp "RANGE" "Build without tests and benchmarks"
    <*> rangeField            "unconstrained"                                                 #cfgUnconstrainted anyVersion
        ^^^ metahelp "RANGE" "Make unconstrained build"
    <*> C.booleanFieldDef "cabal-check"                                                       #cfgCheck True
        ^^^ help "Disable cabal check run"
    <*> C.monoidalFieldAla    "branches"                  (C.alaList' C.FSep C.Token')        #cfgOnlyBranches
        ^^^ metahelp "BRANCH" "Enable builds only for specific branches"
    <*> C.monoidalFieldAla    "irc-channels"              (C.alaList' C.FSep C.Token')        #cfgIrcChannels
        ^^^ metahelp "IRC" "Enable IRC notifications to given channel (e.g. 'irc.freenode.org#haskell-lens')"
    <*> C.optionalFieldAla    "project-name"              C.Token'                            #cfgProjectName
        ^^^ metahelp "NAME" "Project name (used for IRC notifications), defaults to package name or name of first package listed in cabal.project file"
    <*> C.monoidalFieldAla    "folds"                     Folds                               #cfgFolds
        ^^^ metahelp "FOLD" "Build steps to fold"
    <*> C.booleanFieldDef     "ghc-head"                                                      #cfgGhcHead False
        ^^^ help "Add ghc-head job"
    <*> C.booleanFieldDef     "postgresql"                                                    #cfgPostgres False
        ^^^ help "Add postgresql service"
    <*> C.monoidalFieldAla    "env"                       Env                                 #cfgEnv
        ^^^ metahelp "ENV" "Environment variables per job (e.g. `8.0.2:HADDOCK=false`)"
    <*> C.optionalFieldDefAla "allow-failures"            Range                               #cfgAllowFailures noVersion
        ^^^ metahelp "JOB" "Allow failures of particular GHC version"
    <*> C.booleanFieldDef     "last-in-series"                                                #cfgLastInSeries False
        ^^^ help "[Discouraged] Assume there are only GHCs last in major series: 8.2.* will match only 8.2.2"
    <*> C.monoidalFieldAla    "osx"                       (alaSet C.NoCommaFSep)              #cfgOsx
        ^^^ metahelp "JOB" "Jobs to additionally build with OSX"
    <*> C.monoidalFieldAla    "apt"                       (alaSet' C.NoCommaFSep C.Token')    #cfgApt
        ^^^ metahelp "PKG" "Additional apt packages to install"
    <*> C.monoidalFieldAla    "travis-patches"            (C.alaList' C.NoCommaFSep C.Token') #cfgTravisPatches
        ^^^ metahelp "PATCH" ".patch files to apply to the generated Travis YAML file"
    <*> C.booleanFieldDef "insert-version"                                                    #cfgInsertVersion True
        ^^^ help "Don't insert the haskell-ci version into the generated Travis YAML file"
    <*> C.blurFieldGrammar #cfgDoctest doctestConfigGrammar
    <*> C.blurFieldGrammar #cfgHLint   hlintConfigGrammar
    <*> pure [] -- constraint sets
    <*> pure [] -- raw project fields

-------------------------------------------------------------------------------
-- Reading
-------------------------------------------------------------------------------

readConfigFile :: MonadIO m => FilePath -> m Config
readConfigFile = liftIO . readAndParseFile parseConfigFile

parseConfigFile :: [C.Field C.Position] -> C.ParseResult Config
parseConfigFile fields0 = do
    config <- C.parseFieldGrammar C.cabalSpecLatest fields configGrammar
    config' <- traverse parseSection $ concat sections
    return (foldr (.) id config' config)
  where
    (fields, sections) = C.partitionFields fields0

    parseSection :: C.Section C.Position -> C.ParseResult (Config -> Config)
    parseSection (C.MkSection (C.Name pos name) args cfields)
        | name == "constraint-set" = do
            name' <- parseName pos args
            let (fs, _sections) = C.partitionFields cfields
            cs <- C.parseFieldGrammar C.cabalSpecLatest fs (constraintSetGrammar name')
            return $ over #cfgConstraintSets (cs :)
        | name == "raw-project" = do
            let fs = C.fromParsecFields cfields
            return $ over #cfgRawProject (++ fs)
        | otherwise = do
            C.parseWarning pos C.PWTUnknownSection $ "Unknown section " ++ fromUTF8BS name
            return id

-------------------------------------------------------------------------------
-- Env
-------------------------------------------------------------------------------

newtype Env = Env (M.Map Version String)

instance C.Newtype Env (M.Map Version String) where
    pack = coerce
    unpack = coerce

instance C.Parsec Env where
    parsec = Env . M.fromList <$> C.parsecLeadingCommaList p where
        p = do
            v <- C.parsec
            _ <- C.char ':'
            s <- C.munch1 $ \c -> c /= ','
            return (v, s)

instance C.Pretty Env where
    pretty (Env m) = PP.fsep . PP.punctuate PP.comma . map p . M.toList $ m where
        p (v, s) = C.pretty v PP.<> PP.colon PP.<> PP.text s

-------------------------------------------------------------------------------
-- From Cabal
-------------------------------------------------------------------------------

parseName :: C.Position -> [C.SectionArg C.Position] -> C.ParseResult String
parseName pos args = fromUTF8BS <$> parseNameBS pos args

parseNameBS :: C.Position -> [C.SectionArg C.Position] -> C.ParseResult BS.ByteString
parseNameBS pos args = case args of
    [C.SecArgName _pos secName] ->
         pure secName
    [C.SecArgStr _pos secName] ->
         pure secName
    [] -> do
         C.parseFailure pos "name required"
         pure ""
    _ -> do
         -- TODO: pretty print args
         C.parseFailure pos $ "Invalid name " ++ show args
         pure ""
