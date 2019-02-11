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
import           Distribution.Types.VersionRange (VersionRange, anyVersion)
import           GHC.Generics                    (Generic)
import           Lens.Micro                      (over)

import qualified Data.ByteString                 as BS
import qualified Data.Map                        as M
import qualified Data.Set                        as S
import qualified Distribution.CabalSpecVersion   as C
import qualified Distribution.Compat.CharParsing as C
import qualified Distribution.Compat.Newtype     as C
import qualified Distribution.FieldGrammar       as C
import qualified Distribution.Parsec.Class       as C
import qualified Distribution.Parsec.Common      as C
import qualified Distribution.Parsec.Newtypes    as C
import qualified Distribution.Parsec.Parser      as C
import qualified Distribution.Parsec.ParseResult as C
import qualified Distribution.Pretty             as C
import qualified Distribution.Types.Version      as C
import qualified Text.PrettyPrint                as PP

import           HaskellCI.Config.ConstraintSet
import           HaskellCI.Config.Doctest
import           HaskellCI.Config.Folds
import           HaskellCI.Config.HLint
import           HaskellCI.Config.Jobs
import           HaskellCI.Newtypes
import           HaskellCI.ParsecUtils

-- TODO: split other blocks like DoctestConfig
data Config = Config
    { cfgCabalInstallVersion :: Maybe Version
    , cfgJobs                :: Maybe Jobs
    , cfgLocalGhcOptions     :: [String]
    , cfgCache               :: !Bool
    , cfgCheck               :: !Bool
    , cfgNoise               :: !Bool
    , cfgNoTestsNoBench      :: !Bool
    , cfgUnconstrainted      :: !Bool
    , cfgInstallDeps         :: !Bool
    , cfgHaddock             :: !VersionRange
    , cfgOnlyBranches        :: [String]
    , cfgIrcChannels         :: [String]
    , cfgProjectName         :: Maybe String
    , cfgFolds               :: S.Set Fold
    , cfgGhcHead             :: !Bool
    , cfgEnv                 :: M.Map Version String
    , cfgAllowFailures       :: S.Set Version -- TODO: change to range
    , cfgLastInSeries        :: !Bool
    , cfgOsx                 :: S.Set Version
    , cfgApt                 :: S.Set String
    , cfgDoctest             :: !DoctestConfig
    , cfgHLint               :: !HLintConfig
    , cfgConstraintSets      :: [ConstraintSet]
    }
  deriving (Show, Generic)

defaultCabalInstallVersion :: Maybe Version
defaultCabalInstallVersion = Just (C.mkVersion [2,4])

emptyConfig :: Config
emptyConfig = Config
    { cfgCabalInstallVersion = defaultCabalInstallVersion
    , cfgJobs            = Nothing
    , cfgDoctest         = DoctestConfig
        { cfgDoctestEnabled = False
        , cfgDoctestOptions = []
        , cfgDoctestVersion = defaultDoctestVersion
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
    , cfgCheck           = True
    , cfgNoise           = True
    , cfgNoTestsNoBench  = True
    , cfgUnconstrainted  = True
    , cfgInstallDeps     = True
    , cfgHaddock         = anyVersion
    , cfgOnlyBranches    = []
    , cfgIrcChannels     = []
    , cfgProjectName     = Nothing
    , cfgFolds           = S.empty
    , cfgGhcHead         = False
    , cfgEnv             = M.empty
    , cfgAllowFailures   = S.empty
    , cfgLastInSeries    = False
    , cfgOsx             = S.empty
    , cfgApt             = S.empty
    }

-------------------------------------------------------------------------------
-- Grammar
-------------------------------------------------------------------------------

configGrammar
    :: (C.FieldGrammar g, Applicative (g Config), Applicative (g DoctestConfig), Applicative (g HLintConfig))
    => g Config Config
configGrammar = Config
    <$> C.optionalFieldDefAla "cabal-install-version"     HeadVersion                         #cfgCabalInstallVersion defaultCabalInstallVersion
    <*> C.optionalField       "jobs"                                                          #cfgJobs
    <*> C.monoidalFieldAla    "local-ghc-options"         (C.alaList' C.NoCommaFSep C.Token') #cfgLocalGhcOptions
    <*> C.booleanFieldDef     "cache"                                                         #cfgCache True
    <*> C.booleanFieldDef     "cabal-check"                                                   #cfgCheck True
    <*> C.booleanFieldDef     "cabal-noise"                                                   #cfgNoise True
    <*> C.booleanFieldDef     "no-tests-no-benchmarks"                                        #cfgNoTestsNoBench True
    <*> C.booleanFieldDef     "unconstrained-step"                                            #cfgUnconstrainted True
    <*> C.booleanFieldDef     "install-dependencies-step"                                     #cfgInstallDeps True
    <*> C.optionalFieldDef    "haddock"                                                       #cfgHaddock anyVersion
    <*> C.monoidalFieldAla    "branches"                  (C.alaList' C.FSep C.Token')        #cfgOnlyBranches
    <*> C.monoidalFieldAla    "irc-channels"              (C.alaList' C.FSep C.Token')        #cfgIrcChannels
    <*> C.optionalFieldAla    "project-name"              C.Token'                            #cfgProjectName
    <*> C.monoidalFieldAla    "folds"                     Folds                               #cfgFolds
    <*> C.booleanFieldDef     "ghc-head"                                                      #cfgGhcHead False
    <*> C.monoidalFieldAla    "env"                       Env                                 #cfgEnv
    <*> C.monoidalFieldAla    "allow-failures"            (alaSet C.CommaFSep)                #cfgAllowFailures
    <*> C.booleanFieldDef     "last-in-series"                                                #cfgLastInSeries False
    <*> C.monoidalFieldAla    "osx"                       (alaSet C.NoCommaFSep)              #cfgOsx
    <*> C.monoidalFieldAla    "apt"                       (alaSet' C.NoCommaFSep C.Token')    #cfgApt
    <*> C.blurFieldGrammar #cfgDoctest doctestConfigGrammar
    <*> C.blurFieldGrammar #cfgHLint   hlintConfigGrammar
    <*> pure []

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
            return $ over #cfgConstraintSets (++ [cs])
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
