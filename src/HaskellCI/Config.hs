{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
module HaskellCI.Config where

import           Data.Coerce                     (coerce)
import           Data.Generics.Labels            ()
import           Distribution.Version
import           GHC.Generics                    (Generic)

import qualified Data.Map                        as M
import qualified Data.Set                        as S
import qualified Distribution.Compat.CharParsing as C
import qualified Distribution.Compat.Newtype     as C
import qualified Distribution.FieldGrammar       as C
import qualified Distribution.Parsec.Class       as C
import qualified Distribution.Parsec.Newtypes    as C
import qualified Distribution.Pretty             as C
import qualified Distribution.Types.Version      as C
import qualified Text.PrettyPrint                as PP

import           HaskellCI.Config.ConstraintSet
import           HaskellCI.Config.Doctest
import           HaskellCI.Config.Folds
import           HaskellCI.Config.HLint
import           HaskellCI.Config.Jobs
import           HaskellCI.Newtypes

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
    , cfgOnlyBranches        :: [String]
    , cfgIrcChannels         :: [String]
    , cfgProjectName         :: Maybe String
    , cfgFolds               :: S.Set Fold
    , cfgGhcHead             :: !Bool
    , cfgEnv                 :: M.Map Version String
    , cfgAllowFailures       :: S.Set Version -- TODO: change to range
    , cfgLastInSeries        :: !Bool
    , cfgDoctest             :: !DoctestConfig
    , cfgHLint               :: !HLintConfig
    , cfgConstraintSets      :: [ConstraintSet]
    -- TODO: apt-packages
    -- TOOD: osx
    -- "generate osx build job with ghc version"
    }
  deriving (Show, Generic)

emptyConfig :: Config
emptyConfig = Config
    { cfgCabalInstallVersion = Nothing
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
    , cfgOnlyBranches    = []
    , cfgIrcChannels     = []
    , cfgProjectName     = Nothing
    , cfgFolds           = S.empty
    , cfgGhcHead         = False
    , cfgEnv             = M.empty
    , cfgAllowFailures   = S.empty
    , cfgLastInSeries    = False
    }

-------------------------------------------------------------------------------
-- Grammar
-------------------------------------------------------------------------------

configGrammar
    :: (C.FieldGrammar g, Applicative (g Config), Applicative (g DoctestConfig), Applicative (g HLintConfig))
    => g Config Config
configGrammar = Config
    <$> C.optionalFieldDefAla "cabal-install-version"     HeadVersion                         #cfgCabalInstallVersion (Just $ C.mkVersion [2,4])
    <*> C.optionalField       "jobs"                                                          #cfgJobs
    <*> C.monoidalFieldAla    "local-ghc-options"         (C.alaList' C.NoCommaFSep C.Token') #cfgLocalGhcOptions
    <*> C.booleanFieldDef     "cache"                                                         #cfgCache True
    <*> C.booleanFieldDef     "cabal-check"                                                   #cfgCheck True
    <*> C.booleanFieldDef     "cabal-noise"                                                   #cfgNoise True
    <*> C.booleanFieldDef     "no-tests-no-benchmarks"                                        #cfgNoTestsNoBench True
    <*> C.booleanFieldDef     "unconstrained-step"                                            #cfgUnconstrainted True
    <*> C.booleanFieldDef     "install-dependencies-step"                                     #cfgInstallDeps True
    <*> C.monoidalFieldAla    "branches"                  (C.alaList' C.FSep C.Token')        #cfgOnlyBranches
    <*> C.monoidalFieldAla    "irc-channels"              (C.alaList' C.FSep C.Token')        #cfgIrcChannels
    <*> C.optionalFieldAla    "project-name"              C.Token'                            #cfgProjectName
    <*> C.monoidalFieldAla    "folds"                     Folds                               #cfgFolds
    <*> C.booleanFieldDef     "ghc-head"                                                      #cfgGhcHead False
    <*> C.monoidalFieldAla    "env"                       Env                                 #cfgEnv
    <*> C.monoidalFieldAla    "allow-failures"            (alaSet C.CommaFSep)                #cfgAllowFailures
    <*> C.booleanFieldDef     "last-in-series"                                                #cfgLastInSeries False
    <*> C.blurFieldGrammar #cfgDoctest doctestConfigGrammar
    <*> C.blurFieldGrammar #cfgHLint   hlintConfigGrammar
    <*> pure []

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

