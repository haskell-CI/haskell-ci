{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
module HaskellCI.Config where

import HaskellCI.Prelude

import Distribution.Simple.Utils (fromUTF8BS)

import qualified Data.ByteString                 as BS
import qualified Data.Map                        as M
import qualified Data.Set                        as S
import qualified Distribution.CabalSpecVersion   as C
import qualified Distribution.Compat.CharParsing as C
import qualified Distribution.Compat.Newtype     as C
import qualified Distribution.FieldGrammar       as C
import qualified Distribution.Fields             as C
import qualified Distribution.Parsec             as C
import qualified Distribution.Parsec.Newtypes    as C
import qualified Distribution.Pretty             as C
import qualified Distribution.Types.PackageName  as C
import qualified Distribution.Types.Version      as C
import qualified Distribution.Types.VersionRange as C
import qualified Text.PrettyPrint                as PP

import HaskellCI.Config.ConstraintSet
import HaskellCI.Config.CopyFields
import HaskellCI.Config.Doctest
import HaskellCI.Config.Folds
import HaskellCI.Config.HLint
import HaskellCI.Config.Installed
import HaskellCI.Config.Jobs
import HaskellCI.Config.PackageScope
import HaskellCI.Config.Ubuntu
import HaskellCI.Newtypes
import HaskellCI.OptionsGrammar
import HaskellCI.ParsecUtils
import HaskellCI.TestedWith

defaultHeadHackage :: VersionRange
defaultHeadHackage = C.orLaterVersion (C.mkVersion [8,11])

-- TODO: split other blocks like DoctestConfig
data Config = Config
    { cfgCabalInstallVersion :: Maybe Version
    , cfgJobs                :: Maybe Jobs
    , cfgUbuntu              :: !Ubuntu
    , cfgTestedWith          :: !TestedWithJobs
    , cfgCopyFields          :: !CopyFields
    , cfgLocalGhcOptions     :: [String]
    , cfgSubmodules          :: !Bool
    , cfgCache               :: !Bool
    , cfgInstallDeps         :: !Bool
    , cfgInstalled           :: [Installed]
    , cfgTests               :: !VersionRange
    , cfgRunTests            :: !VersionRange
    , cfgBenchmarks          :: !VersionRange
    , cfgHaddock             :: !VersionRange
    , cfgNoTestsNoBench      :: !VersionRange
    , cfgUnconstrainted      :: !VersionRange
    , cfgHeadHackage         :: !VersionRange
    , cfgGhcjsTests          :: !Bool
    , cfgGhcjsTools          :: ![C.PackageName]
    , cfgTestOutputDirect    :: !Bool
    , cfgCheck               :: !Bool
    , cfgOnlyBranches        :: [String]
    , cfgIrcChannels         :: [String]
    , cfgEmailNotifications  :: Bool
    , cfgProjectName         :: Maybe String
    , cfgFolds               :: S.Set Fold
    , cfgGhcHead             :: !Bool
    , cfgPostgres            :: !Bool
    , cfgGoogleChrome        :: !Bool
    , cfgEnv                 :: M.Map Version String
    , cfgAllowFailures       :: !VersionRange
    , cfgLastInSeries        :: !Bool
    , cfgOsx                 :: S.Set Version
    , cfgApt                 :: S.Set String
    , cfgTravisPatches       :: [FilePath]
    , cfgGitHubPatches       :: [FilePath]
    , cfgInsertVersion       :: !Bool
    , cfgErrorMissingMethods :: !PackageScope
    , cfgDoctest             :: !DoctestConfig
    , cfgHLint               :: !HLintConfig
    , cfgConstraintSets      :: [ConstraintSet]
    , cfgRawProject          :: [C.PrettyField ()]
    , cfgRawTravis           :: !String
    }
  deriving (Generic)

defaultCabalInstallVersion :: Maybe Version
defaultCabalInstallVersion = Just (C.mkVersion [3,4])

emptyConfig :: Config
emptyConfig = Config
    { cfgCabalInstallVersion = defaultCabalInstallVersion
    , cfgJobs            = Nothing
    , cfgUbuntu          = Xenial
    , cfgTestedWith      = TestedWithUniform
    , cfgCopyFields      = CopyFieldsSome
    , cfgDoctest         = DoctestConfig
        { cfgDoctestEnabled       = noVersion
        , cfgDoctestOptions       = []
        , cfgDoctestVersion       = defaultDoctestVersion
        , cfgDoctestFilterEnvPkgs = []
        , cfgDoctestFilterSrcPkgs = []
        }
    , cfgHLint = HLintConfig
        { cfgHLintEnabled  = False
        , cfgHLintJob      = HLintJobLatest
        , cfgHLintYaml     = Nothing
        , cfgHLintVersion  = defaultHLintVersion
        , cfgHLintOptions  = []
        , cfgHLintDownload = True
        }
    , cfgLocalGhcOptions = []
    , cfgConstraintSets  = []
    , cfgSubmodules      = False
    , cfgCache           = True
    , cfgInstalled       = []
    , cfgInstallDeps     = True
    , cfgTests           = anyVersion
    , cfgRunTests        = anyVersion
    , cfgBenchmarks      = anyVersion
    , cfgHaddock         = anyVersion
    , cfgNoTestsNoBench  = anyVersion
    , cfgUnconstrainted  = anyVersion
    , cfgHeadHackage     = defaultHeadHackage
    , cfgGhcjsTests      = False
    , cfgGhcjsTools      = []
    , cfgTestOutputDirect = True
    , cfgCheck           = True
    , cfgOnlyBranches    = []
    , cfgIrcChannels     = []
    , cfgEmailNotifications = True
    , cfgProjectName     = Nothing
    , cfgFolds           = S.empty
    , cfgGhcHead         = False
    , cfgPostgres        = False
    , cfgGoogleChrome    = False
    , cfgEnv             = M.empty
    , cfgAllowFailures   = noVersion
    , cfgLastInSeries    = False
    , cfgOsx             = S.empty
    , cfgApt             = S.empty
    , cfgTravisPatches   = []
    , cfgGitHubPatches   = []
    , cfgInsertVersion   = True
    , cfgRawProject      = []
    , cfgRawTravis       = ""
    , cfgErrorMissingMethods = PackageScopeLocal
    }

-------------------------------------------------------------------------------
-- Grammar
-------------------------------------------------------------------------------

configGrammar
    :: (OptionsGrammar g, Applicative (g Config), Applicative (g DoctestConfig), Applicative (g HLintConfig))
    => g Config Config
configGrammar = Config
    <$> C.optionalFieldDefAla "cabal-install-version"     HeadVersion                         (field @"cfgCabalInstallVersion") defaultCabalInstallVersion
        ^^^ metahelp "VERSION" "cabal-install version for all jobs"
    <*> C.optionalField       "jobs"                                                          (field @"cfgJobs")
        ^^^ metahelp "JOBS" "jobs (N:M - cabal:ghc)"
    <*> C.optionalFieldDef    "distribution"                                                  (field @"cfgUbuntu") Xenial
        ^^^ metahelp "DIST" "distribution version (xenial, bionic)"
    <*> C.optionalFieldDef    "jobs-selection"                                                (field @"cfgTestedWith") TestedWithUniform
        ^^^ metahelp "uniform|any" "Jobs selection across packages"
    <*> C.optionalFieldDef    "copy-fields"                                                   (field @"cfgCopyFields") CopyFieldsSome
        ^^^ metahelp "none|some|all" "Copy ? fields from cabal.project fields"
    <*> C.monoidalFieldAla    "local-ghc-options"         (C.alaList' C.NoCommaFSep C.Token') (field @"cfgLocalGhcOptions")
        ^^^ metahelp "OPTS" "--ghc-options for local packages"
    <*> C.booleanFieldDef     "submodules"                                                    (field @"cfgSubmodules") False
        ^^^ help "Clone submodules, i.e. recursively"
    <*> C.booleanFieldDef     "cache"                                                         (field @"cfgCache") True
        ^^^ help "Disable caching"
    <*> C.booleanFieldDef     "install-dependencies"                                          (field @"cfgInstallDeps") True
        ^^^ help "Skip separate dependency installation step"
    <*> C.monoidalFieldAla    "installed"                 (C.alaList C.FSep)                  (field @"cfgInstalled")
        ^^^ metahelp "+/-PKG" "Specify 'constraint: ... installed' packages"
    <*> rangeField            "tests"                                                         (field @"cfgTests") anyVersion
        ^^^ metahelp "RANGE" "Build tests with"
    <*> rangeField            "run-tests"                                                     (field @"cfgRunTests") anyVersion
        ^^^ metahelp "RANGE" "Run tests with (note: only built tests are run)"
    <*> rangeField           "benchmarks"                                                     (field @"cfgBenchmarks") anyVersion
        ^^^ metahelp "RANGE" "Build benchmarks"
    <*> rangeField           "haddock"                                                        (field @"cfgHaddock") anyVersion
        ^^^ metahelp "RANGE" "Haddock step"
    <*> rangeField           "no-tests-no-benchmarks"                                         (field @"cfgNoTestsNoBench") anyVersion
        ^^^ metahelp "RANGE" "Build without tests and benchmarks"
    <*> rangeField            "unconstrained"                                                 (field @"cfgUnconstrainted") anyVersion
        ^^^ metahelp "RANGE" "Make unconstrained build"
    <*> rangeField            "head-hackage"                                                  (field @"cfgHeadHackage") defaultHeadHackage
        ^^^ metahelp "RANGE" "Use head.hackage repository. Also marks as allow-failures"
    <*> C.booleanFieldDef     "ghcjs-tests"                                                   (field @"cfgGhcjsTests") False
        ^^^ help "Run tests with GHCJS (experimental, relies on cabal-plan finding test-suites)"
    <*> C.monoidalFieldAla    "ghcjs-tools"               (C.alaList C.FSep)                  (field @"cfgGhcjsTools")
--        ^^^ metahelp "TOOL" "Additional host tools to install with GHCJS"
    <*> C.booleanFieldDef "test-output-direct"                                                (field @"cfgTestOutputDirect") True
        ^^^ help "Use --test-show-details=direct, may cause problems with build-type: Custom"
    <*> C.booleanFieldDef "cabal-check"                                                       (field @"cfgCheck") True
        ^^^ help "Disable cabal check run"
    <*> C.monoidalFieldAla    "branches"                  (C.alaList' C.FSep C.Token')        (field @"cfgOnlyBranches")
        ^^^ metahelp "BRANCH" "Enable builds only for specific branches"
    <*> C.monoidalFieldAla    "irc-channels"              (C.alaList' C.FSep C.Token')        (field @"cfgIrcChannels")
        ^^^ metahelp "IRC" "Enable IRC notifications to given channel (e.g. 'irc.freenode.org#haskell-lens')"
    <*> C.booleanFieldDef "email-notifications"                                               (field @"cfgEmailNotifications") True
        ^^^ help "Disable email notifications"
    <*> C.optionalFieldAla    "project-name"              C.Token'                            (field @"cfgProjectName")
        ^^^ metahelp "NAME" "Project name (used for IRC notifications), defaults to package name or name of first package listed in cabal.project file"
    <*> C.monoidalFieldAla    "folds"                     Folds                               (field @"cfgFolds")
        ^^^ metahelp "FOLD" "Build steps to fold"
    <*> C.booleanFieldDef     "ghc-head"                                                      (field @"cfgGhcHead") False
        ^^^ help "Add ghc-head job"
    <*> C.booleanFieldDef     "postgresql"                                                    (field @"cfgPostgres") False
        ^^^ help "Add postgresql service"
    <*> C.booleanFieldDef     "google-chrome"                                                 (field @"cfgGoogleChrome") False
        ^^^ help "Add google-chrome service"
    <*> C.monoidalFieldAla    "env"                       Env                                 (field @"cfgEnv")
        ^^^ metahelp "ENV" "Environment variables per job (e.g. `8.0.2:HADDOCK=false`)"
    <*> C.optionalFieldDefAla "allow-failures"            Range                               (field @"cfgAllowFailures") noVersion
        ^^^ metahelp "JOB" "Allow failures of particular GHC version"
    <*> C.booleanFieldDef     "last-in-series"                                                (field @"cfgLastInSeries") False
        ^^^ help "[Discouraged] Assume there are only GHCs last in major series: 8.2.* will match only 8.2.2"
    <*> C.monoidalFieldAla    "osx"                       (alaSet C.NoCommaFSep)              (field @"cfgOsx")
        ^^^ metahelp "JOB" "Jobs to additionally build with OSX"
    <*> C.monoidalFieldAla    "apt"                       (alaSet' C.NoCommaFSep C.Token')    (field @"cfgApt")
        ^^^ metahelp "PKG" "Additional apt packages to install"
    <*> C.monoidalFieldAla    "travis-patches"            (C.alaList' C.NoCommaFSep C.Token') (field @"cfgTravisPatches")
        ^^^ metahelp "PATCH" ".patch files to apply to the generated Travis YAML file"
    <*> C.monoidalFieldAla    "github-patches"            (C.alaList' C.NoCommaFSep C.Token') (field @"cfgGitHubPatches")
        ^^^ metahelp "PATCH" ".patch files to apply to the generated GitHub Actions YAML file"
    <*> C.booleanFieldDef "insert-version"                                                    (field @"cfgInsertVersion") True
        ^^^ help "Don't insert the haskell-ci version into the generated Travis YAML file"
    <*> C.optionalFieldDef "error-missing-methods"                                            (field @"cfgErrorMissingMethods") PackageScopeLocal
        ^^^ metahelp "PKGSCOPE" "Insert -Werror=missing-methods for package scope (none, local, all)"
    <*> C.blurFieldGrammar (field @"cfgDoctest") doctestConfigGrammar
    <*> C.blurFieldGrammar (field @"cfgHLint")   hlintConfigGrammar
    <*> pure [] -- constraint sets
    <*> pure [] -- raw project fields
    <*> C.freeTextFieldDef "raw-travis"                                                       (field @"cfgRawTravis")
        ^^^ help "Raw travis commands which will be run at the very end of the script"

-------------------------------------------------------------------------------
-- Reading
-------------------------------------------------------------------------------

readConfigFile :: MonadIO m => FilePath -> m Config
readConfigFile = liftIO . readAndParseFile parseConfigFile

parseConfigFile :: [C.Field C.Position] -> C.ParseResult Config
parseConfigFile fields0 = do
    config <- C.parseFieldGrammar C.cabalSpecLatest fields configGrammar
    config' <- traverse parseSection $ concat sections
    return (foldl' (&) config config')
  where
    (fields, sections) = C.partitionFields fields0

    parseSection :: C.Section C.Position -> C.ParseResult (Config -> Config)
    parseSection (C.MkSection (C.Name pos name) args cfields)
        | name == "constraint-set" = do
            name' <- parseName pos args
            let (fs, _sections) = C.partitionFields cfields
            cs <- C.parseFieldGrammar C.cabalSpecLatest fs (constraintSetGrammar name')
            return $ over (field @"cfgConstraintSets") (cs :)
        | name == "raw-project" = do
            let fs = C.fromParsecFields cfields
            return $ over (field @"cfgRawProject") (++ map void fs)
        | otherwise = do
            C.parseWarning pos C.PWTUnknownSection $ "Unknown section " ++ fromUTF8BS name
            return id

-------------------------------------------------------------------------------
-- Env
-------------------------------------------------------------------------------

newtype Env = Env (M.Map Version String)
  deriving anyclass (C.Newtype (M.Map Version String))

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
