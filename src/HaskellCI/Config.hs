{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
module HaskellCI.Config where

import HaskellCI.Prelude

import qualified Data.ByteString                 as BS
import qualified Data.Map                        as M
import qualified Data.Set                        as S
import qualified Distribution.CabalSpecVersion   as C
import qualified Distribution.Compat.CharParsing as C
import qualified Distribution.Compat.Newtype     as C
import qualified Distribution.FieldGrammar       as C
import qualified Distribution.Fields             as C
import qualified Distribution.Parsec             as C
import qualified Distribution.Pretty             as C
import qualified Distribution.Types.PackageName  as C
import qualified Distribution.Types.VersionRange as C
import qualified Text.PrettyPrint                as PP

import HaskellCI.Cabal
import HaskellCI.Config.Components
import HaskellCI.Config.ConstraintSet
import HaskellCI.Config.CopyFields
import HaskellCI.Config.Docspec
import HaskellCI.Config.Doctest
import HaskellCI.Config.Empty
import HaskellCI.Config.Folds
import HaskellCI.Config.HLint
import HaskellCI.Config.Installed
import HaskellCI.Config.Jobs
import HaskellCI.Config.PackageScope
import HaskellCI.Config.Ubuntu
import HaskellCI.Ghcup
import HaskellCI.HeadHackage
import HaskellCI.Newtypes
import HaskellCI.OptionsGrammar
import HaskellCI.ParsecUtils
import HaskellCI.TestedWith

-------------------------------------------------------------------------------
-- Config
-------------------------------------------------------------------------------

-- TODO: split other blocks like DoctestConfig
data Config = Config
    { cfgCabalInstallVersion :: Maybe Version
    , cfgJobs                :: Maybe Jobs
    , cfgUbuntu              :: !Ubuntu
    , cfgTestedWith          :: !TestedWithJobs
    , cfgEnabledJobs         :: !VersionRange
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
    , cfgHaddockComponents   :: !Components
    , cfgNoTestsNoBench      :: !VersionRange
    , cfgUnconstrainted      :: !VersionRange
    , cfgHeadHackage         :: !VersionRange
    , cfgHeadHackageOverride :: !Bool
    , cfgGhcjsTests          :: !Bool
    , cfgGhcjsTools          :: ![C.PackageName]
    , cfgTestOutputDirect    :: !Bool
    , cfgCheck               :: !Bool
    , cfgOnlyBranches        :: [String]
    , cfgIrcChannels         :: [String]
    , cfgIrcNickname         :: Maybe String
    , cfgIrcPassword         :: Maybe String
    , cfgIrcIfInOriginRepo   :: Bool
    , cfgEmailNotifications  :: Bool
    , cfgProjectName         :: Maybe String
    , cfgFolds               :: S.Set Fold
    , cfgGhcHead             :: !Bool
    , cfgPostgres            :: !Bool
    , cfgGoogleChrome        :: !Bool
    , cfgEnv                 :: M.Map Version String
    , cfgMatrixExtra         :: M.Map String (S.Set String)
    , cfgAllowFailures       :: !VersionRange
    , cfgLastInSeries        :: !Bool
    , cfgLinuxJobs           :: !VersionRange
    , cfgMacosJobs           :: !VersionRange
    , cfgGhcupCabal          :: !Bool
    , cfgGhcupJobs           :: !VersionRange
    , cfgGhcupVersion        :: !Version
    , cfgApt                 :: S.Set String
    , cfgTravisPatches       :: [FilePath]
    , cfgGitHubPatches       :: [FilePath]
    , cfgInsertVersion       :: !Bool
    , cfgErrorMissingMethods :: !PackageScope
    , cfgDoctest             :: !DoctestConfig
    , cfgDocspec             :: !DocspecConfig
    , cfgHLint               :: !HLintConfig
    , cfgConstraintSets      :: [ConstraintSet]
    , cfgRawProject          :: [C.PrettyField ()]
    , cfgRawTravis           :: !String
    , cfgGitHubActionName    :: !(Maybe String)
    , cfgTimeoutMinutes      :: !Natural
    }
  deriving (Generic)

emptyConfig :: Config
emptyConfig = case runEG configGrammar of
    Left xs -> error $ "Required fields: " ++ show xs
    Right x -> x

-------------------------------------------------------------------------------
-- Grammar
-------------------------------------------------------------------------------

configGrammar
    :: ( OptionsGrammar c g, Applicative (g Config)
       , c (Identity HLintJob)
       , c (Identity PackageScope)
       , c (Identity TestedWithJobs)
       , c (Identity Ubuntu)
       , c (Identity Jobs)
       , c (Identity CopyFields)
       , c (Identity Version)
       , c (Identity Natural)
       , c (Identity Components)
       , c Env, c MatrixExtra, c Folds, c CopyFields, c HeadVersion
       , c (C.List C.FSep (Identity Installed) Installed)
       , Applicative (g DoctestConfig)
       , Applicative (g DocspecConfig)
       , Applicative (g HLintConfig))
    => g Config Config
configGrammar = Config
    <$> C.optionalFieldDefAla "cabal-install-version"     HeadVersion                         (field @"cfgCabalInstallVersion") defaultCabalInstallVersion
        ^^^ metahelp "VERSION" "cabal-install version for all jobs"
    <*> C.optionalField       "jobs"                                                          (field @"cfgJobs")
        ^^^ metahelp "JOBS" "jobs (N:M - cabal:ghc)"
    <*> C.optionalFieldDef    "distribution"                                                  (field @"cfgUbuntu") Bionic
        ^^^ metahelp "DIST" (concat
              [ "distribution version ("
              , intercalate ", " $ map showUbuntu [minBound..maxBound]
              , ")"
              ])
    <*> C.optionalFieldDef    "jobs-selection"                                                (field @"cfgTestedWith") TestedWithUniform
        ^^^ metahelp "uniform|any" "Jobs selection across packages"
    <*> rangeField            "enabled"                                                       (field @"cfgEnabledJobs") anyVersion
        ^^^ metahelp "RANGE" "Restrict jobs selection futher from per package tested-with"
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
    <*> C.optionalFieldDef   "haddock-components"                                             (field @"cfgHaddockComponents") ComponentsAll
        ^^^ metahelp "all|libs" "Haddock components"
    <*> rangeField           "no-tests-no-benchmarks"                                         (field @"cfgNoTestsNoBench") anyVersion
        ^^^ metahelp "RANGE" "Build without tests and benchmarks"
    <*> rangeField            "unconstrained"                                                 (field @"cfgUnconstrainted") anyVersion
        ^^^ metahelp "RANGE" "Make unconstrained build"
    <*> rangeField            "head-hackage"                                                  (field @"cfgHeadHackage") defaultHeadHackage
        ^^^ metahelp "RANGE" "Use head.hackage repository. Also marks as allow-failures"
    <*> C.booleanFieldDef     "head-hackage-override"                                         (field @"cfgHeadHackageOverride") True
        ^^^ help "Use :override for head.hackage repository"
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
        ^^^ metahelp "IRC" "Enable IRC notifications to given channel (e.g. 'irc.libera.chat#haskell-lens')"
    <*> C.freeTextField       "irc-nickname"                                                  (field @"cfgIrcNickname")
        ^^^ metahelp "NICKNAME" "Nickname with which to authenticate to an IRC server. Only used if `irc-channels` are set."
    <*> C.freeTextField       "irc-password"                                                  (field @"cfgIrcPassword")
        ^^^ metahelp "PASSWORD" "Password with which to authenticate to an IRC server. Only used if `irc-channels` are set."
    <*> C.booleanFieldDef     "irc-if-in-origin-repo"                                         (field @"cfgIrcIfInOriginRepo") False
        ^^^ help "Only send IRC notifications if run from the original remote (GitHub Actions only)"
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
    <*> C.monoidalFieldAla    "matrix-extra"              MatrixExtra                         (field @"cfgMatrixExtra")
        ^^^ metahelp "MATRIX" "Extra matrix dimensions (e.g. `libfoo:2.6,3.0,git`)"
    <*> C.optionalFieldDefAla "allow-failures"            Range                               (field @"cfgAllowFailures") noVersion
        ^^^ metahelp "JOB" "Allow failures of particular GHC version"
    <*> C.booleanFieldDef     "last-in-series"                                                (field @"cfgLastInSeries") False
        ^^^ help "[Discouraged] Assume there are only GHCs last in major series: 8.2.* will match only 8.2.2"
    <*> rangeField            "linux-jobs"                                                    (field @"cfgLinuxJobs") anyVersion
        ^^^ metahelp "RANGE" "Jobs to build on Linux"
    <*> rangeField            "macos-jobs"                                                    (field @"cfgMacosJobs") noVersion
        ^^^ metahelp "RANGE" "Jobs to additionally build with OSX"
    <*> C.booleanFieldDef     "ghcup-cabal"                                                   (field @"cfgGhcupCabal") True
        ^^^ help "Use (or don't) ghcup to install cabal"
    <*> rangeField            "ghcup-jobs"                                                    (field @"cfgGhcupJobs") (C.unionVersionRanges (C.intersectVersionRanges (C.laterVersion (mkVersion [8,10,4])) (C.earlierVersion (mkVersion [9]))) (C.laterVersion (mkVersion [9,0,1])))
        ^^^ metahelp "RANGE" "(Linux) jobs to use ghcup to install tools"
    <*> C.optionalFieldDef    "ghcup-version"                                                 (field @"cfgGhcupVersion") defaultGhcupVersion
        ^^^ metahelp "VERSION" "ghcup version"
    <*> C.monoidalFieldAla    "apt"                       (alaSet' C.NoCommaFSep C.Token')    (field @"cfgApt")
        ^^^ metahelp "PKG" "Additional apt packages to install"
    <*> C.monoidalFieldAla    "travis-patches"            (C.alaList' C.NoCommaFSep C.Token') (field @"cfgTravisPatches")
        ^^^ metaActionHelp "PATCH" "file" ".patch files to apply to the generated Travis YAML file"
    <*> C.monoidalFieldAla    "github-patches"            (C.alaList' C.NoCommaFSep C.Token') (field @"cfgGitHubPatches")
        ^^^ metaActionHelp "PATCH" "file" ".patch files to apply to the generated GitHub Actions YAML file"
    <*> C.booleanFieldDef "insert-version"                                                    (field @"cfgInsertVersion") True
        ^^^ help "Don't insert the haskell-ci version into the generated Travis YAML file"
    <*> C.optionalFieldDef "error-missing-methods"                                            (field @"cfgErrorMissingMethods") PackageScopeLocal
        ^^^ metahelp "PKGSCOPE" "Insert -Werror=missing-methods for package scope (none, local, all)"
    <*> C.blurFieldGrammar (field @"cfgDoctest") doctestConfigGrammar
    <*> C.blurFieldGrammar (field @"cfgDocspec") docspecConfigGrammar
    <*> C.blurFieldGrammar (field @"cfgHLint")   hlintConfigGrammar
    <*> pure [] -- constraint sets
    <*> pure [] -- raw project fields
    <*> C.freeTextFieldDef "raw-travis"                                                       (field @"cfgRawTravis")
        ^^^ help "Raw travis commands which will be run at the very end of the script"
    <*> C.freeTextField "github-action-name"                                                  (field @"cfgGitHubActionName")
        ^^^ help "The name of GitHub Action"
    <*> C.optionalFieldDef    "timeout-minutes"                                              (field @"cfgTimeoutMinutes") 60
        ^^^ metahelp "MINUTES" "The maximum number of minutes to let a job run"

-------------------------------------------------------------------------------
-- Reading
-------------------------------------------------------------------------------

readConfigFile :: MonadIO m => FilePath -> m Config
readConfigFile = liftIO . readAndParseFile parseConfigFile

parseConfigFile :: [C.Field C.Position] -> C.ParseResult Config
parseConfigFile fields0 = do
    config <- C.parseFieldGrammar C.cabalSpecLatest fields configGrammar
    config' <- traverse parseSection $ concat sections
    return $ postprocess $ foldl' (&) config config'
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

    postprocess :: Config -> Config
    postprocess cfg
        -- on yammy the only install option is ghcup
        | cfgUbuntu cfg >= Jammy = cfg { cfgGhcupJobs = anyVersion }
        | otherwise              = cfg

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
-- MatrixExtra
-------------------------------------------------------------------------------

newtype MatrixExtra = MatrixExtra (M.Map String (S.Set String))
  deriving anyclass (C.Newtype (M.Map String (S.Set String)))

instance C.Parsec MatrixExtra where
  parsec = MatrixExtra . M.fromList . toList <$> C.sepByNonEmpty p (C.char ';')
    where
    p = do
      k <- C.munch1 (/= ':')
      _ <- C.char ':'
      v <- foldMap S.singleton <$> C.sepByNonEmpty (C.munch1 (`notElem` [',', ';'])) (C.char ',')
      pure (k, v)

instance C.Pretty MatrixExtra where
  pretty (MatrixExtra m) = PP.fsep . PP.punctuate PP.semi . map p . M.toList $ m where
    p (k, v) = PP.text k PP.<> PP.colon PP.<> PP.fsep (PP.punctuate PP.comma (map PP.text (toList v)))


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
