{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

#if !defined(MIN_VERSION_Cabal)
-- As a heuristic, if the macro isn't defined, be pessimistic and
-- assume an "old" Cabal version
# define MIN_VERSION_Cabal(x,y,z) 0
#endif

-- | New-style @.travis.yml@ script generator using cabal 1.24's nix-style
-- tech-preview facilities.
--
-- See also <https://github.com/hvr/multi-ghc-travis>
--
-- NB: This code deliberately avoids relying on non-standard packages and
--     is expected to compile/work with at least GHC 7.0 through GHC 8.0
module MakeTravisYml (
    main,
    -- * for tests
    Result (..),
    Diagnostic (..),
    parseOptsNoCommands,
    formatDiagnostic, formatDiagnostics,
    travisFromConfigFile, MakeTravisOutput, Options (..), defOptions, options,
    ) where

import Control.Applicative ((<$>),(<|>), pure)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (void, when, unless, filterM, liftM, liftM2, forM_, mzero, foldM)
import Data.Char (isSpace, isUpper, toLower)
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import Data.Function
import Data.List
import Data.Maybe
import Data.Monoid (Monoid (..), Endo (..))
import Data.Either (partitionEithers)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Map as M
import System.Console.GetOpt
import System.Directory (doesDirectoryExist, getDirectoryContents, doesFileExist)
import System.Environment
import System.Exit
import System.FilePath.Posix ((</>), takeDirectory, takeFileName, takeExtension)
import System.IO
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Writer
import Text.Read (readMaybe)

import Distribution.Compiler (CompilerFlavor(..))
import Distribution.Package hiding (Package, pkgName)
import qualified Distribution.Package as Pkg
import Distribution.PackageDescription (GenericPackageDescription,packageDescription, testedWith, package, condLibrary, condTestSuites)
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import qualified Distribution.PackageDescription as PD
import qualified Distribution.ParseUtils as PU
import Distribution.Text
import Distribution.Version
#if MIN_VERSION_Cabal(2,0,0)
import Distribution.PackageDescription.Parse (readGenericPackageDescription)
#else
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Verbosity (Verbosity)
#endif
import Distribution.Compat.ReadP
    ( ReadP, (<++), (+++), between, char, many1, munch1
    , pfail, readP_to_S, readS_to_P, look
    , satisfy, sepBy, sepBy1, gather)

#ifdef MIN_VERSION_ShellCheck
import ShellCheck.Checker (checkScript)
import qualified ShellCheck.Interface as SC
import qualified ShellCheck.Formatter.Format as SC
import qualified ShellCheck.Formatter.TTY as SC.TTY

import Data.Functor.Identity (Identity (..))
import System.IO.Unsafe (unsafePerformIO)
#endif

#if MIN_VERSION_base(4,9,0)
import Data.Semigroup (Semigroup (..))
#else
import Data.Monoid ((<>))
#endif

#if !(MIN_VERSION_Cabal(2,0,0))
-- compat helpers for pre-2.0

readGenericPackageDescription :: Verbosity -> FilePath -> IO GenericPackageDescription
readGenericPackageDescription = readPackageDescription

mkVersion :: [Int] -> Version
mkVersion vn = Version vn []

versionNumbers :: Version -> [Int]
versionNumbers (Version vn _) = vn

#endif

-------------------------------------------------------------------------------
-- Hardcoded values
-------------------------------------------------------------------------------

knownGhcVersions :: [Version]
knownGhcVersions = fmap mkVersion
    [ [7,0,1],  [7,0,2], [7,0,3], [7,0,4]
    , [7,2,1],  [7,2,2]
    , [7,4,1],  [7,4,2]
    , [7,6,1],  [7,6,2], [7,6,3]
    , [7,8,1],  [7,8,2], [7,8,3], [7,8,4]
    , [7,10,1], [7,10,2], [7,10,3]
    , [8,0,1], [8,0,2]
    , [8,2,1], [8,2,2]
    , [8,4,1]
    , [8,5] -- HEAD
    ]

ghcAlpha :: Maybe Version
ghcAlpha = Just $ mkVersion [8,4,1]

defaultHLintVersion :: VersionRange
defaultHLintVersion = withinVersion (mkVersion [2,0])

defaultDoctestVersion :: VersionRange
defaultDoctestVersion = withinVersion (mkVersion [0,13])

-------------------------------------------------------------------------------
-- Script
-------------------------------------------------------------------------------

-- |  Encode shell command to be YAML safe and (optionally) ShellCheck it.
sh :: String -> String
sh = sh'
    [ 2034 -- VAR appears unused. Verify it or export it.
    , 2086 -- SC2086: Double quote to prevent globbing and word splitting.
    ]

-- | Like 'sh' but with explicit SC exclude codes.
sh' :: [Integer] -> String -> String
#ifndef MIN_VERSION_ShellCheck
sh' _ = shImpl
#else
sh' excl cmd = case checkScript iface spec of
    Identity res@(SC.CheckResult _ comments)
        | null comments -> shImpl cmd
        -- this is ugly use of unsafePerformIO
        -- but whole ShellCheck here is a little like `traceShow` anyway.
        | otherwise     -> unsafePerformIO $ do
            SC.onResult scFormatter res cmd
            fail "ShellCheck!"
  where
    iface = SC.SystemInterface $ \n -> return $ Left $ "cannot read file: " ++ n
    spec  = SC.CheckSpec "stdin" cmd excl (Just SC.Sh)

scFormatter :: SC.Formatter
scFormatter = unsafePerformIO (SC.TTY.format (SC.FormatterOptions SC.ColorAlways))
#endif

-- Non-ShellCheck version of sh'
shImpl :: String -> String
shImpl cmd
    | ':' `elem` cmd = "  - " ++ show cmd
    | otherwise      = "  - " ++ cmd

comment :: String -> String
comment = ("  # " ++)

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

tellStrLns :: Monad m => [String] -> YamlWriter m ()
tellStrLns = lift . tell . success

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
    | otherwise = tellStrLns [prologue] >> output >> tellStrLns epilogue
  where
    prologue = mconcat
        [ "  - echo ", prettyLabel
        , " && echo -en 'travis_fold:start:", showFold' label, "\\\\r'" ]
    epilogue = ["  - echo -en 'travis_fold:end:" ++ showFold' label ++ "\\\\r'" ]

    showFold' l = showFold l ++ if null pfx then "" else "-" ++ pfx

-- | Return the part after the first argument
--
-- >>> afterInfix "BAR" "FOOBAR XYZZY"
-- Just " XYZZY"
--
afterInfix :: Eq a => [a] -> [a] -> Maybe [a]
afterInfix needle haystack = findMaybe (afterPrefix needle) (tails haystack)

-- |
--
-- >>> afterPrefix "FOO" "FOOBAR"
-- Just "BAR"
--
afterPrefix :: Eq a => [a] -> [a] -> Maybe [a]
afterPrefix needle haystack
    | needle `isPrefixOf` haystack = Just (drop (length needle) haystack)
    | otherwise                    = Nothing

-- |
--
-- >>> findMaybe readMaybe ["foo", "1", "bar"] :: Maybe Int
-- Just 1
--
findMaybe :: (a -> Maybe b) -> [a] -> Maybe b
findMaybe f = foldr (\a b -> f a <|> b) Nothing

-- | >>> maybeReadP PU.parseTokenQ' "foo"
-- Just "foo"
maybeReadP :: ReadP a a -> String -> Maybe a
maybeReadP p s = listToMaybe $
    [ x
    | (x, rest) <- readP_to_S p s
    , all isSpace rest
    ]

main :: IO ()
main = do
    argv <- getArgs
    (opts,argv',configFile,xpkgs) <- parseOpts argv
    genTravisFromConfigFile (argv',opts) configFile xpkgs

parseOpts :: [String] -> IO (Options, [String], FilePath, [String])
parseOpts argv = case argv of
    (cmd : argv') | cmd `isPrefixOf` "regenerate" -> do
        let fp = fromMaybe  ".travis.yml" $ listToMaybe argv'
        ls <- fmap lines (readFile fp >>= evaluate . force) -- strict IO
        case findArgv ls of
            Nothing     -> dieCli [Error $ "expected REGENDATA line in " ++ fp ++ "\n"]
            Just argv'' -> parseOpts argv''
    _ -> parseOptsNoCommands argv
  where
    findArgv :: [String] -> Maybe [String]
    findArgv ls = do
        l <- findMaybe (afterInfix "REGENDATA") ls
        readMaybe l

-- Returns options, used argv, cabal file, xpkgs
parseOptsNoCommands :: [String] -> IO (Options, [String], FilePath, [String])
parseOptsNoCommands argv = case getOpt Permute options argv of
    (opts',configFile:xpkgs,[]) -> do
        opts <- foldOptions defOptions opts'
        return (opts,argv,configFile,xpkgs)
    (_,_,[]) -> dieCli [Error "expected .cabal or cabal.project file as first non-option argument\n"]
    (_,_,errs) -> dieCli (map Error errs)
  where
    foldOptions :: Options -> [Result Diagnostic (Options -> Options)] -> IO Options
    foldOptions def opts = case foldOptions' def opts of
        Success ws x -> do
            hPutStr stderr (formatDiagnostics ws)
            return x
        Failure errs -> dieCli errs

    foldOptions' :: Options -> [Result e (Options -> Options)] -> Result e Options
    foldOptions' opts = fmap (`appEndo` opts) . F.foldMap (fmap Endo)

dieCli ::  [Diagnostic] -> IO a
dieCli errs = hPutStrLn stderr (usageMsg errs) >> exitFailure
 where
    usageMsg errs' = formatDiagnostics errs' ++ usageInfo h options ++ ex

    h = intercalate "\n"
        [ "Usage: runghc make_travis_yml_2.hs [OPTIONS] <cabal-file | cabal.project> <extra-apt-packages...>"
        , ""
        , "Available commands:"
        , "    regenerate [TRAVIS.YAML]  Regenerate the file using the magic command in it. Default .travis.yml"
        , ""
        , "Available options:"
        ]

    ex = unlines
        [ ""
        , "Example:"
        , "    runghc make_travis_yml_2.hs -o .travis.yml someProject.cabal liblzma-dev"
        ]

runYamlWriter :: Maybe FilePath -> YamlWriter IO () -> IO ()
runYamlWriter mfp m = do
    result <- execWriterT (runMaybeT m)
    case result of
        Failure (formatDiagnostics -> errors) -> hPutStr stderr errors >> exitFailure
        Success (formatDiagnostics -> warnings) (unlines -> contents) -> do
            contents' <- evaluate (force contents)
            hPutStr stderr warnings
            case mfp of
                Nothing -> putStr contents'
                Just fp -> writeFile fp contents'

ghcMajVer :: Version -> (Int,Int)
ghcMajVer v
    | x:y:_ <- versionNumbers v = (x,y)
    | otherwise = error $ "panic: ghcMajVer called with " ++ show v

isGhcHead :: Version -> Bool
isGhcHead v
    | (_,y) <- ghcMajVer v = odd y || Just v == ghcAlpha
    | otherwise            = False

isGhcOdd :: Version -> Bool
isGhcOdd v
    | (_,y) <- ghcMajVer v = odd y
    | otherwise            = False

dispGhcVersion :: Version -> String
dispGhcVersion v
    | isGhcOdd v = "head"
    | otherwise = display v

data Package = Pkg
    { pkgName :: String
    , pkgDir :: FilePath
    , pkgGpd :: GenericPackageDescription
    } deriving (Eq, Show)

genTravisFromConfigFile :: ([String],Options) -> FilePath -> [String] -> IO ()
genTravisFromConfigFile args@(_, opts) path xpkgs =
    runYamlWriter (optOutput opts) $ travisFromConfigFile args path xpkgs

travisFromConfigFile
    :: MonadIO m
    => ([String],Options)
    -> FilePath
    -> [String]
    -> YamlWriter m ()
travisFromConfigFile args@(_, opts) path xpkgs = do
    cabalFiles <- getCabalFiles
    pkgs <- T.mapM (configFromCabalFile opts) cabalFiles
    config' <- maybe (return emptyConfig) readConfigFile (optConfig opts)
    (ghcs, prj) <- checkVersions pkgs
    let config = optConfigMorphism opts config'
    genTravisFromConfigs args xpkgs isCabalProject config prj ghcs
  where
    checkVersions
        :: MonadIO m
        => Project (Package, Set Version)
        -> YamlWriter m (Set Version, Project Package)
    checkVersions prj | null (prjPackages prj) = putStrLnErr "Error reading cabal file(s)!"
    checkVersions prj = do
        let (errors, names) = F.foldl' collectConfig mempty prj
        putStrLnErrs errors
        return (allVersions, prj { prjPackages = names })
      where
        allVersions = F.foldMap snd prj

        collectConfig
            :: ([String], [Package])
            -> (Package, Set Version)
            -> ([String], [Package])
        collectConfig aggregate (pkg, testWith) =
            aggregate <> (errors, [pkg])
          where
            symDiff a b = S.union a b `S.difference` S.intersection a b
            diff = symDiff testWith allVersions
            missingVersions = map dispGhcVersion $ S.toList diff
            errors | S.null diff = []
                   | otherwise = pure $ mconcat
                        [ pkgName pkg
                        , " is missing tested-with annotations for: "
                        ] ++ intercalate "," missingVersions

    isCabalProject :: Maybe FilePath
    isCabalProject
        | "cabal.project" `isPrefixOf` takeFileName path = Just path
        | otherwise = Nothing

    getCabalFiles :: MonadIO m => YamlWriter m (Project FilePath)
    getCabalFiles
        | isNothing isCabalProject = return (Project [path] Nothing Nothing)
        | otherwise = do
            contents <- liftIO $ readFile path
            pkgs <- either putStrLnErr return $ parseProjectFile path contents
            overPrjPackages concat `liftM` T.mapM findProjectPackage pkgs

    rootdir = takeDirectory path

    -- See findProjectPackages in cabal-install codebase
    -- this is simple variant.
    findProjectPackage :: MonadIO m => String -> YamlWriter m [FilePath]
    findProjectPackage pkglocstr = do
        mfp <- checkisFileGlobPackage pkglocstr `mplusMaybeT`
               checkIsSingleFilePackage pkglocstr
        maybe (putStrLnErr $ "bad package location: " ++ pkglocstr) return mfp

    checkIsSingleFilePackage pkglocstr = do
        let abspath = rootdir </> pkglocstr
        isFile <- liftIO $ doesFileExist abspath
        isDir  <- liftIO $ doesDirectoryExist abspath
        if | isFile && takeExtension pkglocstr == ".cabal" -> return (Just [abspath])
           | isDir -> checkisFileGlobPackage (pkglocstr </> "*.cabal")
           | otherwise -> return Nothing

    -- if it looks like glob, glob
    checkisFileGlobPackage pkglocstr =
        case filter (null . snd) $ readP_to_S parseFilePathGlobRel pkglocstr of
            [(g, "")] -> do
                files <- liftIO $ expandRelGlob rootdir g
                let files' = filter ((== ".cabal") . takeExtension) files
                -- if nothing is matched, skip.
                if null files' then return Nothing else return (Just files')
            _         -> return Nothing

    mplusMaybeT :: Monad m => m (Maybe a) -> m (Maybe a) -> m (Maybe a)
    mplusMaybeT ma mb = do
        mx <- ma
        case mx of
            Nothing -> mb
            Just x  -> return (Just x)

configFromCabalFile
    :: MonadIO m => Options -> FilePath -> YamlWriter m (Package, Set Version)
configFromCabalFile opts cabalFile = do
    gpd <- liftIO $ readGenericPackageDescription maxBound cabalFile

    let compilers = testedWith $ packageDescription gpd
        pkgNameStr = display $ Pkg.pkgName $ package $ packageDescription gpd

    let unknownComps = nub [ c | (c,_) <- compilers, c /= GHC ]
        ghcVerConstrs = [ vc | (GHC,vc) <- compilers ]
        ghcVerConstrs' = simplifyVersionRange $ foldr unionVersionRanges noVersion ghcVerConstrs
        twoDigitGhcVerConstrs = mapMaybe isTwoDigitGhcVersion ghcVerConstrs :: [Version]
        specificGhcVers = nub $ mapMaybe isSpecificVersion ghcVerConstrs

    unless (null twoDigitGhcVerConstrs) $ do
        putStrLnWarn $ "'tested-with:' uses two digit GHC versions (which don't match any existing GHC version): " ++ intercalate ", " (map display twoDigitGhcVerConstrs)
        putStrLnInfo $ "Either use wild-card format, for example 'tested-with: GHC ==7.10.*' or a specific existing version 'tested-with: GHC ==7.10.3'"

    when (null compilers) $ do
        putStrLnErr (unlines $
                     [ "empty or missing top-level 'tested-with:' definition in " ++ cabalFile ++ " file; example definition:"
                     , ""
                     , "tested-with: " ++ intercalate ", " [ "GHC==" ++ display v | v <- lastStableGhcVers ]
                     ])

    unless (null unknownComps) $ do
        putStrLnWarn $ "ignoring unsupported compilers mentioned in tested-with: " ++ show unknownComps

    when (null ghcVerConstrs) $ do
        putStrLnErr "'tested-with:' doesn't mention any 'GHC' version"

    when (isNoVersion ghcVerConstrs') $ do
        putStrLnErr "'tested-with:' describes an empty version range for 'GHC'"

    when (isAnyVersion ghcVerConstrs') $ do
        putStrLnErr "'tested-with:' allows /any/ 'GHC' version"

    let unknownGhcVers = sort $ specificGhcVers \\ knownGhcVersions

    unless (null unknownGhcVers) $ do
        putStrLnErr ("'tested-with:' specifically refers to unknown 'GHC' versions: "
                     ++ intercalate ", " (map display unknownGhcVers) ++ "\n"
                     ++ "Known GHC versions: " ++ intercalate ", " (map display knownGhcVersions))

    let testedGhcVersions = filter (`withinRange` ghcVerConstrs') knownGhcVersions

    when (null testedGhcVersions) $ do
        putStrLnErr "no known GHC version is allowed by the 'tested-with' specification"

    forM_ (optCollections opts) $ \c -> do
        let v = collToGhcVer c
        unless (v `elem` testedGhcVersions) $
            putStrLnErr $ unlines
               [ "collection " ++ c ++ " requires GHC " ++ dispGhcVersion v
               , "add 'tested-width: GHC == " ++ dispGhcVersion v ++ "' to your .cabal file"
               ]

    let pkg = Pkg pkgNameStr (takeDirectory cabalFile) gpd

    return (pkg, S.fromList testedGhcVersions)
  where
    lastStableGhcVers = nubBy ((==) `on` ghcMajVer) $ filter (not . isGhcHead) $ sortBy (flip compare) knownGhcVersions

    isTwoDigitGhcVersion :: VersionRange -> Maybe Version
    isTwoDigitGhcVersion vr = isSpecificVersion vr >>= t
      where
        t v | [_,_] <- versionNumbers v = Just v
        t _                             = Nothing

genTravisFromConfigs
    :: Monad m
    => ([String], Options)
    -> [String]
    -> Maybe FilePath
    -> Config
    -> Project Package
    -> Set Version
    -> YamlWriter m ()
genTravisFromConfigs (argv,opts) xpkgs isCabalProject config prj@Project { prjPackages = pkgs } versions = do
    let folds = cfgFolds config

    putStrLnInfo $
        "Generating Travis-CI config for testing for GHC versions: " ++ ghcVersions

    unless (null $ optOsx opts) $  do
        putStrLnInfo $ "Also OSX jobs for: " ++ ghcOsxVersions
        unless (S.null omittedOsxVersions) $
            putStrLnWarn $ "Not all GHC versions specified with --osx are generated: " ++ ghcOmittedOsxVersions

    ---------------------------------------------------------------------------
    -- travis.yml generation starts here

    tellStrLns
        [ "# This Travis job script has been generated by a script via"
        , "#"
        , "#   runghc make_travis_yml_2.hs " ++ unwords [ "'" ++ a ++ "'" | a <- argv ]
        , "#"
        , "# For more information, see https://github.com/hvr/multi-ghc-travis"
        , "#"
        , "language: c"
        , "sudo: false"
        , ""
        , "git:"
        , "  submodules: false  # whether to recursively clone submodules"
        , ""
        ]

    let projectName = fromMaybe (pkgName $ head pkgs) (cfgProjectName config)
    unless (null $ cfgIrcChannels config) $ tellStrLns $
        [ "notifications:"
        , "  irc:"
        , "    channels:"
        ] ++
        [ "      - \"" ++ chan ++ "\"" | chan <- cfgIrcChannels config ] ++
        [ "    skip_join: true"
        , "    template:"
        , "      - \"\\x0313" ++ projectName ++ "\\x03/\\x0306%{branch}\\x03 \\x0314%{commit}\\x03 %{build_url} %{message}\""
        , ""
        ]

    unless (null $ cfgOnlyBranches config) $ tellStrLns $
        [ "branches:"
        , "  only:"
        ] ++
        [ "    - " ++ branch
        | branch <- cfgOnlyBranches config
        ] ++
        [ ""
        ]

    when (cfgCache config) $ tellStrLns
        [ "cache:"
        , "  directories:"
        , "    - $HOME/.cabal/packages"
        , "    - $HOME/.cabal/store"
        ]

    -- on OSX ghc is installed in $HOME so we can cache it
    -- independently of linux
    when (cfgCache config && not (null (optOsx opts))) $ tellStrLns
        [ "    - $HOME/.ghc-install"
        ]

    when (cfgCache config) $ tellStrLns
        [ ""
        , "before_cache:"
        , "  - rm -fv $HOME/.cabal/packages/hackage.haskell.org/build-reports.log"
        , "  # remove files that are regenerated by 'cabal update'"
        , "  - rm -fv $HOME/.cabal/packages/hackage.haskell.org/00-index.*" -- legacy
        , "  - rm -fv $HOME/.cabal/packages/hackage.haskell.org/*.json" -- TUF meta-data
        , "  - rm -fv $HOME/.cabal/packages/hackage.haskell.org/01-index.cache"
        , "  - rm -fv $HOME/.cabal/packages/hackage.haskell.org/01-index.tar"
        , "  - rm -fv $HOME/.cabal/packages/hackage.haskell.org/01-index.tar.idx"
        , ""
        , "  - rm -rfv $HOME/.cabal/packages/head.hackage" -- if we cache, it will break builds.
        , ""
        ]

    tellStrLn "matrix:"
    tellStrLn "  include:"

    let colls = [ (collToGhcVer cid,cid) | cid <- reverse $ optCollections opts ]

    let tellJob osx gv = do
            let cvs = dispGhcVersion (lookupCabVer gv)
                gvs = dispGhcVersion gv

                xpkgs' = concatMap (',':) xpkgs

                colls' = [ cid | (v,cid) <- colls, v == gv ]

            tellStrLns
                [ "    - compiler: \"ghc-" <> gvs <> "\""
                , if | isGhcHead gv -> "      env: GHCHEAD=true"
                     | null colls'  -> "    # env: TEST=--disable-tests BENCH=--disable-benchmarks"
                     | otherwise    -> "      env: 'COLLECTIONS=" ++ intercalate "," colls' ++ "'"
                , "      addons: {apt: {packages: [ghc-ppa-tools,cabal-install-" <> cvs <> ",ghc-" <> gvs <> xpkgs' <> "], sources: [hvr-ghc]}}"
                ]

            when osx $ tellStrLns
                [ "      os: osx"
                ]

    F.forM_ versions $ tellJob False
    F.forM_ osxVersions $ tellJob True

    unless (S.null headGhcVers) $ do
        tellStrLn ""
        tellStrLn "  allow_failures:"

    F.forM_ headGhcVers $ \gv -> do
        let gvs = dispGhcVersion gv
        tellStrLn $ concat [ "    - compiler: \"ghc-", gvs, "\"" ]

    tellStrLns
        [ ""
        , "before_install:"
        , sh "HC=${CC}"
        , sh' [2034,2039] "HCPKG=${HC/ghc/ghc-pkg}" -- SC2039. In POSIX sh, string replacement is undefined.
        , sh "unset CC"
        -- rootdir is useful for manual script additions
        , sh "ROOTDIR=$(pwd)"
        , sh "mkdir -p $HOME/.local/bin"
        ]

    let haskellOnMacos = "https://haskell.futurice.com/haskell-on-macos.py"

    if null (optOsx opts)
    then tellStrLns
        [ sh "PATH=/opt/ghc/bin:/opt/ghc-ppa-tools/bin:$HOME/local/bin:$PATH"
        ]
    else tellStrLns
        [ sh $ "if [ \"$(uname)\" = \"Darwin\" ]; then brew update; brew install python3; curl " ++ haskellOnMacos ++ " | python3 - --make-dirs --install-dir=$HOME/.ghc-install --cabal-alias=head install cabal-install-head ${HC}; fi"
        , sh $ "if [ \"$(uname)\" = \"Darwin\" ]; then PATH=$HOME/.ghc-install/ghc/bin:$HOME/local/bin:$PATH; else PATH=/opt/ghc/bin:/opt/ghc-ppa-tools/bin:$HOME/local/bin:$PATH; fi"
        ]

    -- HCNUMVER, numeric HC version, e.g. ghc 7.8.4 is 70804 and 7.10.3 is 71003
    tellStrLns
        [ sh $ "HCNUMVER=$(( $(${HC} --numeric-version|sed -E 's/([0-9]+)\\.([0-9]+)\\.([0-9]+).*/\\1 * 10000 + \\2 * 100 + \\3/') ))"
        , sh "echo $HCNUMVER"
        ]

    unless (null colls) $
       tellStrLn " - IFS=', ' read -a COLLS <<< \"$COLLECTIONS\""

    tellStrLns
        [ ""
        , "install:"
        , sh "cabal --version"
        , sh "echo \"$(${HC} --version) [$(${HC} --print-project-git-commit-id 2> /dev/null || echo '?')]\""
        , sh "BENCH=${BENCH---enable-benchmarks}"
        , sh "TEST=${TEST---enable-tests}"
        , sh "HADDOCK=${HADDOCK-true}"
        , sh "INSTALLED=${INSTALLED-true}"
        , sh "GHCHEAD=${GHCHEAD-false}"
        ]

    -- Update hackage index. Side-effect: ~/.cabal.config is created.
    tellStrLns
        [ sh "travis_retry cabal update -v"
        , sh "sed -i.bak 's/^jobs:/-- jobs:/' ${HOME}/.cabal/config"
        , sh "rm -fv cabal.project cabal.project.local"
        ]

    -- Cabal jobs
    case cfgJobs config of
        (Just n, _) -> tellStrLns
            [ sh $ "sed -i.bak 's/^-- jobs:.*/jobs: " ++ show n ++ "/' ${HOME}/.cabal/config"
            ]
        _ -> return ()

    -- GHC jobs
    case cfgJobs config of
        (_, Just m) -> tellStrLns
            [ sh $ "if [ $HCNUMVER -ge 70800 ]; then sed -i.bak 's/-- ghc-options:.*/ghc-options: -j" ++ show m ++ "/' ${HOME}/.cabal/config; fi"
            ]
        _ -> return ()

    -- Add head.hackage repository to ~/.cabal/config
    -- (locally you want to add it to cabal.project)
    unless (S.null headGhcVers) $ tellStrLns
        [ "  # Overlay Hackage Package Index for GHC HEAD: https://github.com/hvr/head.hackage"
        , "  - |"
        , "    if $GHCHEAD; then"
        --  See: https://ghc.haskell.org/trac/ghc/wiki/Commentary/Libraries/VersionHistory
        --  other packages don't have major bumps GHC-8.2.2 -> GHC-8.4.1 (2018-01-03)
        , "      sed -i.bak 's/-- allow-newer:.*/allow-newer: *:base, *:template-haskell, *:ghc, *:Cabal/' ${HOME}/.cabal/config"
        , ""
        , "      echo 'repository head.hackage'                                                        >> ${HOME}/.cabal/config"
        , "      echo '   url: http://head.hackage.haskell.org/'                                       >> ${HOME}/.cabal/config"
        , "      echo '   secure: True'                                                                >> ${HOME}/.cabal/config"
        , "      echo '   root-keys: 07c59cb65787dedfaef5bd5f987ceb5f7e5ebf88b904bbd4c5cbdeb2ff71b740' >> ${HOME}/.cabal/config"
        , "      echo '              2e8555dde16ebd8df076f1a8ef13b8f14c66bad8eafefd7d9e37d0ed711821fb' >> ${HOME}/.cabal/config"
        , "      echo '              8f79fd2389ab2967354407ec852cbe73f2e8635793ac446d09461ffb99527f6e' >> ${HOME}/.cabal/config"
        , "      echo '   key-threshold: 3'                                                            >> ${HOME}/.cabal.config"
        , ""
        , "      cabal new-update head.hackage -v"
        , "    fi"
        ]

    -- Output cabal.config
    tellStrLns
        [ sh "grep -Ev -- '^\\s*--' ${HOME}/.cabal/config | grep -Ev '^\\s*$'"
        ]

    -- Install doctest
    let doctestVersionConstraint
            | isAnyVersion (cfgDoctestVersion config) = ""
            | otherwise = " --constraint='doctest " ++ display (cfgDoctestVersion config) ++ "'"
    when (cfgDoctest config) $ tellStrLns
        [ sh $ "if [ $HCNUMVER -ge 80000 ]; then cabal new-install -w ${HC} --symlink-bindir=$HOME/.local/bin doctest" ++ doctestVersionConstraint ++ "; fi"
        ]

    -- Install hlint
    let hlintVersionConstraint
            | isAnyVersion (cfgHLintVersion config) = ""
            | otherwise = " --constraint='hlint " ++ display (cfgHLintVersion config) ++ "'"
    when (cfgHLint config) $ tellStrLns
        [ sh $ "if [ $HCNUMVER -eq 80202 ]; then cabal new-install -w ${HC} --symlink-bindir=$HOME/.local/bin hlint" ++ hlintVersionConstraint ++ "; fi"
        ]

    -- create cabal.project file
    generateCabalProject False

    let pkgFilter = intercalate " | " $ map (wrap.pkgName) pkgs
        wrap s = "grep -Fv \"" ++ s ++ " ==\""
    unless (null colls) $ tellStrLns
        [ "  - for COLL in \"${COLLS[@]}\"; do"
        , "      echo \"== collection $COLL ==\";"
        , "      ghc-travis collection ${COLL} > /dev/null || break;"
        , "      ghc-travis collection ${COLL} | " ++ pkgFilter ++ " > cabal.project.freeze;"
        , "      grep ' collection-id' cabal.project.freeze;"
        , "      rm -rf dist-newstyle/;"
        , "      cabal new-build -w ${HC} ${TEST} ${BENCH} --project-file=\"" ++ projectFile ++ "\" --dep -j2 all;"
        , "    done"
        , ""
        ]

    forM_ pkgs $ \Pkg{pkgDir} -> tellStrLns
        [ "  - if [ -f \"" ++ pkgDir ++ "/configure.ac\" ]; then"
        , "      (cd \"" ++ pkgDir ++ "\" && autoreconf -i);"
        , "    fi"
        ]

    let quotedRmPaths =
          quotedPaths (\Pkg{pkgDir} -> pkgDir ++ "/.ghc.environment.*")
          ++ " " ++
          quotedPaths (\Pkg{pkgDir} -> pkgDir ++ "/dist")

    tellStrLns
        [ sh $ "rm -f cabal.project.freeze"
        ]

    -- Install dependencies
    when (cfgInstallDeps config) $ do
        tellStrLns
            [ sh $ "cabal new-build -w ${HC} ${TEST} ${BENCH} --project-file=\"" ++ projectFile ++"\" --dep -j2 all"
            ]
        when (cfgNoTestsNoBench config) $ tellStrLns
            [ sh $ "cabal new-build -w ${HC} --disable-tests --disable-benchmarks --project-file=\"" ++ projectFile ++ "\" --dep -j2 all"
            ]

    tellStrLns
        [ sh $ "rm -rf " ++ quotedRmPaths
        , sh $  "DISTDIR=$(mktemp -d /tmp/dist-test.XXXX)"
        ]

    tellStrLns
        [ ""
        , "# Here starts the actual work to be performed for the package under test;"
        , "# any command which exits with a non-zero exit code causes the build to fail."
        , "script:"
        , "  # test that source-distributions can be generated"
        ]

    foldedTellStrLns FoldSDist "Packaging..." folds $ do
        forM_ pkgs $ \Pkg{pkgDir} -> tellStrLns
            [ sh $ "(cd \"" ++ pkgDir ++ "\" && cabal sdist)"
            ]

    let tarFiles = quotedPaths $ \Pkg{pkgDir,pkgName} ->
                pkgDir </> "dist" </> pkgName ++ "-*.tar.gz"


    foldedTellStrLns FoldUnpack "Unpacking..." folds $ do
        tellStrLns
            [ sh $ "mv " ++ tarFiles ++ " ${DISTDIR}/"
            , sh $ "cd ${DISTDIR} || false" -- fail explicitly, makes SC happier
            , sh $ "find . -maxdepth 1 -name '*.tar.gz' -exec tar -xvf '{}' \\;"
            ]
        generateCabalProject True

    when (cfgNoTestsNoBench config) $ foldedTellStrLns FoldBuild "Building..." folds $ tellStrLns
        [ comment "this builds all libraries and executables (without tests/benchmarks)"
        , sh "cabal new-build -w ${HC} --disable-tests --disable-benchmarks all"
        ]

    tellStrLns [""]

    when (cfgInstalled config) $ foldedTellStrLns FoldBuildInstalled
        "Building with installed constraints for package in global-db..." folds $ tellStrLns
        [ comment "Build with installed constraints for packages in global-db"
        -- SC2046: Quote this to prevent word splitting.
        -- here we split on purpose!
        , sh' [2046, 2086] $ unwords
            [ "if $INSTALLED;"
            , "then echo cabal new-build -w ${HC} --disable-tests --disable-benchmarks $(${HCPKG} list --global --simple-output --names-only | sed 's/\\([a-zA-Z0-9-]\\{1,\\}\\) */--constraint=\"\\1 installed\" /g') all | sh;"
            , "else echo \"Not building with installed constraints\"; fi"
            ]
        ]

    tellStrLns [""]

    foldedTellStrLns FoldBuildEverything
        "Building with tests and benchmarks..." folds $ tellStrLns
        [ comment "build & run tests, build benchmarks"
        , sh "cabal new-build -w ${HC} ${TEST} ${BENCH} all"
        ]

    -- cabal new-test fails if there are no test-suites.
    when hasTests $
        foldedTellStrLns FoldTest "Testing..." folds $ tellStrLns
            [ sh $ mconcat
                [ "if [ \"x$TEST\" = \"x--enable-tests\" ]; then "
                , if cfgNoise config
                     then "cabal "
                     else "(set -o pipefail; cabal -vnormal+nowrap+markoutput "
                , "new-test -w ${HC} ${TEST} ${BENCH} all"
                , if cfgNoise config
                     then ""
                     else " 2>&1 | sed '/^-----BEGIN CABAL OUTPUT-----$/,/^-----END CABAL OUTPUT-----$/d' )"
                , "; fi"
                ]
            ]

    tellStrLns [""]

    when (cfgDoctest config) $ do
        let doctestOptions = unwords $ map (show . PU.showToken) $ cfgDoctestOptions config
        tellStrLns [ comment "doctest" ]
        foldedTellStrLns FoldDoctest "Doctest..." folds $ do
            forM_ pkgs $ \Pkg{pkgName,pkgGpd} -> do
                let args = doctestArgs pkgGpd
                    args' = unwords args
                unless (null args) $ tellStrLns
                    [ sh $ "if [ $HCNUMVER -ge 80000 ]; then (cd " ++ pkgName ++ "-* && doctest " ++ doctestOptions ++ " " ++ args' ++ "); fi"
                    ]
        tellStrLns [ "" ]

    when (cfgHLint config) $ do
        let hlintOptions = maybe "" (" -h ${ROOTDIR}/" ++) (cfgHLintYaml config)
        tellStrLns [ comment "hlint" ]
        foldedTellStrLns FoldHLint "HLint.." folds $ do
            forM_ pkgs $ \Pkg{pkgName,pkgGpd} -> do
                -- note: same arguments work so far for doctest and hlint
                let args = doctestArgs pkgGpd
                    args' = unwords args
                unless (null args) $ tellStrLns
                    [ sh $ "if [ $HCNUMVER -eq 80202 ]; then (cd " ++ pkgName ++ "-* && hlint" ++ hlintOptions ++ " " ++ args' ++ "); fi"
                    ]
        tellStrLns [ "" ]

    when (cfgCheck config) $
        foldedTellStrLns FoldCheck "cabal check..." folds $ do
            tellStrLns [ comment "cabal check" ]
            forM_ pkgs $ \Pkg{pkgName} -> tellStrLns
                [ sh $ "(cd " ++ pkgName ++ "-* && cabal check)"

                ]
            tellStrLns [ "" ]

    when hasLibrary $
        foldedTellStrLns FoldHaddock "Haddock..." folds $ tellStrLns
            [ comment "haddock"
            , sh "rm -rf ./dist-newstyle"
            , sh "if $HADDOCK; then cabal new-haddock -w ${HC} ${TEST} ${BENCH} all; else echo \"Skipping haddock generation\";fi"
            , ""
            ]

    let constraintSets = cfgConstraintSets config
    forM_ constraintSets $ \cs -> do
        let name = csName cs
        let constraintFlags = concatMap (\x ->  " --constraint='" ++ x ++ "'") (csConstraints cs)
        foldedTellStrLns' FoldConstraintSets name ("Constraint set " ++ name) folds $ tellStrLns
            [ sh' [2086] $ "if " ++ ghcVersionPredicate (csGhcVersions cs) ++ "; then cabal new-build -w ${HC} --disable-tests --disable-benchmarks" ++ constraintFlags ++ " all; else echo skipping...; fi"
            , ""
            ]

    unless (null colls) $
        foldedTellStrLns FoldStackage "Stackage builds..." folds $ tellStrLns
            [ "  # try building & testing for package collections"
            , "  - for COLL in \"${COLLS[@]}\"; do"
            , "      echo \"== collection $COLL ==\";"
            , "      ghc-travis collection ${COLL} > /dev/null || break;"
            , "      ghc-travis collection ${COLL} | " ++ pkgFilter ++ " > cabal.project.freeze;"
            , "      grep ' collection-id' cabal.project.freeze;"
            , "      rm -rf dist-newstyle/;"
            , "      cabal new-build -w ${HC} ${TEST} ${BENCH} all || break;"
            , "      if [ \"x$TEST\" = \"x--enable-tests\" ]; then cabal new-test -w ${HC} ${TEST} ${BENCH} all || break; fi;"
            , "    done"
            , ""
            ]

    tellStrLns
        [ "# REGENDATA " ++ show argv
        , "# EOF"
        ]

    return ()
  where
    hasTests   = F.any (\Pkg{pkgGpd} -> not . null $ condTestSuites pkgGpd) pkgs
    hasLibrary = F.any (\Pkg{pkgGpd} -> isJust $ condLibrary pkgGpd) pkgs

    headGhcVers = S.filter isGhcHead versions

    generateCabalProject dist = do
        tellStrLns
            [ sh $ "printf 'packages: " ++ cabalPaths ++ "\\n' > cabal.project"
            ]
        F.forM_ (prjConstraints prj) $ \xs -> do
            let s = concat (lines xs)
            tellStrLns
                [ sh $ "echo 'constraints: " ++ s ++ "' >> cabal.project"
                ]
        F.forM_ (prjAllowNewer prj) $ \xs -> do
            let s = concat (lines xs)
            tellStrLns
                [ sh $ "echo 'allow-newer: " ++ s ++ "' >> cabal.project"
                ]
        unless (null (cfgLocalGhcOptions config)) $ forM_ pkgs $ \Pkg{pkgName} -> do
            let s = unwords $ map (show . PU.showToken) $ cfgLocalGhcOptions config
            tellStrLns
                [ sh $ "echo 'package " ++ pkgName ++ "' >> cabal.project"
                , sh $ "echo '  ghc-options: " ++ s ++ "' >> cabal.project"
                ]
        tellStrLns
            [ sh $ "cat cabal.project"
            ]
      where
        cabalPaths
            | dist      = quotedPaths $ \Pkg{pkgName} -> pkgName ++ "-*/*.cabal"
            | otherwise = quotedPaths $ \Pkg{pkgDir}  -> pkgDir

    projectFile :: FilePath
    projectFile = fromMaybe "cabal.project" isCabalProject

    quotedPaths :: (Package -> FilePath) -> String
    quotedPaths f = unwords $ map (f . quote) pkgs
      where
        quote pkg = pkg{ pkgDir = "\"" ++ pkgDir pkg ++ "\"" }

    showVersions :: Set Version -> String
    showVersions = unwords . map dispGhcVersion . S.toList

    -- specified ersions
    osxVersions' :: Set Version
    osxVersions' = S.fromList $ mapMaybe simpleParse $ optOsx opts

    ghcVersions :: String
    ghcVersions = showVersions versions

    osxVersions, omittedOsxVersions :: Set Version
    (osxVersions, omittedOsxVersions) = S.partition (`S.member` versions) osxVersions'

    ghcOsxVersions :: String
    ghcOsxVersions = showVersions osxVersions

    ghcOmittedOsxVersions :: String
    ghcOmittedOsxVersions = showVersions omittedOsxVersions

    lookupCabVer :: Version -> Version
    lookupCabVer v = fromMaybe (error "internal error") $ lookup (x,y) cabalVerMap
      where
        (x,y) = ghcMajVer v
        cabalVerMap = fmap (fmap mkVersion)
                      [ ((7, 0),  [1,25]) -- Use HEAD for everything.
                      , ((7, 2),  [1,25])
                      , ((7, 4),  [1,25])
                      , ((7, 6),  [1,25])
                      , ((7, 8),  [1,25])
                      , ((7,10),  [1,25])
                      , ((8, 0),  [1,25])
                      , ((8, 2),  [1,25])
                      , ((8, 4),  [1,25])
                      , ((8, 5),  [1,25])
                      ]

-- | Modules arguments to the library
--
-- * We check the library component
--
-- * If there are hs-source-dirs, use them
--
-- * otherwise use exposed + other modules
--
-- * Also add default-extensions
--
-- /Note:/ same argument work for hlint too!
--
doctestArgs :: GenericPackageDescription -> [String]
doctestArgs gpd = case PD.library $ flattenPackageDescription gpd of
    Nothing -> []
    Just l  -> exts ++ dirsOrMods
      where
        bi = PD.libBuildInfo l

        dirsOrMods
            | null (PD.hsSourceDirs bi) = map display (PD.exposedModules l)
            | otherwise = PD.hsSourceDirs bi

        exts = map (("-X" ++) . display) (PD.defaultExtensions bi)

collToGhcVer :: String -> Version
collToGhcVer cid = case simpleParse cid of
  Nothing -> error ("invalid collection-id syntax " ++ show cid)
  Just (PackageIdentifier n (versionNumbers -> v))
    | display n /= "lts" -> error ("unknown collection " ++ show cid)
    | isPrefixOf [0] v -> mkVersion [7,8,3]
    | isPrefixOf [1] v -> mkVersion [7,8,4]
    | isPrefixOf [2] v -> mkVersion [7,8,4]
    | isPrefixOf [3] v -> mkVersion [7,10,2]
    | isPrefixOf [4] v -> mkVersion [7,10,3]
    | isPrefixOf [5] v -> mkVersion [7,10,3]
    | isPrefixOf [6] v -> mkVersion [7,10,3]
    | isPrefixOf [7] v -> mkVersion [8,0,1]
    | otherwise -> error ("unknown collection " ++ show cid)

-------------------------------------------------------------------------------
-- Jobs
-------------------------------------------------------------------------------

-- | parse jobs defintion
--
-- * N:M - N ghcs (cabal -j), M threads (ghc -j)
--
-- >>> let parseJobs = maybeReadP parseJobsQ
-- >>> parseJobs "2:2"
-- Just (Just 2,Just 2)
--
-- >>> parseJobs ":2"
-- Just (Nothing,Just 2)
--
-- >>> parseJobs "2"
-- Just (Just 2,Nothing)
--
-- >>> parseJobs "garbage"
-- Nothing
--
parseJobsQ :: ReadP r (Maybe Int, Maybe Int)
parseJobsQ = nm <++ m <++ n <++ return (Nothing, Nothing)
  where
    nm = do
      x <- parseInt
      _ <- char ':'
      y <- parseInt
      return (Just x, Just y)

    m = do
      _ <- char ':'
      y <- parseInt
      return (Nothing, Just y)

    n = do
      x <- parseInt
      return (Just x, Nothing)

-------------------------------------------------------------------------------
-- Project file
-------------------------------------------------------------------------------

data Project a = Project
    { prjPackages    :: [a]
    , prjConstraints :: Maybe String
    , prjAllowNewer  :: Maybe String
    }
  deriving (Show, Functor, F.Foldable, T.Traversable)

overPrjPackages :: ([a] -> [b]) -> Project a -> Project b
overPrjPackages f prj = prj { prjPackages = f (prjPackages prj) }

emptyProject :: Project [a]
emptyProject = Project [] Nothing Nothing

-- | Parse project file. Extracts only @packages@ field.
--
-- >>> fmap prjPackages $ parseProjectFile "cabal.project" "packages: foo bar/*.cabal"
-- Right ["foo","bar/*.cabal"]
--
parseProjectFile :: FilePath -> String -> Either String (Project String)
parseProjectFile path contents =
    case PU.parseFields legacyProjectConfigFieldDescrs emptyProject contents of
        PU.ParseOk _ x -> Right x
        PU.ParseFailed err -> Left $ case PU.locatedErrorMsg err of
            (l, msg) -> "ERROR " ++ path ++ ":" ++ show l ++ ": " ++ msg

legacyProjectConfigFieldDescrs :: [PU.FieldDescr (Project String)]
legacyProjectConfigFieldDescrs =
    [ PU.listField "packages"
        (error "we don't pretty print") -- pretty
        parsePackageLocationTokenQ -- parse
        prjPackages
        (\x prj -> prj { prjPackages = x })
    , PU.simpleField "constraints"
        (error "we don't pretty print") -- pretty
        (fmap Just PU.parseFreeText)
        prjConstraints
        (\x prj -> prj { prjConstraints = maybeAlt2 commaConcat (prjConstraints prj) x })
    , PU.simpleField "allow-newer"
        (error "we don't pretty print") -- pretty
        (fmap Just PU.parseFreeText)
        prjAllowNewer
        (\x prj -> prj { prjAllowNewer = maybeAlt2 commaConcat (prjAllowNewer prj) x })
    ]
  where
    maybeAlt2 _ Nothing  x        = x
    maybeAlt2 _ x        Nothing  = x
    maybeAlt2 f (Just x) (Just y) = Just (f x y)

    commaConcat x y
        | all isSpace x = y
        | all isSpace y = x
        | otherwise     = x ++ ", " ++ y

-------------------------------------------------------------------------------
-- Options
-------------------------------------------------------------------------------

data Options = Options
    { optCollections :: [String]
    , optOutput :: Maybe FilePath
    , optOsx :: [String]
    , optConfig :: Maybe FilePath
    , optConfigMorphism :: Config -> Config
    }

defOptions :: Options
defOptions = Options
    { optCollections = []
    , optOutput = Nothing
    , optOsx = []
    , optConfig = Nothing
    , optConfigMorphism = id
    }

options :: [OptDescr (Result Diagnostic (Options -> Options))]
options =
    [ Option [] ["no-cache"]
      (NoArg $ successCM $ \cfg -> cfg { cfgCache = False })
      "disable Travis caching"
    , Option [] ["no-cabal-noise"]
      (NoArg $ successCM $ \cfg -> cfg { cfgNoise = False })
      "remove cabal noise from test output"
    , Option [] ["no-cabal-check"]
      (NoArg $ successCM $ \cfg -> cfg { cfgCheck = False })
      "Disable cabal check"
    , Option [] ["no-install-dependencies"]
      (NoArg $ successCM $ \cfg -> cfg { cfgInstallDeps = False })
      "Disable installing dependencies in a seperate step"
    , Option [] ["no-no-tests-no-bench"]
      (NoArg $ successCM $ \cfg -> cfg { cfgNoTestsNoBench = False })
      "Don't build with --no-tests --no-benchmarks"
    , Option [] ["no-installed"]
      (NoArg $ successCM $ \cfg -> cfg { cfgInstalled = False })
      "Don't build with 'installed' constraints"
    , Option ['c'] ["collection"]
      (ReqArg (success' $ \arg opts -> opts { optCollections = arg : optCollections opts }) "CID")
      "enable package collection(s) (e.g. 'lts-7'), use multiple times for multiple collections"
    , Option ['f'] ["fold"]
      (flip OptArg "FOLDS" $ \arg -> case arg of
        Nothing   -> successCM $ \cfg -> cfg { cfgFolds = S.fromList possibleFolds }
        Just arg' -> case maybeReadP parseFoldQ arg' of
            Nothing -> Failure [Error $ "cannot parse --fold argument: " ++ arg' ++ "\n"]
            Just f  -> successCM $ \cfg -> cfg { cfgFolds = f (cfgFolds cfg) })
      ("build output(s) to fold, use multiple times for multiple folds. No argument defaults to 'all'. Possible values: all, all-but-test, " ++ intercalate ", " (map showFold possibleFolds))
    , Option [] ["irc-channel"]
      (ReqArg (successCM' $ \arg cfg -> cfg { cfgIrcChannels = arg : cfgIrcChannels cfg }) "HOST#CHANNEL")
      "enable IRC notifcations to given channel (e.g. 'irc.freenode.org#haskell-lens'), use multiple times for multiple channels"
    , Option ['n'] ["name"]
      (ReqArg (successCM' $ \arg cfg -> cfg { cfgProjectName = Just arg }) "NAME")
      "project name (used for IRC notifications), defaults to package name or name of first package listed in cabal.project file"
    , Option ['b'] ["branch"]
      (ReqArg (successCM' $ \arg cfg -> cfg { cfgOnlyBranches = arg : cfgOnlyBranches cfg }) "BRANCH")
      "enable builds only for specific brances, use multiple times for multiple branches"
    , Option ['o'] ["output"]
      (ReqArg (success' $ \arg opts -> opts { optOutput = Just arg }) "OUTPUT")
      "output file (stdout if omitted)"
    , Option [] ["config"]
      (OptArg (success' $ \arg opts -> opts { optConfig = Just $ fromMaybe "cabal.make-travis-yml" arg }) "CONFIG")
      "config file, currently used only to specify constraint sets"
    , Option [] ["osx"]
      (ReqArg (success' $ \arg opts -> opts { optOsx = arg : optOsx opts }) "GHC")
      "generate osx build job with ghc version"
    , Option ['j'] ["jobs"]
      (reqArgReadP parseJobsQ (\jobs cfg -> cfg { cfgJobs = jobs }) "JOBS")
      "jobs (N:M - cabal:ghc)"
    , Option [] ["local-ghc-options"]
      (reqArgReadP parseOptsQ (\xs cfg -> cfg { cfgLocalGhcOptions = xs }) "OPTIONS")
      "--ghc-options for local packages"
    , Option ['d'] ["doctest"]
      (NoArg $ successCM $ \cfg -> cfg { cfgDoctest = True })
      "Run doctest using .ghc.environment files."
    , Option [] ["doctest-options"]
      (reqArgReadP parseOptsQ (\xs cfg -> cfg { cfgDoctestOptions = xs }) "OPTIONS")
      "Additional doctest options."
    , Option ['l'] ["hlint"]
      (NoArg $ successCM $ \cfg -> cfg { cfgHLint = True })
      "Run hlint (only on GHC-8.2.2 target)"
    , Option [] ["hlint-yaml"]
      (ReqArg (successCM' $ \arg cfg -> cfg { cfgHLintYaml = Just arg }) "HLINT.YAML")
      "Relative path to .hlint.yaml."
    , Option [] ["hlint-version"]
      (reqArgReadP parse (\arg cfg -> cfg { cfgHLintVersion = arg }) "VERSION")
      "HLint version range"
    ]
  where
    overCM f opts = opts
        { optConfigMorphism = f . optConfigMorphism opts
        }

    success' f arg = success (f arg)

    successCM = success . overCM
    successCM' f arg = successCM (f arg)

    reqArgReadP :: ReadP a a -> (a -> Config -> Config) -> String -> ArgDescr (Result Diagnostic (Options -> Options))
    reqArgReadP p f n = flip ReqArg n $ \arg -> case maybeReadP  p arg of
        Nothing -> Failure [Error $  "cannot parse: " ++ arg ]
        Just x  -> successCM' f x

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
#if MIN_VERSION_base(4,9,0)
    mappend = (<>)

instance Monoid a => Semigroup (Result e a) where
    Failure err1   <> Failure err2   = Failure $ err1 <> err2
    Failure err1   <> Success err2 _ = Failure $ err1 <> err2
    Success err1 _ <> Failure err2   = Failure $ err1 <> err2
    Success l1 o1  <> Success l2 o2  = Success (mappend l1 l2) (mappend o1 o2)
#else
    Failure err1   `mappend` Failure err2   = Failure $ err1 `mappend` err2
    Failure err1   `mappend` Success err2 _ = Failure $ err1 `mappend` err2
    Success err1 _ `mappend` Failure err2   = Failure $ err1 `mappend` err2
    Success l1 o1  `mappend` Success l2 o2  = Success (mappend l1 l2) (mappend o1 o2)
#endif

-------------------------------------------------------------------------------
-- Fold
-------------------------------------------------------------------------------

data Fold
    = FoldSDist
    | FoldUnpack
    | FoldBuild
    | FoldBuildInstalled
    | FoldBuildEverything
    | FoldTest
    | FoldHaddock
    | FoldStackage
    | FoldCheck
    | FoldDoctest
    | FoldHLint
    | FoldConstraintSets
  deriving (Eq, Ord, Show, Enum, Bounded)

showFold :: Fold -> String
showFold = dashise . drop 4 . show
  where
    dashise = intercalate "-" . map (map toLower) . split

    split [] = []
    split xs0 =
        let (ys, xs1) = span isUpper xs0
            (zs, xs2) = break isUpper xs1
        in (ys ++ zs) : split xs2

possibleFolds :: [Fold]
possibleFolds = [minBound .. maxBound]

parseFoldQ :: ReadP r (Set Fold -> Set Fold)
parseFoldQ = do
    t <- PU.parseTokenQ
    case t of
        "all"          -> return $ const $ S.fromList possibleFolds
        "all-but-test" -> return $ const $ S.delete FoldTest $ S.fromList possibleFolds
        n -> case M.lookup n ps of
            Just n' -> return (S.insert n')
            Nothing -> fail $ "Illegal fold name: " ++ n
  where
    ps = M.fromList $ map (\x -> (showFold x, x)) possibleFolds

-------------------------------------------------------------------------------
-- Config file
-------------------------------------------------------------------------------

data Config = Config
    { cfgHLint           :: !Bool
    , cfgHLintYaml       :: !(Maybe FilePath)
    , cfgHLintVersion    :: !VersionRange
    , cfgJobs            :: (Maybe Int, Maybe Int)
    , cfgDoctest         :: !Bool
    , cfgDoctestOptions  :: [String]
    , cfgDoctestVersion  :: !VersionRange
    , cfgLocalGhcOptions :: [String]
    , cfgConstraintSets  :: [ConstraintSet]
    , cfgCache           :: !Bool
    , cfgCheck           :: !Bool
    , cfgNoise           :: !Bool
    , cfgNoTestsNoBench  :: !Bool
    , cfgInstalled       :: !Bool
    , cfgInstallDeps     :: !Bool
    , cfgOnlyBranches    :: [String]
    , cfgIrcChannels     :: [String]
    , cfgProjectName     :: Maybe String
    , cfgFolds           :: Set Fold
    }
  deriving (Show)

emptyConfig :: Config
emptyConfig = Config
    { cfgHLint           = False
    , cfgHLintYaml       = Nothing
    , cfgHLintVersion    = defaultHLintVersion
    , cfgJobs            = (Nothing, Nothing)
    , cfgDoctest         = False
    , cfgDoctestOptions  = []
    , cfgDoctestVersion  = defaultDoctestVersion
    , cfgLocalGhcOptions = []
    , cfgConstraintSets  = []
    , cfgCache           = True
    , cfgCheck           = True
    , cfgNoise           = True
    , cfgNoTestsNoBench  = True
    , cfgInstalled       = True
    , cfgInstallDeps     = True
    , cfgOnlyBranches    = []
    , cfgIrcChannels     = []
    , cfgProjectName     = Nothing
    , cfgFolds           = S.empty
    }

configFieldDescrs :: [PU.FieldDescr Config]
configFieldDescrs =
    [ PU.simpleField  "jobs"
        (error "we don't pretty print")
        parseJobsQ
        cfgJobs
        (\x cfg -> cfg { cfgJobs = x })
    , PU.boolField  "hlint"
        cfgHLint
        (\b cfg -> cfg { cfgHLint = b })
    , PU.simpleField "hlint-yaml"
        (error "we don't pretty print")
        (fmap Just PU.parseFilePathQ)
        cfgHLintYaml
        (\x cfg -> cfg { cfgHLintYaml = x })
    , PU.simpleField "hlint-version"
        (error "we don't pretty print")
        parse
        cfgHLintVersion
        (\x cfg -> cfg { cfgHLintVersion = x })
    , PU.boolField  "doctest"
        cfgDoctest
        (\b cfg -> cfg { cfgDoctest = b })
    , PU.simpleField "doctest-options"
        (error "we don't pretty print")
        parseOptsQ
        cfgDoctestOptions
        (\x cfg -> cfg { cfgDoctestOptions = cfgDoctestOptions cfg ++ x })
    , PU.simpleField "doctest-version"
        (error "we don't pretty print")
        parse
        cfgDoctestVersion
        (\x cfg -> cfg { cfgDoctestVersion = x })
    , PU.simpleField "local-ghc-options"
        (error "we don't pretty print")
        parseOptsQ
        cfgLocalGhcOptions
        (\x cfg -> cfg { cfgLocalGhcOptions = cfgLocalGhcOptions cfg ++ x })
    , PU.boolField  "cache"
        cfgCache
        (\b cfg -> cfg { cfgCache = b })
    , PU.boolField  "cabal-noise"
        cfgNoise
        (\b cfg -> cfg { cfgNoise = b })
    , PU.boolField  "cabal-check"
        cfgCheck
        (\b cfg -> cfg { cfgCheck = b })
    , PU.boolField  "install-dependencies-step"
        cfgInstallDeps
        (\b cfg -> cfg { cfgInstallDeps = b })
    , PU.boolField  "no-tests-no-benchmarks"
        cfgNoTestsNoBench
        (\b cfg -> cfg { cfgNoTestsNoBench = b })
    , PU.boolField  "build-with-installed-step"
        cfgInstalled
        (\b cfg -> cfg { cfgInstalled = b })
    , PU.listField  "irc-channels"
        (error "we don't pretty print")
        PU.parseTokenQ
        cfgIrcChannels
        (\x cfg -> cfg { cfgIrcChannels = x })
    , PU.simpleField "name"
        (error "we don't pretty print")
        (fmap Just PU.parseTokenQ)
        cfgProjectName
        (\x cfg -> cfg { cfgProjectName = x })
    , PU.listField  "branches"
        (error "we don't pretty print")
        PU.parseTokenQ
        cfgOnlyBranches
        (\x cfg -> cfg { cfgOnlyBranches = x })
    , PU.simpleField  "folds"
        (error "we don't pretty print")
        (sepBy parseFoldQ (munch1 isSpace))
        (\cfg -> [\_ -> cfgFolds cfg])
        (\x cfg -> cfg { cfgFolds = foldl' (flip id) (cfgFolds cfg) x })
    ]

parseOptsQ :: ReadP r [String]
parseOptsQ = sepBy PU.parseTokenQ' (munch1 isSpace)

readConfigFile :: MonadIO m => FilePath -> YamlWriter m Config
readConfigFile path = do
    contents <- liftIO $ readFile path
    parseConfigFile path contents

parseConfigFile :: Monad m => FilePath -> String -> YamlWriter m Config
parseConfigFile path contents = toWriter $ do
    fields' <- PU.readFields contents
    let (fields, sections) = partitionEithers (map classify fields')
    config <- accumFields configFieldDescrs emptyConfig fields
    go config sections
  where
    toWriter r = case r of
        PU.ParseOk ws x -> do
            forM_ ws $ \w -> putStrLnWarn (PU.showPWarning path w)
            return x
        PU.ParseFailed err -> case PU.locatedErrorMsg err of
            (l, msg) -> putStrLnErr $ path ++ ":" ++ show l ++ ": " ++ msg

    classify x@PU.IfBlock {} = Right x
    classify x@PU.Section {} = Right x
    classify x@PU.F {}       = Left x

    go :: Config -> [PU.Field] -> PU.ParseResult Config
    go  cfg [] = return cfg
    go _cfg (PU.IfBlock {} : _fields) = fail "if conditional found"
    go  cfg (PU.F {} : fields)        = go cfg fields
    go  cfg (PU.Section line name arg subfields : fields)
        | name == "constraint-set" = do
            cs <- accumFields constraintSetFieldDescrs (emptyConstraintSet arg) subfields
            let cfg' = cfg { cfgConstraintSets = cfgConstraintSets cfg ++ [cs] }
            go cfg' fields
        | otherwise = do
            PU.warning $ "Unknown section " ++ name ++ " on line " ++ show line
            go cfg fields

-------------------------------------------------------------------------------
-- ConstraintSet
-------------------------------------------------------------------------------

data ConstraintSet = ConstraintSet
    { csName        :: String
    , csGhcVersions :: VersionRange
    , csConstraints :: [String] -- we parse these simply as strings
    }
  deriving (Show)

emptyConstraintSet :: String -> ConstraintSet
emptyConstraintSet n = ConstraintSet n anyVersion []

constraintSetFieldDescrs :: [PU.FieldDescr ConstraintSet]
constraintSetFieldDescrs =
    [ PU.listField "constraints"
        (error "we don't pretty print") -- pretty
        (parseHaskellString <++ munch1 (`notElem` [',', '"']))
        csConstraints
        (\c cs -> cs { csConstraints = csConstraints cs ++ c })
    , PU.simpleField "ghc"
        (error "we don't pretty print") -- pretty
        Distribution.Text.parse
        csGhcVersions
        (\c cs -> cs { csGhcVersions = c })
    ]

ghcVersionPredicate :: VersionRange -> String
ghcVersionPredicate = conj . asVersionIntervals
  where
    conj = intercalate "  ||  " . map disj

    disj :: VersionInterval -> String
    disj (LowerBound v InclusiveBound, UpperBound u InclusiveBound)
        | v == u              = "[ $HCNUMVER -eq " ++ f v ++ " ]"
    disj (lb, NoUpperBound)   = lower lb
    disj (lb, UpperBound v b) = lower lb ++ " && " ++ upper v b

    lower (LowerBound v InclusiveBound) = "[ $HCNUMVER -ge " ++ f v ++ " ]"
    lower (LowerBound v ExclusiveBound) = "[ $HCNUMVER -gt " ++ f v ++ " ]"

    upper v InclusiveBound = "[ $HCNUMVER -le " ++ f v ++ " ]"
    upper v ExclusiveBound = "[ $HCNUMVER -lt " ++ f v ++ " ]"

    f v =  case versionNumbers v of
        []        -> "0"
        [x]       -> show (x * 10000)
        [x,y]     -> show (x * 10000 + y * 100)
        (x:y:z:_) -> show (x * 10000 + y * 100 + z)

-------------------------------------------------------------------------------
-- From Cabal
-------------------------------------------------------------------------------

accumFields :: [PU.FieldDescr a] -> a -> [PU.Field] -> PU.ParseResult a
accumFields fields = foldM setField
  where
    fieldMap = M.fromList
        [ (name, f) | f@(PU.FieldDescr name _ _) <- fields ]
    setField accum (PU.F line name value) = case M.lookup name fieldMap of
      Just (PU.FieldDescr _ _ set) -> set line value accum
      Nothing -> do
          PU.warning $ "Unrecognized field " ++ name ++ " on line " ++ show line
          return accum
    setField accum f = do
        PU.warning ("Unrecognized stanza on line " ++ show (PU.lineNo f))
        return accum

-------------------------------------------------------------------------------
-- From cabal-install
-------------------------------------------------------------------------------

-- | This is a bit tricky since it has to cover globs which have embedded @,@
-- chars. But we don't just want to parse strictly as a glob since we want to
-- allow http urls which don't parse as globs, and possibly some
-- system-dependent file paths. So we parse fairly liberally as a token, but
-- we allow @,@ inside matched @{}@ braces.
--
parsePackageLocationTokenQ :: ReadP r String
parsePackageLocationTokenQ = parseHaskellString <++ parsePackageLocationToken
  where
    parsePackageLocationToken :: ReadP r String
    parsePackageLocationToken = fmap fst (gather outerTerm)
      where
        outerTerm   = alternateEither1 outerToken (braces innerTerm)
        innerTerm   = alternateEither  innerToken (braces innerTerm)
        outerToken  = void $ munch1 outerChar
        innerToken  = void $ munch1 innerChar
        outerChar c = not (isSpace c || c == '{' || c == '}' || c == ',')
        innerChar c = not (isSpace c || c == '{' || c == '}')
        braces      = between (char '{') (char '}')

    alternateEither, alternateEither1,
      alternatePQs, alternate1PQs, alternateQsP, alternate1QsP
      :: ReadP r () -> ReadP r () -> ReadP r ()

    alternateEither1 p q = alternate1PQs p q +++ alternate1QsP q p
    alternateEither  p q = alternateEither1 p q +++ return ()
    alternate1PQs    p q = p >> alternateQsP q p
    alternatePQs     p q = alternate1PQs p q +++ return ()
    alternate1QsP    q p = many1 q >> alternatePQs p q
    alternateQsP     q p = alternate1QsP q p +++ return ()

parseHaskellString :: ReadP r String
parseHaskellString = readS_to_P reads

parseInt :: ReadP r Int
parseInt = readS_to_P reads

-------------------------------------------------------------------------------
-- Glob
-------------------------------------------------------------------------------

{-

Globbing code and grammar judiciously stolen from cabal-install:

FilePathGlob    ::= FilePathRoot FilePathGlobRel
FilePathRoot    ::= {- empty -}        # relative to cabal.project
                  | "/"                # Unix root
                  | [a-zA-Z] ":" [/\\] # Windows root
                  | "~"                # home directory

FilePathGlobRel ::= Glob "/"  FilePathGlobRel # Unix directory
                  | Glob "\\" FilePathGlobRel # Windows directory
                  | Glob         # file
                  | {- empty -}  # trailing slash

Glob      ::= GlobPiece *
GlobPiece ::= "*"            # wildcard
            | [^*{},/\\] *   # literal string
            | "\\" [*{},]    # escaped reserved character
            | "{" Glob "," ... "," Glob "}" # union (match any of these)
-}

data FilePathGlob = FilePathGlob FilePathRoot FilePathGlobRel
  deriving (Eq, Show)

data FilePathGlobRel
   = GlobDir  Glob FilePathGlobRel
   | GlobFile Glob
   | GlobDirTrailing -- trailing dir, a glob ending in '/'
  deriving (Eq, Show)

-- | A single directory or file component of a globbed path
type Glob = [GlobPiece]

-- | A piece of a globbing pattern
data GlobPiece = WildCard
               | Literal String
               | Union [Glob]
  deriving (Eq, Show)

data FilePathRoot
   = FilePathRelative
   | FilePathRoot FilePath -- e.g. '/', 'c:\' or result of 'takeDrive'
   | FilePathHomeDir
  deriving (Eq, Show)

parseFilePathGlobRel :: ReadP r FilePathGlobRel
parseFilePathGlobRel =
      parseGlob >>= \globpieces ->
          asDir globpieces
      <++ asTDir globpieces
      <++ asFile globpieces
  where
    asDir  glob = do dirSep
                     globs <- parseFilePathGlobRel
                     return (GlobDir glob globs)
    asTDir glob = do dirSep
                     return (GlobDir glob GlobDirTrailing)
    asFile glob = return (GlobFile glob)

    dirSep = void (char '/')
         +++ (do _ <- char '\\'
                 -- check this isn't an escape code
                 following <- look
                 case following of
                   (c:_) | isGlobEscapedChar c -> pfail
                   _                           -> return ())

parseGlob :: ReadP r Glob
parseGlob = many1 parsePiece
  where
    parsePiece = literal +++ wildcard +++ union'

    wildcard = char '*' >> return WildCard

    union' = between (char '{') (char '}') $
              fmap Union (sepBy1 parseGlob (char ','))

    literal = Literal `fmap` litchars1

    litchar = normal +++ escape

    normal  = satisfy (\c -> not (isGlobEscapedChar c)
                                && c /= '/' && c /= '\\')
    escape  = char '\\' >> satisfy isGlobEscapedChar

    litchars1 :: ReadP r [Char]
    litchars1 = liftM2 (:) litchar litchars

    litchars :: ReadP r [Char]
    litchars = litchars1 <++ return []

isGlobEscapedChar :: Char -> Bool
isGlobEscapedChar '*'  = True
isGlobEscapedChar '{'  = True
isGlobEscapedChar '}'  = True
isGlobEscapedChar ','  = True
isGlobEscapedChar _    = False

expandRelGlob :: MonadIO m => FilePath -> FilePathGlobRel -> m [FilePath]
expandRelGlob root glob0 = liftIO $ go glob0 ""
  where
    go (GlobFile glob) dir = do
      entries <- getDirectoryContents (root </> dir)
      let files = filter (matchGlob glob) entries
      return (map (dir </>) files)

    go (GlobDir glob globPath) dir = do
      entries <- getDirectoryContents (root </> dir)
      subdirs <- filterM (\subdir -> doesDirectoryExist
                                       (root </> dir </> subdir))
               $ filter (matchGlob glob) entries
      concat <$> mapM (\subdir -> go globPath (dir </> subdir)) subdirs

    go GlobDirTrailing dir = return [dir]

matchGlob :: Glob -> FilePath -> Bool
matchGlob = goStart
  where
    -- From the man page, glob(7):
    --   "If a filename starts with a '.', this character must be
    --    matched explicitly."

    go, goStart :: [GlobPiece] -> String -> Bool

    goStart (WildCard:_) ('.':_)  = False
    goStart (Union globs:rest) cs = any (\glob -> goStart (glob ++ rest) cs)
                                        globs
    goStart rest               cs = go rest cs

    go []                 ""    = True
    go (Literal lit:rest) cs
      | Just cs' <- stripPrefix lit cs
                                = go rest cs'
      | otherwise               = False
    go [WildCard]         ""    = True
    go (WildCard:rest)   (c:cs) = go rest (c:cs) || go (WildCard:rest) cs
    go (Union globs:rest)   cs  = any (\glob -> go (glob ++ rest) cs) globs
    go []                (_:_)  = False
    go (_:_)              ""    = False
