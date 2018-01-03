{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiWayIf #-}

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
    travisFromConfigFile, MakeTravisOutput(..), Options (..), defOptions, options,
    ) where

import Control.Applicative ((<$>),(<$),(<|>), pure)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (void, when, unless, filterM, liftM, liftM2, forM_, mzero, foldM)
import Data.Char (isSpace, isUpper, toLower)
import qualified Data.Foldable as F
import Data.Function
import Data.List
import Data.Maybe
import Data.Monoid (Monoid(..), (<>))
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
    , satisfy, sepBy1, gather)

#ifdef MIN_VERSION_ShellCheck
import ShellCheck.Checker (checkScript)
import qualified ShellCheck.Interface as SC
import qualified ShellCheck.Formatter.Format as SC
import qualified ShellCheck.Formatter.TTY as SC.TTY

import Data.Functor.Identity (Identity (..))
import System.IO.Unsafe (unsafePerformIO)
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

ghcAlpha :: Maybe Version
ghcAlpha = Just $ mkVersion [8,4,1]

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

data MakeTravisOutput
    = Success [String] [String]
    | Failure [String]
    deriving (Eq, Show)

instance Monoid MakeTravisOutput where
    mempty = Success [] []
    Failure err1 `mappend` Failure err2 = Failure $ err1 `mappend` err2
    Failure err1 `mappend` Success err2 _ = Failure $ err1 `mappend` err2
    Success err1 _ `mappend` Failure err2 = Failure $ err1 `mappend` err2
    Success l1 o1 `mappend` Success l2 o2 =
        Success (mappend l1 l2) (mappend o1 o2)

-- MaybeT is used to preserve the short-circuiting semantics of 'putStrLnErr'.
type YamlWriter m a = MaybeT (WriterT MakeTravisOutput m) a

putStrLnErr :: Monad m => String -> YamlWriter m a
putStrLnErr m = do
    lift . tell $ Failure ["*ERROR* " ++ m]
    mzero

putStrLnWarn, putStrLnInfo :: Monad m => String -> YamlWriter m ()
putStrLnWarn m = lift . tell $ Success ["*WARNING* " ++ m] []
putStrLnInfo m = lift . tell $ Success ["*INFO* " ++ m] []

tellStrLn :: Monad m => String -> YamlWriter m ()
tellStrLn str = lift . tell $ Success [] [str]

tellStrLns :: Monad m => [String] -> YamlWriter m ()
tellStrLns = lift . tell . Success []

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
afterInfix :: Eq a => [a] -> [a] -> Maybe [a]
afterInfix needle haystack = findMaybe (afterPrefix needle) (tails haystack)

afterPrefix :: Eq a => [a] -> [a] -> Maybe [a]
afterPrefix needle haystack
    | needle `isPrefixOf` haystack = Just (drop (length needle) haystack)
    | otherwise                    = Nothing

findMaybe :: (a -> Maybe b) -> [a] -> Maybe b
findMaybe f = foldr (\a b -> f a <|> b) Nothing

data Options = Options
    { optNoCache :: !Bool
    , optCollections :: [String]
    , optFolds :: Either [String] (Set Fold)
    , optIrcChannels :: [String]
    , optProjectName :: Maybe String
    , optOnlyBranches :: [String]
    , optOutput :: Maybe FilePath
    , optRegenerate :: Maybe FilePath
    , optQuietTests :: !Bool
    , optNoCheck :: !Bool
    , optOsx :: [String]
    , optJobs :: Maybe String
    , optDoctest :: Maybe String
    , optHLint :: Maybe FilePath
    , optConfig :: Maybe FilePath
    } deriving Show

defOptions :: Options
defOptions = Options
    { optNoCache = False
    , optIrcChannels = []
    , optCollections = []
    , optFolds = Right S.empty
    , optProjectName = Nothing
    , optOnlyBranches = []
    , optOutput = Nothing
    , optRegenerate = Nothing
    , optQuietTests = False
    , optNoCheck = False
    , optOsx = []
    , optJobs = Nothing
    , optDoctest = Nothing
    , optHLint = Nothing
    , optConfig = Nothing
    }

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

setFolds
    :: Maybe String
    -> Either [String] (Set Fold)
    -> Either [String] (Set Fold)
setFolds Nothing val = S.fromList possibleFolds <$ val
setFolds (Just "all") val = setFolds Nothing val
setFolds (Just "all-but-test") val = S.delete FoldTest <$> setFolds Nothing val
setFolds (Just n) val
    | Just n' <- M.lookup n ps = S.insert n' <$> val
    | otherwise = case val of
        Left errs -> Left $ errs ++ err
        Right _ -> Left err
    where
      err = ["illegal fold name: '" ++ n ++ "'"]
      ps = M.fromList $ map (\x -> (showFold x, x)) possibleFolds

options :: [OptDescr (Options -> Options)]
options =
    [ Option [] ["no-cache"]
      (NoArg $ \opts -> opts { optNoCache = True })
      "disable Travis caching"
    , Option [] ["no-cabal-noise"]
      (NoArg $ \opts -> opts { optQuietTests = True })
      "remove cabal noise from test output"
    , Option [] ["no-cabal-check"]
      (NoArg $ \opts -> opts { optNoCheck = True })
      "diable cabal check"
    , Option ['c'] ["collection"]
      (ReqArg (\arg opts -> opts { optCollections = arg : optCollections opts }) "CID")
      "enable package collection(s) (e.g. 'lts-7'), use multiple times for multiple collections"
    , Option ['f'] ["fold"]
      (OptArg (\arg opts -> opts { optFolds = setFolds arg (optFolds opts) }) "FOLD")
      ("build output(s) to fold, use multiple times for multiple folds. No argument defaults to 'all'. Possible values: all, all-but-test, " ++ intercalate ", " (map showFold possibleFolds))
    , Option [] ["irc-channel"]
      (ReqArg (\arg opts -> opts { optIrcChannels = arg : optIrcChannels opts }) "HOST#CHANNEL")
      "enable IRC notifcations to given channel (e.g. 'irc.freenode.org#haskell-lens'), use multiple times for multiple channels"
    , Option ['n'] ["name"]
      (ReqArg (\arg opts -> opts { optProjectName = Just arg }) "NAME")
      "project name (used for IRC notifications), defaults to package name or name of first package listed in cabal.project file"
    , Option ['b'] ["branch"]
      (ReqArg (\arg opts -> opts { optOnlyBranches = arg : optOnlyBranches opts }) "BRANCH")
      "enable builds only for specific brances, use multiple times for multiple branches"
    , Option ['o'] ["output"]
      (ReqArg (\arg opts -> opts { optOutput = Just arg }) "OUTPUT")
      "output file (stdout if omitted)"
    , Option ['r'] ["regenerate"]
      (OptArg (\arg opts -> opts { optRegenerate = Just $ fromMaybe ".travis.yml" arg }) "INPUTOUTPUT")
      "regenerate the file using the magic command in output file. Default: .travis.yml"
    , Option [] ["config"]
      (OptArg (\arg opts -> opts { optConfig = Just $ fromMaybe "cabal.make-travis-yml" arg }) "CONFIG")
      "config file, currently used only to specify constraint sets"
    , Option [] ["osx"]
      (ReqArg (\arg opts -> opts { optOsx = arg : optOsx opts }) "GHC")
      "generate osx build job with ghc version"
    , Option ['j'] ["jobs"]
      (ReqArg (\arg opts -> opts { optJobs = Just arg }) "JOBS")
      "jobs (N:M - cabal:ghc)"
    , Option ['d'] ["doctest"]
      (OptArg (\arg opts -> opts { optDoctest = Just $ maybe "" (' ' :) arg }) "OPTIONS")
      "Run doctest using .ghc.environment files. You can supply additional doctest options as an optional argument."
    , Option ['l'] ["hlint"]
      (OptArg (\arg opts -> opts { optHLint = Just $ fromMaybe "" arg }) "HLINT.YAML")
      "Run hlint (only on GHC-8.2.2 target). Specify relative path to .hlint.yaml."
    ]

main :: IO ()
main = do
    argv <- getArgs
    (opts,argv',configFile,xpkgs) <- parseOpts True argv
    genTravisFromConfigFile (argv',opts) configFile xpkgs

parseOpts :: Bool -> [String] -> IO (Options, [String], FilePath, [String])
parseOpts regenerate argv = case getOpt Permute options argv of
    (opts',_,[])
      | regenerate, Just fp <- optRegenerate opts -> do
        ls <- fmap lines (readFile fp >>= evaluate . force) -- strict IO
        case findArgv ls of
          Nothing    -> dieCli ["expected REGENDATA line in " ++ fp ++ "\n"]
          Just argv' -> parseOpts False argv'
      where opts = foldl (flip id) defOptions opts'
    (opts,configFile:xpkgs,[]) -> return (foldl (flip id) defOptions opts,argv,configFile,xpkgs)
    (_,_,[]) -> dieCli ["expected .cabal or cabal.project file as first non-option argument\n"]
    (_,_,errs) -> dieCli errs
  where
    findArgv :: [String] -> Maybe [String]
    findArgv ls = do
        l <- findMaybe (afterInfix "REGENDATA") ls
        readMaybe l

    dieCli errs = hPutStrLn stderr (usageMsg errs) >> exitFailure
    usageMsg errs = concatMap ("*ERROR* "++) errs ++ usageInfo h options ++ ex
    h = concat
        [ "Usage: runghc make_travis_yml_2.hs [OPTIONS] <cabal-file | cabal.project> <extra-apt-packages...>\n"
        , "\n"
        , "Available options:"
        ]

    ex = unlines
        [ ""
        , "Example:"
        , "  runghc make_travis_yml_2.hs -o .travis.yml someProject.cabal liblzma-dev"
        ]

runYamlWriter :: Maybe FilePath -> YamlWriter IO () -> IO ()
runYamlWriter mfp m = do
    result <- execWriterT (runMaybeT m)
    case result of
        Failure (unlines -> errors) -> hPutStr stderr errors >> exitFailure
        Success (unlines -> warnings) (unlines -> contents) -> do
            hPutStr stderr warnings
            case mfp of
                Nothing -> putStr contents
                Just fp -> writeFile fp contents

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
    pkgs <- mapM (configFromCabalFile opts) cabalFiles
    config <- maybe (return emptyConfig) readConfigFile (optConfig opts)
    checkVersions pkgs >>= genTravisFromConfigs args xpkgs isCabalProject config
  where
    checkVersions
        :: MonadIO m
        => [(Package, Set Version)]
        -> YamlWriter m (Set Version, [Package])
    checkVersions [] = putStrLnErr "Error reading cabal file(s)!"
    checkVersions cfgs = do
        let (errors, names) = foldl' collectConfig mempty cfgs
        unless (null errors) $ putStrLnErr . intercalate "\n" $ "":errors
        return (allVersions, names)
      where
        allVersions = S.unions $ map snd cfgs

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

    getCabalFiles :: MonadIO m => YamlWriter m [FilePath]
    getCabalFiles
        | isNothing isCabalProject = return [path]
        | otherwise = do
            contents <- liftIO $ readFile path
            pkgs <- either putStrLnErr return $ parseProjectFile path contents
            concat `liftM` mapM findProjectPackage pkgs

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

    lastStableGhcVers :: [Version]
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
    -> (Set Version, [Package])
    -> YamlWriter m ()
genTravisFromConfigs (argv,opts) xpkgs isCabalProject config (versions, pkgs) = do
    folds <- case optFolds opts of
        Left errs -> putStrLnErr $ unlines errs
        Right val -> return val

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

    let projectName = fromMaybe (pkgName $ head pkgs) (optProjectName opts)
    unless (null $ optIrcChannels opts) $ tellStrLns $
        [ "notifications:"
        , "  irc:"
        , "    channels:"
        ] ++
        [ "      - \"" ++ chan ++ "\"" | chan <- optIrcChannels opts ] ++
        [ "    skip_join: true"
        , "    template:"
        , "      - \"\\x0313" ++ projectName ++ "\\x03/\\x0306%{branch}\\x03 \\x0314%{commit}\\x03 %{build_url} %{message}\""
        , ""
        ]

    unless (null $ optOnlyBranches opts) $ tellStrLns $
        [ "branches:"
        , "  only:"
        ] ++
        [ "    - " ++ branch
        | branch <- optOnlyBranches opts
        ] ++
        [ ""
        ]

    unless (optNoCache opts) $ tellStrLns
        [ "cache:"
        , "  directories:"
        , "    - $HOME/.cabal/packages"
        , "    - $HOME/.cabal/store"
        ]

    -- on OSX ghc is installed in $HOME so we can cache it
    -- independently of linux
    unless (optNoCache opts || null (optOsx opts)) $ tellStrLns
        [ "    - $HOME/.ghc-install"
        ]

    unless (optNoCache opts) $ tellStrLns
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
        , "  - rm -fv $HOME/.cabal/packages/head.hackage" -- if we cache, it will break builds.
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
    case parseJobsM (optJobs opts) of
        (Just n, _) -> tellStrLns
            [ sh $ "sed -i.bak 's/^-- jobs:.*/jobs: " ++ show n ++ "/' ${HOME}/.cabal/config"
            ]
        _ -> return ()

    -- GHC jobs
    case parseJobsM (optJobs opts) of
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
    when (isJust $ optDoctest opts) $ tellStrLns
        [ sh "if [ $HCNUMVER -ge 80000 ]; then cabal new-install -w ${HC} --symlink-bindir=$HOME/.local/bin doctest; fi"
        ]

    -- Install hlint
    when (isJust $ optHLint opts) $ tellStrLns
        [ sh "if [ $HCNUMVER -eq 80202 ]; then cabal new-install -w ${HC} --symlink-bindir=$HOME/.local/bin hlint; fi"
        ]

    -- create cabal.project file
    when (isNothing isCabalProject) $ generateCabalProject False

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
        , sh $ "cabal new-build -w ${HC} ${TEST} ${BENCH} --project-file=\"" ++ projectFile ++"\" --dep -j2 all"
        , sh $ "cabal new-build -w ${HC} --disable-tests --disable-benchmarks --project-file=\"" ++ projectFile ++ "\" --dep -j2 all"
        , sh $ "rm -rf " ++ quotedRmPaths
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

    foldedTellStrLns FoldBuild "Building..." folds $ tellStrLns
        [ comment "this builds all libraries and executables (without tests/benchmarks)"
        , sh "cabal new-build -w ${HC} --disable-tests --disable-benchmarks all"
        ]

    tellStrLns [""]

    foldedTellStrLns FoldBuildInstalled
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
                [ "if [ \"x$TEST\" = \"x--enable-tests\" ]; then cabal "
                , if optQuietTests opts
                     then "-vnormal+nowrap+markoutput "
                     else ""
                , "new-test -w ${HC} ${TEST} all"
                , if optQuietTests opts
                     then " | sed '/^-----BEGIN CABAL OUTPUT-----$/,/^-----END CABAL OUTPUT-----$/d'"
                     else ""
                , "; fi"
                ]
            ]

    tellStrLns [""]

    F.forM_ (optDoctest opts) $ \doctestOptions -> do
        tellStrLns [ comment "doctest" ]
        foldedTellStrLns FoldDoctest "Doctest..." folds $ do
            forM_ pkgs $ \Pkg{pkgName,pkgGpd} -> do
                let args = doctestArgs pkgGpd
                    args' = unwords args
                unless (null args) $ tellStrLns
                    [ sh $ "if [ $HCNUMVER -ge 80000 ]; then (cd " ++ pkgName ++ "-* && doctest" ++ doctestOptions ++ " " ++ args' ++ "); fi"
                    ]
        tellStrLns [ "" ]

    F.forM_ (optHLint opts) $ \hlintYaml -> do
        let hlintOptions | null hlintYaml = ""
                         | otherwise      = " -h ${ROOTDIR}/" ++ hlintYaml
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

    unless (optNoCheck opts) $
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
            , sh "if $HADDOCK; then cabal new-haddock -w ${HC} --disable-tests --disable-benchmarks all; else echo \"Skipping haddock generation\";fi"
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
            , "      if [ \"x$TEST\" = \"x--enable-tests\" ]; then cabal new-test -w ${HC} ${TEST} all || break; fi;"
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

    generateCabalProject dist = tellStrLns
        [ sh $ "printf 'packages: " ++ cabalPaths ++ "\\n' > cabal.project"
        , sh $ "cat cabal.project"
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
-- >>> parseJobs "2:2"
-- (Just 2,Just 2)
--
-- >>> parseJobs ":2"
-- (Nothing,Just 2)
--
-- >>> parseJobs "2"
-- (Just 2,Nothing)
--
-- >>> parseJobs "garbage"
-- (Nothing,Nothing)
--
parseJobs :: String -> (Maybe Int, Maybe Int)
parseJobs input = case filter (null . snd) $ readP_to_S jobsP input of
    [(r, "")] -> r
    _         -> (Nothing, Nothing)
  where
    jobsP :: ReadP r (Maybe Int, Maybe Int)
    jobsP = nm <++ m <++ n <++ return (Nothing, Nothing)

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

parseJobsM :: Maybe String -> (Maybe Int, Maybe Int)
parseJobsM = maybe (Nothing, Nothing) parseJobs

-------------------------------------------------------------------------------
-- Project file
-------------------------------------------------------------------------------

-- | Parse project file. Extracts only @packages@ field.
--
-- >>> parseProjectFile "cabal.project" "packages: foo bar/*.cabal"
-- Right ["foo","bar/*.cabal"]
--
parseProjectFile :: FilePath -> String -> Either String [String]
parseProjectFile path contents =
    case PU.parseFields legacyProjectConfigFieldDescrs [] contents of
        PU.ParseOk _ x -> Right x
        PU.ParseFailed err -> Left $ case PU.locatedErrorMsg err of
            (l, msg) -> "ERROR " ++ path ++ ":" ++ show l ++ ": " ++ msg

legacyProjectConfigFieldDescrs :: [PU.FieldDescr [String]]
legacyProjectConfigFieldDescrs =
    [ PU.listField "packages"
        (error "we don't pretty print") -- pretty
        parsePackageLocationTokenQ -- parse
        id           -- getter
        const        -- setter
    ]

-------------------------------------------------------------------------------
-- Config file
-------------------------------------------------------------------------------

newtype Config = Config
    { cfgConstraintSets :: [ConstraintSet]
    }
  deriving (Show)

emptyConfig :: Config
emptyConfig = Config []

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

readConfigFile :: MonadIO m => FilePath -> YamlWriter m Config
readConfigFile path = do
    contents <- liftIO $ readFile path
    either putStrLnErr return (parseConfigFile path contents)

parseConfigFile :: FilePath -> String -> Either String Config
parseConfigFile path contents = toEither $ do
    fields <- PU.readFields contents
    go emptyConfig fields
  where
    toEither r = case r of
        PU.ParseOk _ x -> Right x
        PU.ParseFailed err -> Left $ case PU.locatedErrorMsg err of
            (l, msg) -> "ERROR " ++ path ++ ":" ++ show l ++ ": " ++ msg

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
