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
module MakeTravisYml where

import Control.Applicative ((<$>),(<$),(<*>),(<*),(*>),(<|>), pure)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad
import Data.Char (isAsciiLower, isAsciiUpper, isSpace)
import Data.Either (partitionEithers)
import qualified Data.Foldable as F
import Data.Function
import Data.List
import Data.Maybe
import Data.Monoid (Monoid(..), (<>))
import Data.Set (Set)
import qualified Data.Set as S
import System.Console.GetOpt
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.Environment
import System.Exit
import System.FilePath.Posix ((</>), takeDirectory, takeFileName)
import System.IO
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Writer
import Text.Read (readMaybe)

import Distribution.Compiler (CompilerFlavor(..))
import Distribution.Package hiding (Package, pkgName)
import qualified Distribution.Package as Pkg
import Distribution.PackageDescription (packageDescription, testedWith, package, condLibrary, condTestSuites)
import Distribution.Text
import Distribution.Version
#if MIN_VERSION_Cabal(2,0,0)
import Distribution.PackageDescription.Parse (readGenericPackageDescription)
#else
import Distribution.PackageDescription (GenericPackageDescription)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Verbosity (Verbosity)
#endif
import Text.ParserCombinators.ReadP
    ( ReadP, (<++), between, char, eof, munch, munch1, pfail, readP_to_S
    , satisfy, sepBy, sepBy1, string)

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
    => String
    -> String
    -> Set String
    -> YamlWriter m ()
    -> YamlWriter m ()
foldedTellStrLns label prettyLabel labels output
    | label `S.notMember` labels = output
    | otherwise = tellStrLns [prelude] >> output >> tellStrLns epilogue
  where
    prelude = mconcat
        [ "  - echo ", prettyLabel
        , " && echo -en 'travis_fold:start:", label, "\\\\r'" ]
    epilogue = ["  - echo -en 'travis_fold:end:" ++ label ++ "\\\\r'" ]

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
    , optFolds :: Either [String] (Set String)
    , optIrcChannels :: [String]
    , optProjectName :: Maybe String
    , optOnlyBranches :: [String]
    , optOutput :: Maybe FilePath
    , optRegenerate :: Maybe FilePath
    , optQuietTests :: !Bool
    , optOsx :: [String]
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
    , optOsx = []
    }

possibleFolds :: [String]
possibleFolds =
  [ "sdist", "unpack", "build", "build-installed", "build-everything", "test"
  , "haddock", "stack" ]

setFolds
    :: Maybe String
    -> Either [String] (Set String)
    -> Either [String] (Set String)
setFolds Nothing val = S.fromList possibleFolds <$ val
setFolds (Just "all") val = setFolds Nothing val
setFolds (Just "all-but-test") val = S.delete "test" <$> setFolds Nothing val
setFolds (Just n) val
    | n `elem` possibleFolds = S.insert n <$> val
    | otherwise = case val of
        Left errs -> Left $ errs ++ err
        Right _ -> Left err
    where
      err = ["illegal fold name: '" ++ n ++ "'"]

options :: [OptDescr (Options -> Options)]
options =
    [ Option [] ["no-cache"]
      (NoArg $ \opts -> opts { optNoCache = True })
      "disable Travis caching"
    , Option [] ["no-cabal-noise"]
      (NoArg $ \opts -> opts { optQuietTests = True })
      "remove cabal noise from test output"
    , Option ['c'] ["collection"]
      (ReqArg (\arg opts -> opts { optCollections = arg : optCollections opts }) "CID")
      "enable package collection(s) (e.g. 'lts-7'), use multiple times for multiple collections"
    , Option ['f'] ["fold"]
      (OptArg (\arg opts -> opts { optFolds = setFolds arg (optFolds opts) }) "FOLD")
      ("build output(s) to fold, use multiple times for multiple folds. No argument defaults to 'all'. Possible values: all, all-but-test, " ++ intercalate ", " possibleFolds)
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
      (ReqArg (\arg opts -> opts { optRegenerate = Just arg }) "INPUTOUTPUT")
      "regenerate the file using the magic command in output file"
    , Option [] ["osx"]
      (ReqArg (\arg opts -> opts { optOsx = arg : optOsx opts }) "GHC")
      "generate osx build job with ghc version"
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
    usageMsg errs = concat (map ("*ERROR* "++) errs) ++ usageInfo h options ++ ex
    h = concat
        [ "Usage: make_travis_yml_2.hs [OPTIONS] <cabal-file | cabal.project> <extra-apt-packages...>\n"
        , "\n"
        , "Available options:"
        ]

    ex = unlines
        [ ""
        , "Example:"
        , "  make_travis_yml_2.hs -o .travis.yml someProject.cabal liblzma-dev"
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

data Config = Cfg { hasTests :: Bool, hasLibrary :: Bool }
    deriving (Eq, Show)

data Package = Pkg
    { pkgName :: String
    , pkgDir :: FilePath
    } deriving (Eq, Show)

instance Monoid Config where
    mempty = Cfg False False
    mappend cfg1 cfg2 = Cfg
        { hasTests = hasTests cfg1 || hasTests cfg2
        , hasLibrary = hasLibrary cfg1 || hasLibrary cfg2
        }

genTravisFromConfigFile :: ([String],Options) -> FilePath -> [String] -> IO ()
genTravisFromConfigFile args@(_, opts) path xpkgs =
    runYamlWriter (optOutput opts) $ travisFromConfigFile args path xpkgs

travisFromConfigFile
    :: MonadIO m
    => ([String],Options)
    -> FilePath
    -> [String]
    -> YamlWriter m ()
travisFromConfigFile args@(_, opts) path xpkgs =
  getCabalFiles
    >>= mapM (configFromCabalFile opts)
    >>= checkVersions
    >>= genTravisFromConfigs args xpkgs isCabalProject
  where
    checkVersions
        :: MonadIO m
        => [(Package, Config, Set Version)]
        -> YamlWriter m (Set Version, Config, [Package])
    checkVersions [] = putStrLnErr "Error reading cabal file(s)!"
    checkVersions cfgs = do
        let (errors, cfg, names) = foldl' collectConfig mempty cfgs
        unless (null errors) $ putStrLnErr . intercalate "\n" $ "":errors
        return (allVersions, cfg, names)
      where
        allVersions = S.unions $ map (\(_, _, s) -> s) cfgs

        collectConfig
            :: ([String], Config, [Package])
            -> (Package, Config, Set Version)
            -> ([String], Config, [Package])
        collectConfig aggregate (pkg, config, testWith) =
            aggregate <> (errors, config, [pkg])
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
            result <- readP_to_S projectFile `liftM` liftIO (readFile path)
            globs <- case result of
                [(r,"")] -> return r
                [] -> putStrLnErr $ "Parse error trying to parse: " ++ path
                _ -> putStrLnErr $ "Ambiguous parse trying to parse: " ++ path
            when (null globs) $
                putStrLnErr $ "No 'packages:' entry found in: " ++ path

            expandGlobs (takeDirectory path) globs

configFromCabalFile
    :: MonadIO m => Options -> FilePath -> YamlWriter m (Package, Config, Set Version)
configFromCabalFile opts cabalFile = do
    gpd <- liftIO $ readGenericPackageDescription maxBound cabalFile

    let compilers = testedWith $ packageDescription $ gpd
        pkgNameStr = display $ Pkg.pkgName $ package $ packageDescription gpd

    let unknownComps = nub [ c | (c,_) <- compilers, c /= GHC ]
        ghcVerConstrs = [ vc | (GHC,vc) <- compilers ]
        ghcVerConstrs' = simplifyVersionRange $ foldr unionVersionRanges noVersion ghcVerConstrs
        twoDigitGhcVerConstrs = mapMaybe isTwoDigitGhcVersion ghcVerConstrs :: [Version]
        specificGhcVers = nub $ mapMaybe isSpecificVersion ghcVerConstrs

    when (not . null $ twoDigitGhcVerConstrs) $ do
        putStrLnWarn $ "'tested-with:' uses two digit GHC versions (which don't match any existing GHC version): " ++ (intercalate ", " $ map display twoDigitGhcVerConstrs)
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

    let hasTests = not . null $ condTestSuites gpd
        hasLibrary = case condLibrary gpd of
            Just _ -> True
            Nothing -> False
        pkg = Pkg pkgNameStr (takeDirectory cabalFile)

    return (pkg, Cfg hasTests hasLibrary, S.fromList testedGhcVersions)
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
    lastStableGhcVers = nubBy ((==) `on` ghcMajVer) $ filter (not . isGhcHead) $ reverse $ sort $ knownGhcVersions

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
    -> (Set Version, Config, [Package])
    -> YamlWriter m ()
genTravisFromConfigs (argv,opts) xpkgs isCabalProject (versions,cfg,pkgs) = do
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
        , ("#   make_travis_yml_2.hs " ++ unwords [ "'" ++ a ++ "'" | a <- argv ])
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
        ]

    let haskellOnMacos = "https://haskell.futurice.com/haskell-on-macos.py"

    if null (optOsx opts)
    then tellStrLns
        [ sh "PATH=/opt/ghc/bin:/opt/ghc-ppa-tools/bin:$PATH"
        ]
    else tellStrLns
        [ sh $ "if [ \"$(uname)\" = \"Darwin\" ]; then brew update; brew install python3; curl " ++ haskellOnMacos ++ " | python3 - --make-dirs --install-dir=$HOME/.ghc-install --cabal-alias=head install cabal-install-head ${HC}; fi"
        , sh $ "if [ \"$(uname)\" = \"Darwin\" ]; then PATH=$HOME/.ghc-install/ghc/bin:$PATH; else PATH=/opt/ghc/bin:/opt/ghc-ppa-tools/bin:$PATH; fi"
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
        , sh "travis_retry cabal update -v"
        , sh "sed -i.bak 's/^jobs:/-- jobs:/' ${HOME}/.cabal/config"
        , sh "rm -fv cabal.project.local"
        ]

    when (isNothing isCabalProject) $ generateCabalProject False

    tellStrLns
        [ sh "if $GHCHEAD; then cabal new-update head.hackage -v; fi"
        ]

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
        , "      (cd \"" ++ pkgDir ++ "\"; autoreconf -i);"
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

    foldedTellStrLns "sdist" "Packaging..." folds $ do
        forM_ pkgs $ \Pkg{pkgDir} -> tellStrLns
            [ sh $ "(cd \"" ++ pkgDir ++ "\" && cabal sdist)"
            ]

    let tarFiles = quotedPaths $ \Pkg{pkgDir,pkgName} ->
                pkgDir </> "dist" </> pkgName ++ "-*.tar.gz"


    foldedTellStrLns "unpack" "Unpacking..." folds $ do
        tellStrLns
            [ sh $ "mv " ++ tarFiles ++ " ${DISTDIR}/"
            , sh $ "cd ${DISTDIR} || false" -- fail explicitly, makes SC happier
            , sh $ "find . -maxdepth 1 -name '*.tar.gz' -exec tar -xvf '{}' \\;"
            ]
        generateCabalProject True

    foldedTellStrLns "build" "Building..." folds $ tellStrLns
        [ comment "this builds all libraries and executables (without tests/benchmarks)"
        , sh "cabal new-build -w ${HC} --disable-tests --disable-benchmarks all"
        ]

    tellStrLns [""]

    let msg = "Building with installed constraints for package in global-db..."
    foldedTellStrLns "build-installed" msg folds $ tellStrLns
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

    foldedTellStrLns "build-everything"
        "Building with tests and benchmarks..." folds $ tellStrLns
        [ comment "build & run tests, build benchmarks"
        , sh "cabal new-build -w ${HC} ${TEST} ${BENCH} all"
        ]

    -- cabal new-test fails if there are no test-suites.
    when (hasTests cfg) $
        foldedTellStrLns "test" "Testing..." folds $ tellStrLns
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

    when (hasLibrary cfg) $
        foldedTellStrLns "haddock" "Haddock..." folds $ tellStrLns
            [ comment "haddock"
            , sh "rm -rf ./dist-newstyle"
            , sh "if $HADDOCK; then cabal new-haddock -w ${HC} --disable-tests --disable-benchmarks all; else echo \"Skipping haddock generation\";fi"
            , ""
            ]

    unless (null colls) $
        foldedTellStrLns "stack" "Stack builds..." folds $ tellStrLns
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
    headGhcVers = S.filter isGhcHead versions

    generateCabalProject dist = do
        tellStrLns
            [ sh $ "printf 'packages: " ++ cabalPaths ++ "\\n' > cabal.project"
            ]
        unless (S.null headGhcVers) $ tellStrLns
            [ "  # Overlay Hackage Package Index for GHC HEAD: https://github.com/hvr/head.hackage"
            , "  - |"
            , "    if $GHCHEAD; then"
            , "      echo 'allow-newer: *:base, *:template-haskell' >> cabal.project"
            , "      echo 'repository head.hackage' >> cabal.project"
            , "      echo '   url: http://head.hackage.haskell.org/' >> cabal.project"
            , "      echo '   secure: True' >> cabal.project"
            , "      echo '   root-keys: 07c59cb65787dedfaef5bd5f987ceb5f7e5ebf88b904bbd4c5cbdeb2ff71b740' >> cabal.project"
            , "      echo '              2e8555dde16ebd8df076f1a8ef13b8f14c66bad8eafefd7d9e37d0ed711821fb' >> cabal.project"
            , "      echo '              8f79fd2389ab2967354407ec852cbe73f2e8635793ac446d09461ffb99527f6e' >> cabal.project"
            , "      echo '   key-threshold: 3' >> cabal.project"
            , "    fi"
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
    lookupCabVer v = maybe (error "internal error") id $ lookup (x,y) cabalVerMap
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

prettyPathGlob :: FilePathGlob -> String
prettyPathGlob (FilePathGlob root relGlob) =
  prettyRoot ++ prettyRelPathGlob relGlob
  where
    prettyRoot = case root of
        FilePathRelative -> "./"
        FilePathRoot r -> r
        FilePathHomeDir -> "~/"

prettyRelPathGlob :: FilePathGlobRel -> String
prettyRelPathGlob relPathGlob = case relPathGlob of
  GlobDir globs relGlob -> prettyGlob globs ++ "/" ++ prettyRelPathGlob relGlob
  GlobFile globs -> prettyGlob globs
  GlobDirTrailing -> ""

prettyGlob :: Glob -> String
prettyGlob = concatMap prettyGlobPiece

prettyGlobPiece :: GlobPiece -> String
prettyGlobPiece globPiece = case globPiece of
    WildCard -> "*"
    Literal l -> concatMap escape l
    Union globs -> "{" ++ intercalate "," (map prettyGlob globs) ++ "}"
  where
    escape c | c `elem` reservedChars = ['\\',c]
             | otherwise = [c]

satisfyP :: (a -> Bool) -> ReadP a -> ReadP a
satisfyP f p = p >>= \r -> if f r then return r else pfail

eol :: ReadP ()
eol = void (char '\n') <++ void (string "\r\n")

line :: ReadP ()
line = munch (\c -> c /= '\n' && c /= '\r') >> eol

choice :: [ReadP a] -> ReadP a
choice = foldr (<++) pfail

option :: a -> ReadP a -> ReadP a
option r p = p <++ return r

many :: ReadP a -> ReadP [a]
many = option [] . many1

many1 :: ReadP a -> ReadP [a]
many1 p = (:) <$> p <*> many p

skipSpaces :: ReadP Int
skipSpaces = length <$> munch (\c -> isSpace c && c /= '\n' && c /= '\r')

skipSpaces1 :: ReadP Int
skipSpaces1 = satisfyP (0/=) skipSpaces

emptyLines :: Int -> ReadP Int
emptyLines i = do
    many1 (skipSpaces >> eol)
    satisfyP (>i) skipSpaces

projectFile :: ReadP [FilePathGlob]
projectFile = do
    (i, r) <- findPackages
    many (skipSpaces >> eol)
    satisfyP (<=i) skipSpaces
    many line >> eof
    return r

findPackages :: ReadP (Int, [FilePathGlob])
findPackages = packagesField <++ (line >> findPackages)

packagesField :: ReadP (Int, [FilePathGlob])
packagesField = do
    i <- skipSpaces
    string "packages:"
    emptyLines i <++ skipSpaces
    (,) i <$> globLines i

globLines :: Int -> ReadP [FilePathGlob]
globLines i = sepBy1 filePathGlob (globSep i) <* (skipSpaces >> eol)

globSep :: Int -> ReadP ()
globSep i = void $ choice [comma, nonComma]
  where
    comma = do
        emptyLines i <++ skipSpaces
        char ','
        emptyLines i <++ skipSpaces

    nonComma = emptyLines i <++ satisfyP (>0) skipSpaces

filePathGlob :: ReadP FilePathGlob
filePathGlob = between (char '"') (char '"') (baseGlob True) <++ baseGlob False
  where
    nonEmpty = satisfyP (GlobDirTrailing/=)
    baseGlob isQuoted = choice
        [ FilePathGlob <$> filePathRoot <*> filePathGlobRel isQuoted
        , FilePathGlob FilePathRelative <$> nonEmpty (filePathGlobRel isQuoted)
        ]

filePathRoot :: ReadP FilePathRoot
filePathRoot = unixRoot <++ windowsRoot <++ homeDir
  where
    pathSep = char '\\' <++ char '/'
    unixRoot = FilePathRoot . pure <$> char '/'
    windowsRoot = do
        drive <- satisfy $ \c -> isAsciiUpper c || isAsciiLower c
        char ':'
        sep <- pathSep
        return $ FilePathRoot [drive,':',sep]
    homeDir = FilePathHomeDir <$ char '~' <* pathSep

filePathGlobRel :: Bool -> ReadP FilePathGlobRel
filePathGlobRel b = nonEmptyFilePathGlobRel b <++ return GlobDirTrailing

nonEmptyFilePathGlobRel :: Bool -> ReadP FilePathGlobRel
nonEmptyFilePathGlobRel isQuoted = globDir <++ globFile
  where
    globDir = GlobDir <$> glob isQuoted <* (char '/' <++ char '\\')
                      <*> filePathGlobRel isQuoted
    globFile = GlobFile <$> glob isQuoted

glob :: Bool -> ReadP Glob
glob isQuoted = do
    r <- globPiece isQuoted
    (r:) <$> glob'
  where
    glob' :: ReadP Glob
    glob' = ((:) <$> globPiece isQuoted <*> glob') <++ return []

reservedChars :: [Char]
reservedChars = "*{},\""

globPiece :: Bool -> ReadP GlobPiece
globPiece isQuoted = wildCard <++ union <++ literal
  where
    wildCard = WildCard <$ char '*'
    union = between (char '{') (char '}') $
        Union <$> sepBy (glob isQuoted) (char ',')

    literal = Literal <$> many1 (safeChar <++ escapedChar)
      where
        notReserved c
            | isQuoted = True
            | otherwise = not (isSpace c)
        safeChar = satisfy (\c -> c `notElem` "*{},/\\\"" && notReserved c)
        escapedChar = char '\\' *> satisfy (`elem` reservedChars)

expandGlobs
    :: MonadIO m => FilePath -> [FilePathGlob] -> YamlWriter m [FilePath]
expandGlobs root globs = concat `liftM` mapM (expandGlob root) globs

expandGlob :: MonadIO m => FilePath -> FilePathGlob -> YamlWriter m [FilePath]
expandGlob _ g@(FilePathGlob FilePathHomeDir _) = do
    putStrLnInfo $ "Ignoring home directory glob: " ++ prettyPathGlob g
    return []
expandGlob _ g@(FilePathGlob FilePathRoot{} _) = do
    putStrLnInfo $ "Ignoring global directory glob: " ++ prettyPathGlob g
    return []
expandGlob root g@(FilePathGlob FilePathRelative relGlob) = do
    result <- expandRelGlob root relGlob
    when (null result) $
        putStrLnErr $ "Illegal empty package glob: " ++ prettyPathGlob g
    return result

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

    go GlobDirTrailing dir = go (GlobFile [WildCard,Literal ".cabal"]) dir

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
