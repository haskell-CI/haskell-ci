#! /usr/bin/env runghc
-- NOTE: -XCPP + shebang require at least GHC 7.8.4; prior versions of
-- GHC don't support this. For older GHCs, remove the line and pass
-- this script manually to `runghc` (or compile it)

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

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
module Main where

import Control.Applicative ((<|>))
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad
import Data.Function
import Data.List
import Data.Maybe
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Control.Monad.IO.Class
import Control.Monad.Trans.Writer
import Text.Read (readMaybe)

import Distribution.Compiler (CompilerFlavor(..))
import Distribution.Package
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


#if !(MIN_VERSION_Cabal(2,0,0))
-- compat helpers for pre-2.0

readGenericPackageDescription :: Verbosity -> FilePath -> IO GenericPackageDescription
readGenericPackageDescription = readPackageDescription

mkVersion :: [Int] -> Version
mkVersion vn = Version vn []

versionNumbers :: Version -> [Int]
versionNumbers (Version vn _) = vn

#endif

putStrLnErr, putStrLnWarn, putStrLnInfo :: MonadIO m => String -> m ()
putStrLnErr  m = liftIO $ hPutStrLn stderr ("*ERROR* " ++ m) >> exitFailure
putStrLnWarn m = liftIO $ hPutStrLn stderr ("*WARNING* " ++ m)
putStrLnInfo m = liftIO $ hPutStrLn stderr ("*INFO* " ++ m)

-- putStrLns :: [String] -> IO ()
-- putStrLns = putStr . unlines

tellStrLn :: Monad m => String -> WriterT [String] m ()
tellStrLn str = tell [str]

tellStrLns :: Monad m => [String] -> WriterT [String] m ()
tellStrLns = tell

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
    , optIrcChannels :: [String]
    , optOnlyBranches :: [String]
    , optOutput :: Maybe FilePath
    , optRegenerate :: Maybe FilePath
    } deriving Show

defOptions :: Options
defOptions = Options
    { optNoCache = False
    , optIrcChannels = []
    , optCollections = []
    , optOnlyBranches = []
    , optOutput = Nothing
    , optRegenerate = Nothing
    }

options :: [OptDescr (Options -> Options)]
options =
    [ Option [] ["no-cache"]
      (NoArg $ \opts -> opts { optNoCache = True })
      "disable Travis caching"
    , Option ['c'] ["collection"]
      (ReqArg (\arg opts -> opts { optCollections = arg : optCollections opts }) "CID")
      "enable package collection(s) (e.g. 'lts-7'), use multiple times for multiple collections"
    , Option [] ["irc-channel"]
      (ReqArg (\arg opts -> opts { optIrcChannels = arg : optIrcChannels opts }) "HOST#CHANNEL")
      "enable IRC notifcations to given channel (e.g. 'irc.freenode.org#haskell-lens'), use multiple times for multiple channels"
    , Option [] ["branch"]
      (ReqArg (\arg opts -> opts { optOnlyBranches = arg : optOnlyBranches opts }) "BRANCH")
      "enable builds only for specific brances, use multiple times for multiple branches"
    , Option ['o'] ["output"]
      (ReqArg (\arg opts -> opts { optOutput = Just arg }) "OUTPUT")
      "output file (stdout if omitted)"
    , Option ['r'] ["regerate"]
      (ReqArg (\arg opts -> opts { optRegenerate = Just arg }) "INPUTOUTPUT")
      "regenerate the file using the magic command in output file"
    ]

main :: IO ()
main = do
    argv <- getArgs
    (opts,argv',cabfn,xpkgs) <- parseOpts True argv
    genTravisFromCabalFile (argv',opts) cabfn xpkgs


parseOpts :: Bool -> [String] -> IO (Options, [String], FilePath, [String])
parseOpts regenerate argv = case getOpt Permute options argv of
    (opts',_,[])
      | regenerate, Just fp <- optRegenerate opts -> do
        ls <- fmap lines (readFile fp >>= evaluate . force) -- strict IO
        case findArgv ls of
          Nothing    -> dieCli ["expected REGENDATA line in " ++ fp ++ "\n"]
          Just argv' -> parseOpts False argv'
      where opts = foldl (flip id) defOptions opts'
    (opts,cabfn:xpkgs,[]) -> return (foldl (flip id) defOptions opts,argv,cabfn,xpkgs)
    (_,_,[]) -> dieCli ["expected .cabal fle as first non-option argument\n"]
    (_,_,errs) -> dieCli errs
  where
    findArgv :: [String] -> Maybe [String]
    findArgv ls = do
        l <- findMaybe (afterInfix "REGENDATA") ls
        readMaybe l

    dieCli errs = hPutStrLn stderr (usageMsg errs) >> exitFailure
    usageMsg errs = concat (map ("*ERROR* "++) errs) ++ usageInfo h options ++ ex
    h = concat
        [ "Usage: make_travis_yml_2.hs [OPTIONS] <cabal-file> <extra-apt-packages...>\n"
        , "\n"
        , "Available options:"
        ]

    ex = unlines
        [ ""
        , "Example:"
        , "  make_travis_yml_2.hs -o .travis.yml someProject.cabal liblzma-dev"
        ]

runFileWriter :: Maybe FilePath -> WriterT [String] IO () -> IO ()
runFileWriter mfp m = do
    contents <- fmap unlines (execWriterT m)
    case mfp of
        Nothing -> putStr contents
        Just fp -> writeFile fp contents

genTravisFromCabalFile :: ([String],Options) -> FilePath -> [String] -> IO ()
genTravisFromCabalFile (argv,opts) fn xpkgs = runFileWriter (optOutput opts) $ do
    gpd <- liftIO $ readGenericPackageDescription maxBound fn

    let compilers = testedWith $ packageDescription $ gpd
        pkgNameStr = display $ pkgName $ package $ packageDescription gpd

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
                     [ "empty or missing top-level 'tested-with:' definition in .cabal file; example definition:"
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
               [ "collection " ++ c ++ " requires GHC " ++ disp' v
               , "add 'tested-width: GHC == " ++ disp' v ++ "' to your .cabal file"
               ]

    putStrLnInfo $ "Generating Travis-CI config for testing for GHC versions: " ++ (unwords $ map disp' $ testedGhcVersions)

    ----------------------------------------------------------------------------
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

    unless (null $ optIrcChannels opts) $ tellStrLns $
        [ "notifications:"
        , "  irc:"
        , "    channels:"
        ] ++
        [ "      - \"" ++ chan ++ "\"" | chan <- optIrcChannels opts ] ++
        [ "    skip_join: true"
        , "    template:"
        , "      - \"\\x0313" ++ pkgNameStr ++ "\\x03/\\x0306%{branch}\\x03 \\x0314%{commit}\\x03 %{build_url} %{message}\""
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
        , ""
        , "before_cache:"
        , "  - rm -fv $HOME/.cabal/packages/hackage.haskell.org/build-reports.log"
        , "  # remove files that are regenerated by 'cabal update'"
        , "  - rm -fv $HOME/.cabal/packages/hackage.haskell.org/00-index.*" -- legacy
        , "  - rm -fv $HOME/.cabal/packages/hackage.haskell.org/*.json" -- TUF meta-data
        , "  - rm -fv $HOME/.cabal/packages/hackage.haskell.org/01-index.cache"
        , "  - rm -fv $HOME/.cabal/packages/hackage.haskell.org/01-index.tar"
        , "  - rm -fv $HOME/.cabal/packages/hackage.haskell.org/01-index.tar.idx"
        , ""
        ]

    tellStrLn "matrix:"
    tellStrLn "  include:"

    let colls = [ (collToGhcVer cid,cid) | cid <- reverse $ optCollections opts ]

    forM_ testedGhcVersions $ \gv -> do
        let cvs = disp' (lookupCabVer gv)
            gvs = disp' gv

            xpkgs' = concatMap (',':) xpkgs

            colls' = [ cid | (v,cid) <- colls, v == gv ]

        tellStrLns
            [ concat [ "    - compiler: \"ghc-", gvs, "\"" ]
            , if null colls' then
                       "    # env: TEST=--disable-tests BENCH=--disable-benchmarks"
              else
                      ("      env: 'COLLECTIONS=" ++ intercalate "," colls' ++ "'")
            , concat [ "      addons: {apt: {packages: [ghc-ppa-tools,cabal-install-", cvs, ",ghc-", gvs, xpkgs', "], sources: [hvr-ghc]}}" ]
            ]
        return ()

    let headGhcVers = filter isHead testedGhcVersions

    unless (null headGhcVers) $ do
        tellStrLn ""
        tellStrLn "  allow_failures:"

    forM_ headGhcVers $ \gv -> do
        let gvs = disp' gv
        tellStrLn $ concat [ "    - compiler: \"ghc-", gvs, "\"" ]

    tellStrLns
        [ ""
        , "before_install:"
        , " - HC=${CC}"
        , " - HCPKG=${HC/ghc/ghc-pkg}"
        , " - unset CC"
        , " - PATH=/opt/ghc/bin:/opt/ghc-ppa-tools/bin:$PATH"
        , " - PKGNAME='" ++ pkgNameStr ++ "'"
        ]

    unless (null colls) $
       tellStrLn " - IFS=', ' read -a COLLS <<< \"$COLLECTIONS\""

    tellStrLns
        [ ""
        , "install:"
        , " - cabal --version"
        , " - echo \"$(${HC} --version) [$(${HC} --print-project-git-commit-id 2> /dev/null || echo '?')]\""
        , " - BENCH=${BENCH---enable-benchmarks}"
        , " - TEST=${TEST---enable-tests}"
        , " - HADDOCK=${HADDOCK-true}"
        , " - INSTALLED=${INSTALLED-true}"
        , " - travis_retry cabal update -v"
        , " - sed -i.bak 's/^jobs:/-- jobs:/' ${HOME}/.cabal/config"
        , " - rm -fv cabal.project.local"
        , " - \"echo 'packages: .' > cabal.project\""
        ]

    unless (null colls) $ tellStrLns
        [ " - for COLL in \"${COLLS[@]}\"; do"
        , "     echo \"== collection $COLL ==\";"
        , "     ghc-travis collection ${COLL} > /dev/null || break;"
        , "     ghc-travis collection ${COLL} | grep -Fv \" ${PKGNAME} ==\" > cabal.project.freeze;"
        , "     grep ' collection-id' cabal.project.freeze;"
        , "     rm -rf dist-newstyle/;"
        , "     cabal new-build -w ${HC} ${TEST} ${BENCH} --dep -j2 all;"
        , "   done"
        ]

    tellStrLns
        [ " - rm -f cabal.project.freeze"
        , " - cabal new-build -w ${HC} ${TEST} ${BENCH} --dep -j2 all"
        , " - cabal new-build -w ${HC} --disable-tests --disable-benchmarks --dep -j2 all"
        , ""
        , "# Here starts the actual work to be performed for the package under test;"
        , "# any command which exits with a non-zero exit code causes the build to fail."
        , "script:"
        , " - if [ -f configure.ac ]; then autoreconf -i; fi"
        , " - rm -rf .ghc.environment.* dist/"
        , " - cabal sdist # test that a source-distribution can be generated"
        , " - cd dist/"
        , " - SRCTAR=(${PKGNAME}-*.tar.gz)"
        , " - SRC_BASENAME=\"${SRCTAR/%.tar.gz}\""
        , " - tar -xvf \"./$SRC_BASENAME.tar.gz\""
        , " - cd \"$SRC_BASENAME/\""
        , "## from here on, CWD is inside the extracted source-tarball"
        , " - rm -fv cabal.project.local"
        , " - \"echo 'packages: .' > cabal.project\""
        , " # this builds all libraries and executables (without tests/benchmarks)"
        , " - rm -f cabal.project.freeze"
        , " - cabal new-build -w ${HC} --disable-tests --disable-benchmarks all"
        , " # this builds all libraries and executables (including tests/benchmarks)"
        , " # - rm -rf ./dist-newstyle"
        , ""
        ]

    tellStrLns
        [ " # Build with installed constraints for packages in global-db"
        , " - if $INSTALLED; then"
        , "     echo cabal new-build -w ${HC} --disable-tests --disable-benchmarks $(${HCPKG} list --global --simple-output --names-only | sed 's/\\([a-zA-Z0-9-]\\{1,\\}\\) */--constraint=\"\\1 installed\" /g') all | sh;"
        , "   else echo \"Not building with installed constraints\"; fi"
        , ""
        ]

    tellStrLns
        [ " # build & run tests, build benchmarks"
        , " - cabal new-build -w ${HC} ${TEST} ${BENCH} all"
        ]

    -- cabal new-test fails if there are no test-suites.
    unless (null $ condTestSuites gpd) $ tellStrLns
        [ " - if [ \"x$TEST\" = \"x--enable-tests\" ]; then cabal new-test -w ${HC} ${TEST} all; fi"
        ]

    tellStrLns
        [ ""
        ]

    unless (isNothing $ condLibrary gpd) $ tellStrLns
        [ " # haddock"
        , " - rm -rf ./dist-newstyle"
        , " - if $HADDOCK; then cabal new-haddock -w ${HC} --disable-tests --disable-benchmarks all; else echo \"Skipping haddock generation\";fi"
        , ""
        ]

    unless (null colls) $ tellStrLns
        [ " # try building & testing for package collections"
        , " - for COLL in \"${COLLS[@]}\"; do"
        , "     echo \"== collection $COLL ==\";"
        , "     ghc-travis collection ${COLL} > /dev/null || break;"
        , "     ghc-travis collection ${COLL} | grep -Fv \" ${PKGNAME} ==\" > cabal.project.freeze;"
        , "     grep ' collection-id' cabal.project.freeze;"
        , "     rm -rf dist-newstyle/;"
        , "     cabal new-build -w ${HC} ${TEST} ${BENCH} all || break;"
        , "     if [ \"x$TEST\" = \"x--enable-tests\" ]; then cabal new-test -w ${HC} ${TEST} all || break; fi;"
        , "   done"
        , ""
        ]

    tellStrLns
        [ "# REGENDATA " ++ show argv
        , "# EOF"
        ]

    return ()
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
                       , [8,2,1]
                       , [8,3] -- HEAD
                       ]

    lastStableGhcVers :: [Version]
    lastStableGhcVers = nubBy ((==) `on` majVer) $ filter (not . isHead) $ reverse $ sort $ knownGhcVersions

    majVer :: Version -> (Int,Int)
    majVer v
      | x:y:_ <- versionNumbers v = (x,y)
      | otherwise = undefined

    lookupCabVer :: Version -> Version
    lookupCabVer v = maybe (error "internal error") id $ lookup (x,y) cabalVerMap
      where
        (x,y) = majVer v
        cabalVerMap = fmap (fmap mkVersion)
                      [ ((7, 0),  [1,25]) -- Use HEAD for everything.
                      , ((7, 2),  [1,25])
                      , ((7, 4),  [1,25])
                      , ((7, 6),  [1,25])
                      , ((7, 8),  [1,25])
                      , ((7,10),  [1,25])
                      , ((8, 0),  [1,25])
                      , ((8, 2),  [1,25])
                      , ((8, 3),  [1,25])
                      ]

    isHead v
      | (_,y) <- majVer v = odd y
      | otherwise         = False

    disp' v | isHead v = "head"
            | otherwise = display v

    isTwoDigitGhcVersion :: VersionRange -> Maybe Version
    isTwoDigitGhcVersion vr = isSpecificVersion vr >>= t
      where
        t v | [_,_] <- versionNumbers v = Just v
        t _                             = Nothing

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
