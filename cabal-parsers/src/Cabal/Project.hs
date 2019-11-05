{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
-- | License: GPL-3.0-or-later AND BSD-3-Clause
--
module Cabal.Project (
    -- * Project
    Project (..),
    triverseProject,
    emptyProject,
    -- * Parse project
    parseProject,
    -- * Resolve project
    resolveProject,
    ResolveError (..),
    renderResolveError,
    -- * Read packages
    readPackagesOfProject
    ) where

import Control.Exception            (Exception (..))
import Control.Monad.IO.Class       (liftIO)
import Control.Monad.Trans.Except   (ExceptT, runExceptT, throwE)
import Data.Bifoldable              (Bifoldable (..))
import Data.Bifunctor               (Bifunctor (..))
import Data.Bitraversable           (Bitraversable (..), bifoldMapDefault, bimapDefault)
import Data.ByteString              (ByteString)
import Data.Either                  (partitionEithers)
import Data.Functor                 (void)
import Data.Traversable             (for)
import Data.Void                    (Void)
import Distribution.Compat.Lens     (LensLike', over)
import GHC.Generics                 (Generic)
import Network.URI                  (URI, parseURI)
import System.Directory             (doesDirectoryExist, doesFileExist)
import System.FilePath              (takeDirectory, takeExtension, (</>))
import Text.ParserCombinators.ReadP (readP_to_S)

import qualified Data.ByteString                              as BS
import qualified Data.Map.Strict                              as M
import qualified Distribution.CabalSpecVersion                as C
import qualified Distribution.FieldGrammar                    as C
import qualified Distribution.Fields                          as C
import qualified Distribution.PackageDescription              as C
import qualified Distribution.PackageDescription.FieldGrammar as C
import qualified Distribution.PackageDescription.Parsec       as C
import qualified Distribution.Parsec                          as C
import qualified Distribution.Parsec.Newtypes                 as C

import Cabal.Internal.Glob
import Cabal.Internal.Newtypes
import Cabal.Optimization
import Cabal.Parse

infixl 1 <&>
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

-- $setup
-- >>> :set -XOverloadedStrings

-- | @cabal.project@ file
data Project uri opt pkg = Project
    { prjPackages     :: [pkg]  -- ^ packages field
    , prjOptPackages  :: [opt]  -- ^ optional packages
    , prjUriPackages  :: [uri]  -- ^ URI packages, filled in by 'resolveProject'
    , prjConstraints  :: [String] -- ^ constaints, parsed as 'String's.
    , prjAllowNewer   :: [String] -- ^ allow-newer, parsed as 'String's.
    , prjReorderGoals :: Bool
    , prjMaxBackjumps :: Maybe Int
    , prjOptimization :: Optimization
    , prjSourceRepos  :: [C.SourceRepo]
    , prjOrigFields   :: [C.PrettyField ()] -- ^ original fields
    }
  deriving (Functor, Foldable, Traversable, Generic)

-- | Doesn't compare prjOrigFields
instance (Eq uri, Eq opt, Eq pkg) => Eq (Project uri opt pkg) where
    x == y = and
        [ eqOn prjPackages
        , eqOn prjOptPackages
        , eqOn prjUriPackages
        , eqOn prjConstraints
        , eqOn prjAllowNewer
        , eqOn prjReorderGoals
        , eqOn prjMaxBackjumps
        , eqOn prjOptimization
        , eqOn prjSourceRepos
        ]
      where
        eqOn f = f x == f y

instance Bifunctor (Project c) where bimap = bimapDefault
instance Bifoldable (Project c) where bifoldMap = bifoldMapDefault

-- | 'traverse' over all three type arguments of 'Project'.
triverseProject
    :: Applicative f
    => (uri -> f uri')
    -> (opt -> f opt')
    -> (pkg -> f pkg')
    -> Project uri opt pkg -> f (Project uri' opt' pkg')
triverseProject f g h prj =
    (\c b a -> prj { prjPackages = a, prjOptPackages = b, prjUriPackages = c })
        <$> traverse f (prjUriPackages prj)
        <*> traverse g (prjOptPackages prj)
        <*> traverse h (prjPackages prj)

instance Bitraversable (Project uri) where
    bitraverse = triverseProject pure

-- | Empty project.
emptyProject :: Project c b a
emptyProject = Project [] [] [] [] [] False Nothing OptimizationOn [] []

-------------------------------------------------------------------------------
-- Initial  parsing
-------------------------------------------------------------------------------

-- | Parse project file. Extracts only few fields.
--
-- >>> fmap prjPackages $ parseProject "cabal.project" "packages: foo bar/*.cabal"
-- Right ["foo","bar/*.cabal"]
--
parseProject :: FilePath -> ByteString -> Either ParseError (Project Void String String)
parseProject = parseWith $ \fields0 -> do
    let (fields1, sections) = C.partitionFields fields0
    let fields2  = M.filterWithKey (\k _ -> k `elem` knownFields) fields1
    parse fields0 fields2 sections
  where
    knownFields = C.fieldGrammarKnownFieldList $ grammar []

    parse origFields fields sections = do
        let prettyOrigFields = map void $ C.fromParsecFields $ filter notPackages origFields
        prj <- C.parseFieldGrammar C.cabalSpecLatest fields $ grammar prettyOrigFields
        foldr ($) prj <$> traverse parseSec (concat sections)

    parseSec :: C.Section C.Position -> C.ParseResult (Project Void String String -> Project Void String String)
    parseSec (C.MkSection (C.Name _pos name) [] fields) | name == "source-repository-package" = do
        let fields' = fst $ C.partitionFields fields
        repo <- C.parseFieldGrammar C.cabalSpecLatest fields' (C.sourceRepoFieldGrammar $ C.RepoKindUnknown "unused")
        return $ over prjSourceReposL (repo :)

    parseSec _ = return id

notPackages :: C.Field ann -> Bool
notPackages (C.Field (C.Name _ "packages") _) = False
notPackages _                                 = True

grammar :: [C.PrettyField ()] -> C.ParsecFieldGrammar (Project Void String String) (Project Void String String)
grammar origFields = Project
    <$> C.monoidalFieldAla "packages"          (C.alaList' C.FSep PackageLocation) prjPackagesL
    <*> C.monoidalFieldAla "optional-packages" (C.alaList' C.FSep PackageLocation) prjOptPackagesL
    <*> pure []
    <*> C.monoidalFieldAla "constraints"       (C.alaList' C.CommaVCat NoCommas)   prjConstraintsL
    <*> C.monoidalFieldAla "allow-newer"       (C.alaList' C.CommaVCat NoCommas)   prjAllowNewerL
    <*> C.booleanFieldDef  "reorder-goals"                                         prjReorderGoalsL False
    <*> C.optionalFieldAla "max-backjumps"     Int'                                prjMaxBackjumpsL
    <*> C.optionalFieldDef "optimization"                                          prjOptimizationL OptimizationOn
    <*> pure []
    <*> pure origFields

-------------------------------------------------------------------------------
-- Lenses
-------------------------------------------------------------------------------

prjPackagesL :: Functor f => LensLike' f (Project uri opt pkg) [pkg]
prjPackagesL f prj = f (prjPackages prj) <&> \x -> prj { prjPackages = x }

prjOptPackagesL :: Functor f => LensLike' f (Project uri opt pkg) [opt]
prjOptPackagesL f prj = f (prjOptPackages prj) <&> \x -> prj { prjOptPackages = x }

prjConstraintsL :: Functor f => LensLike' f (Project uri opt pkg) [String]
prjConstraintsL f prj = f (prjConstraints prj) <&> \x -> prj { prjConstraints = x }

prjAllowNewerL :: Functor f => LensLike' f (Project uri opt pkg) [String]
prjAllowNewerL f prj = f (prjAllowNewer prj) <&> \x -> prj { prjAllowNewer = x }

prjReorderGoalsL :: Functor f => LensLike' f (Project uri opt pkg) Bool
prjReorderGoalsL f prj = f (prjReorderGoals prj) <&> \x -> prj { prjReorderGoals = x }

prjMaxBackjumpsL :: Functor f => LensLike' f (Project uri opt pkg) (Maybe Int)
prjMaxBackjumpsL f prj = f (prjMaxBackjumps prj) <&> \x -> prj { prjMaxBackjumps = x }

prjOptimizationL :: Functor f => LensLike' f (Project uri opt pkg) Optimization
prjOptimizationL f prj = f (prjOptimization prj) <&> \x -> prj { prjOptimization = x }

prjSourceReposL :: Functor f => LensLike' f (Project uri opt pkg) [C.SourceRepo]
prjSourceReposL f prj = f (prjSourceRepos prj) <&> \x -> prj { prjSourceRepos = x }

-------------------------------------------------------------------------------
-- Resolving
-------------------------------------------------------------------------------

-- | A 'resolveProject' error.
newtype ResolveError = BadPackageLocation String
  deriving Show

instance Exception ResolveError where
    displayException = renderResolveError

-- | Pretty print 'ResolveError'.
renderResolveError :: ResolveError -> String
renderResolveError (BadPackageLocation s) = "Bad package location: " ++ show s

-- | Resolve project package locations.
--
-- Separate 'URI' packages, glob @packages@ and @optional-packages@
-- into individual fields.
--
-- The result 'prjPackages' 'FilePath's will be relative to the
-- directory of the project file.
--
resolveProject
    :: FilePath                                        -- ^ filename of project file
    -> Project Void String String                      -- ^ parsed project file
    -> IO (Either ResolveError (Project URI Void FilePath))  -- ^ resolved project
resolveProject filePath prj = runExceptT $ do
    prj' <- bitraverse findOptProjectPackage findProjectPackage prj
    let (uris, pkgs) = partitionEithers $ concat $ prjPackages prj'
    return prj'
        { prjPackages    = pkgs ++ concat (prjOptPackages prj')
        , prjOptPackages = []
        , prjUriPackages = uris
        }
  where
    rootdir = takeDirectory filePath

    findProjectPackage :: String -> ExceptT ResolveError IO [Either URI FilePath]
    findProjectPackage pkglocstr = do
        mfp <- fmap3 Right (checkisFileGlobPackage pkglocstr) `mplusMaybeT`
               fmap3 Right (checkIsSingleFilePackage pkglocstr) `mplusMaybeT`
               fmap2 (\uri -> [Left uri]) (return $ parseURI pkglocstr)
        maybe (throwE $ BadPackageLocation pkglocstr) return mfp

    fmap2 f = fmap (fmap f)
    fmap3 f = fmap (fmap (fmap f))

    findOptProjectPackage pkglocstr = do
        mfp <- checkisFileGlobPackage pkglocstr `mplusMaybeT`
               checkIsSingleFilePackage pkglocstr
        maybe (return []) return mfp

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

-------------------------------------------------------------------------------
-- Read package files
-------------------------------------------------------------------------------

readPackagesOfProject :: Project uri opt FilePath -> IO (Either ParseError (Project uri opt (FilePath, C.GenericPackageDescription)))
readPackagesOfProject prj = runExceptT $ for prj $ \fp -> do
    contents <- liftIO $ BS.readFile fp
    case C.runParseResult $ C.parseGenericPackageDescription contents of
        (ws, Left (_mv, errs)) -> throwE $ ParseError fp contents errs ws
        (_, Right gpd)         -> return (fp, gpd)
