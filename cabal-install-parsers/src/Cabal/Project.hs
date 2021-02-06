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
    readProject,
    parseProject,
    -- * Resolve project
    resolveProject,
    ResolveError (..),
    renderResolveError,
    -- * Read packages
    readPackagesOfProject
    ) where

import Control.DeepSeq              (NFData (..))
import Control.Exception            (Exception (..), throwIO)
import Control.Monad.IO.Class       (liftIO)
import Control.Monad.Trans.Except   (ExceptT, runExceptT, throwE)
import Data.Bifoldable              (Bifoldable (..))
import Data.Bifunctor               (Bifunctor (..))
import Data.Bitraversable           (Bitraversable (..), bifoldMapDefault, bimapDefault)
import Data.ByteString              (ByteString)
import Data.Either                  (partitionEithers)
import Data.Foldable                (toList)
import Data.Function                ((&))
import Data.Functor                 (void)
import Data.List                    (foldl')
import Data.List.NonEmpty           (NonEmpty)
import Data.Traversable             (for)
import Data.Void                    (Void)
import Distribution.Compat.Lens     (LensLike', over)
import GHC.Generics                 (Generic)
import Network.URI                  (URI, parseURI)
import System.Directory             (doesDirectoryExist, doesFileExist)
import System.FilePath              (takeDirectory, takeExtension, (</>))
import Text.ParserCombinators.ReadP (readP_to_S)

import qualified Data.ByteString                 as BS
import qualified Data.Map.Strict                 as M
import qualified Distribution.CabalSpecVersion   as C
import qualified Distribution.FieldGrammar       as C
import qualified Distribution.Fields             as C
import qualified Distribution.PackageDescription as C
import qualified Distribution.Parsec             as C

import Cabal.Internal.Glob
import Cabal.Internal.Newtypes
import Cabal.Optimization
import Cabal.Package
import Cabal.Parse
import Cabal.SourceRepo

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
    , prjSourceRepos  :: [SourceRepositoryPackage Maybe]
    , prjOtherFields  :: [C.PrettyField ()] -- ^ other fields
    }
  deriving (Functor, Foldable, Traversable, Generic)

-- | Doesn't compare prjOtherFields
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

-- | @since 0.2.1
instance (NFData c, NFData b, NFData a) => NFData (Project c b a) where
    rnf (Project x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) =
        rnf x1 `seq` rnf x2 `seq` rnf x3 `seq`
        rnf x4 `seq` rnf x5 `seq` rnf x6 `seq`
        rnf x7 `seq` rnf x8 `seq` rnf x9 `seq`
        rnfList rnfPrettyField x10
      where
        rnfList :: (a -> ()) -> [a] -> ()
        rnfList _ []     = ()
        rnfList f (x:xs) = f x `seq` rnfList f xs

        rnfPrettyField :: NFData x => C.PrettyField x -> ()
        rnfPrettyField (C.PrettyField ann fn d) =
            rnf ann `seq` rnf fn `seq` rnf d
        rnfPrettyField (C.PrettySection ann fn ds fs) =
            rnf ann `seq` rnf fn `seq` rnf ds `seq` rnfList rnfPrettyField fs

-------------------------------------------------------------------------------
-- Initial  parsing
-------------------------------------------------------------------------------

-- | High level conviniene function to read and elaborate @cabal.project@ files
--
-- May throw 'IOException' when file doesn't exist, 'ParseError'
-- on parse errors, or 'ResolveError' on package resolution error.
--
readProject :: FilePath -> IO (Project URI Void (FilePath, C.GenericPackageDescription))
readProject fp = do
    contents <- BS.readFile fp
    prj0 <- either throwIO return (parseProject fp contents)
    prj1 <- resolveProject fp prj0 >>= either throwIO return
    readPackagesOfProject prj1 >>= either throwIO return

-- | Parse project file. Extracts only few fields.
--
-- >>> fmap prjPackages $ parseProject "cabal.project" "packages: foo bar/*.cabal"
-- Right ["foo","bar/*.cabal"]
--
parseProject :: FilePath -> ByteString -> Either (ParseError NonEmpty) (Project Void String String)
parseProject = parseWith $ \fields0 -> do
    let (fields1, sections) = C.partitionFields fields0
    let fields2  = M.filterWithKey (\k _ -> k `elem` knownFields) fields1
    parse fields0 fields2 sections
  where
    knownFields = C.fieldGrammarKnownFieldList $ grammar []

    parse otherFields fields sections = do
        let prettyOtherFields = map void $ C.fromParsecFields $ filter otherFieldName otherFields
        prj <- C.parseFieldGrammar C.cabalSpecLatest fields $ grammar prettyOtherFields
        foldl' (&) prj <$> traverse parseSec (concat sections)

    -- Special case for source-repository-package. If you add another such
    -- special case, make sure to update otherFieldName appropriately.
    parseSec :: C.Section C.Position -> C.ParseResult (Project Void String String -> Project Void String String)
    parseSec (C.MkSection (C.Name _pos name) [] fields) | name == sourceRepoSectionName = do
        let fields' = fst $ C.partitionFields fields
        repos <- C.parseFieldGrammar C.cabalSpecLatest fields' sourceRepositoryPackageGrammar
        return $ over prjSourceReposL (++ toList (srpFanOut repos))

    parseSec _ = return id

-- | Returns 'True' if a field should be a part of 'prjOtherFields'. This
-- excludes any field that is a part of 'grammar' as well as
-- @source-repository-package@ (see 'parseProject', which has a special case
-- for it).
otherFieldName :: C.Field ann -> Bool
otherFieldName (C.Field (C.Name _ fn) _)     = fn `notElem` C.fieldGrammarKnownFieldList (grammar [])
otherFieldName (C.Section (C.Name _ fn) _ _) = fn /= sourceRepoSectionName

-- | This contains a subset of the fields in the @cabal.project@ grammar that
-- are distinguished by a 'Project'. Note that this does not /not/ contain
-- @source-repository-package@, as that is handled separately in 'parseProject'.
grammar :: [C.PrettyField ()] -> C.ParsecFieldGrammar (Project Void String String) (Project Void String String)
grammar otherFields = Project
    <$> C.monoidalFieldAla "packages"          (C.alaList' C.FSep PackageLocation) prjPackagesL
    <*> C.monoidalFieldAla "optional-packages" (C.alaList' C.FSep PackageLocation) prjOptPackagesL
    <*> pure []
    <*> C.monoidalFieldAla "constraints"       (C.alaList' C.CommaVCat NoCommas)   prjConstraintsL
    <*> C.monoidalFieldAla "allow-newer"       (C.alaList' C.CommaVCat NoCommas)   prjAllowNewerL
    <*> C.booleanFieldDef  "reorder-goals"                                         prjReorderGoalsL False
    <*> C.optionalFieldAla "max-backjumps"     Int'                                prjMaxBackjumpsL
    <*> C.optionalFieldDef "optimization"                                          prjOptimizationL OptimizationOn
    <*> pure []
    <*> pure otherFields

sourceRepoSectionName :: C.FieldName
sourceRepoSectionName = "source-repository-package"

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

prjSourceReposL :: Functor f => LensLike' f (Project uri opt pkg) [SourceRepositoryPackage Maybe]
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

-- | Read and parse the cabal files of packages in the 'Project'.
--
-- May throw 'IOException'.
--
readPackagesOfProject :: Project uri opt FilePath -> IO (Either (ParseError NonEmpty) (Project uri opt (FilePath, C.GenericPackageDescription)))
readPackagesOfProject prj = runExceptT $ for prj $ \fp -> do
    contents <- liftIO $ BS.readFile fp
    either throwE (\gpd -> return (fp, gpd)) (parsePackage fp contents)
