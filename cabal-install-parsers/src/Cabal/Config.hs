{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}
module Cabal.Config (
    -- * Types
    Config (..),
    Repo (..),
    RepoName,
    -- * Parsing
    readConfig,
    findConfig,
    parseConfig,
    resolveConfig,
    -- * Hackage
    cfgRepoIndex,
    hackageHaskellOrg,
    ) where

import Control.DeepSeq          (NFData (..))
import Control.Exception        (throwIO)
import Data.ByteString          (ByteString)
import Data.Function            ((&))
import Data.Functor.Identity    (Identity (..))
import Data.List                (foldl')
import Data.Map                 (Map)
import Data.Maybe               (fromMaybe)
import Distribution.Compat.Lens (LensLike', over)
import GHC.Generics             (Generic)
import Network.URI              (URI)
import System.Directory         (getAppUserDataDirectory)
import System.Environment       (lookupEnv)
import System.FilePath          ((</>))

import qualified Data.ByteString               as BS
import qualified Data.Map.Strict               as M
import qualified Distribution.CabalSpecVersion as C
import qualified Distribution.FieldGrammar     as C
import qualified Distribution.Fields           as C
import qualified Distribution.Parsec           as C
import qualified Distribution.Parsec.Newtypes  as C
import qualified Distribution.Simple.Utils     as C

import Cabal.Internal.Newtypes
import Cabal.Parse

infixl 1 <&>
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

-------------------------------------------------------------------------------
-- Read config
-------------------------------------------------------------------------------

-- | High level convinience function to find and read @~/.cabal/config@ file
--
-- May throw 'IOException' when file doesn't exist, and 'ParseError'
-- on parse error.
--
readConfig :: IO (Config Identity)
readConfig = do
    fp <- findConfig
    bs <- BS.readFile fp
    either throwIO resolveConfig (parseConfig fp bs)

-------------------------------------------------------------------------------
-- Find config
-------------------------------------------------------------------------------

-- | Find the @~\/.cabal\/config@ file.
findConfig :: IO FilePath
findConfig = do
    env <- lookupEnv "CABAL_CONFIG"
    case env of
        Just p -> return p
        Nothing -> do
            c <- getAppUserDataDirectory "cabal"
            return (c </> "config")

-------------------------------------------------------------------------------
-- Config
-------------------------------------------------------------------------------

-- | Very minimal representation of @~\/.cabal\/config@ file.
data Config f = Config
    { cfgRepositories    :: Map RepoName Repo
    , cfgRemoteRepoCache :: f FilePath
    , cfgInstallDir      :: f FilePath
    , cfgStoreDir        :: f FilePath
    }
  deriving (Generic)

deriving instance Show (f FilePath) => Show (Config f)

-- | @since 0.2.1
instance NFData (f FilePath) => NFData (Config f)

-- | Repository.
--
-- missing @root-keys@, @key-threshold@ which we don't need now.
--
data Repo = Repo
    { repoURL    :: URI
    , repoSecure :: Bool -- ^ @since 0.2
    }
  deriving (Show, Generic)

-- | Repository name, bare 'String'.
type RepoName = String

-- | @since 0.2.1
instance NFData Repo

-------------------------------------------------------------------------------
-- Finding index
-------------------------------------------------------------------------------

-- | Find a @01-index.tar@ for particular repository
cfgRepoIndex
    :: Config Identity
    -> RepoName
    -> Maybe FilePath
cfgRepoIndex cfg repo
    | repo `M.member` cfgRepositories cfg =
        Just (runIdentity (cfgRemoteRepoCache cfg) </> repo </> "01-index.tar")
    | otherwise = Nothing

-- | The default repository of haskell packages, <https://hackage.haskell.org/>.
hackageHaskellOrg :: RepoName
hackageHaskellOrg = "hackage.haskell.org"

-------------------------------------------------------------------------------
-- Parsing
-------------------------------------------------------------------------------

-- | Parse @~\/.cabal\/config@ file.
parseConfig :: FilePath -> ByteString -> Either ParseError (Config Maybe)
parseConfig = parseWith $ \fields0 -> do
    let (fields1, sections) = C.partitionFields fields0
    let fields2 = M.filterWithKey (\k _ -> k `elem` knownFields) fields1
    parse fields2 sections
  where
    knownFields = C.fieldGrammarKnownFieldList grammar

    parse fields sections = do
        cfg <- C.parseFieldGrammar C.cabalSpecLatest fields grammar
        foldl' (&) cfg <$> traverse parseSec (concat sections)

    parseSec :: C.Section C.Position -> C.ParseResult (Config f -> Config f)
    parseSec (C.MkSection (C.Name _pos name) [C.SecArgName _pos' secName] fields) | name == "repository" = do
        let repoName = C.fromUTF8BS secName
        let fields' = fst $ C.partitionFields fields
        repo <- C.parseFieldGrammar C.cabalSpecLatest fields' repoGrammar
        return $ over cfgRepositoriesL $ M.insert repoName repo

    parseSec _ = return id

grammar :: C.ParsecFieldGrammar (Config Maybe) (Config Maybe)
grammar = Config mempty
    <$> C.optionalFieldAla "remote-repo-cache" C.FilePathNT cfgRemoteRepoCacheL
    <*> C.optionalFieldAla "installdir"        C.FilePathNT cfgInstallDirL
    <*> C.optionalFieldAla "store-dir"         C.FilePathNT cfgStoreDirL

repoGrammar :: C.ParsecFieldGrammar Repo Repo
repoGrammar = Repo
    <$> C.uniqueFieldAla  "url"    WrapURI repoURLL
    <*> C.booleanFieldDef "secure"         repoSecureL False

-------------------------------------------------------------------------------
-- Resolving
-------------------------------------------------------------------------------

-- | Fill the default in @~\/.cabal\/config@  file.
resolveConfig :: Config Maybe -> IO (Config Identity)
resolveConfig cfg = do
    c <- getAppUserDataDirectory "cabal"
    return cfg
        { cfgRemoteRepoCache = Identity $ fromMaybe (c </> "packages") (cfgRemoteRepoCache cfg)
        , cfgInstallDir      = Identity $ fromMaybe (c </> "bin")      (cfgInstallDir cfg)
        , cfgStoreDir        = Identity $ fromMaybe (c </> "store")    (cfgStoreDir cfg)
        }

-------------------------------------------------------------------------------
-- Lenses
-------------------------------------------------------------------------------

cfgRepositoriesL :: Functor f => LensLike' f (Config g) (Map String Repo)
cfgRepositoriesL f cfg = f (cfgRepositories cfg) <&> \x -> cfg { cfgRepositories = x }

cfgRemoteRepoCacheL :: Functor f => LensLike' f (Config g) (g FilePath)
cfgRemoteRepoCacheL f cfg = f (cfgRemoteRepoCache cfg) <&> \x -> cfg { cfgRemoteRepoCache = x }

cfgInstallDirL :: Functor f => LensLike' f (Config g) (g FilePath)
cfgInstallDirL f cfg = f (cfgInstallDir cfg) <&> \x -> cfg { cfgInstallDir = x }

cfgStoreDirL :: Functor f => LensLike' f (Config g) (g FilePath)
cfgStoreDirL f cfg = f (cfgStoreDir cfg) <&> \x -> cfg { cfgStoreDir = x }

repoURLL :: Functor f => LensLike' f Repo URI
repoURLL f s = f (repoURL s) <&> \x -> s { repoURL = x }

repoSecureL :: Functor f => LensLike' f Repo Bool
repoSecureL f s = f (repoSecure s) <&> \x -> s { repoSecure = x }
