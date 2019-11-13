{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Cabal.Index (
    -- * Metadata construction
    indexMetadata,
    -- ** Exceptions thrown
    MetadataParseError (..),
    InvalidHash (..),
    InvalidIndexFile (..),
    -- * Metadata types
    PackageInfo (..),
    piPreferredVersions,
    ReleaseInfo (..),
    -- ** Hashes
    SHA256,
    sha256,
    validSHA256,
    mkSHA256,
    unsafeMkSHA256,
    getSHA256,

    {-
    MD5,
    validMD5,
    mkMD5,
    unsafeMkMD5,
    getMD5,
    -}
    -- * Generic folding
    foldIndex,
    IndexFile (..),
    ) where

import Prelude hiding (pi)

import Control.Exception (Exception, evaluate, throwIO)
import Control.Monad     (unless, void)
import Data.ByteString   (ByteString)
import Data.Map.Strict   (Map)
import Data.Text         (Text)
import GHC.Generics      (Generic)

import qualified Codec.Archive.Tar                   as Tar
import qualified Crypto.Hash.SHA256                  as SHA256
import qualified Data.Aeson                          as A
import qualified Data.ByteString                     as BS
import qualified Data.ByteString.Base16              as Base16
import qualified Data.ByteString.Lazy                as LBS
import qualified Data.Map.Strict                     as Map
import qualified Data.Text.Encoding                  as TE
import qualified Distribution.Compat.CharParsing     as C
import qualified Distribution.Package                as C
import qualified Distribution.Parsec                 as C
import qualified Distribution.Parsec.FieldLineStream as C
import qualified Distribution.Pretty                 as C
import qualified Distribution.Version                as C
import qualified System.FilePath                     as FP

-------------------------------------------------------------------------------
-- Generic folding
-------------------------------------------------------------------------------

-- | Fold over Hackage @01-index.tar@ file.
--
-- May throw 'Tar.FormatError' or 'InvalidIndexFile'.
foldIndex
    :: FilePath -- ^ path to the @01-index.tar@ file
    -> a        -- ^ initial value
    -> (FilePath -> IndexFile -> ByteString -> a -> IO a)
    -> IO a
foldIndex fp ini action = do
    contents <- LBS.readFile fp
    foldEntries go throwIO ini (Tar.read contents)
  where
    go !acc entry = case Tar.entryContent entry of
        Tar.NormalFile contents _ -> do
            bs <- evaluate $ LBS.toStrict contents
            idxFile <- either (throwIO . InvalidIndexFile) return (elaborateIndexFile fpath)
            action fpath idxFile bs acc
        Tar.Directory -> return acc
        _             -> return acc
     where
       fpath = Tar.entryPath entry

foldEntries :: (a -> Tar.Entry -> IO a) -> (e -> IO a) -> a -> Tar.Entries e -> IO a
foldEntries next fail' = go where
    go !acc (Tar.Next e es) = next acc e >>= \acc' -> go acc' es
    go  _   (Tar.Fail e)    = fail' e
    go  acc Tar.Done        = return acc

-------------------------------------------------------------------------------
-- IndexFile
-------------------------------------------------------------------------------

-- | Varions files in @01-index.tar@.
data IndexFile
    = CabalFile C.PackageName C.Version
    | PackageJson C.PackageName C.Version
    | PreferredVersions C.PackageName
  deriving (Show)

-- | Thrown when when not a @.cabal@, @package.json@ or @preferred-versions@
-- file is encountered.
newtype InvalidIndexFile = InvalidIndexFile String
  deriving (Show)

instance Exception InvalidIndexFile

elaborateIndexFile :: FilePath -> Either String IndexFile
elaborateIndexFile fp = case FP.splitDirectories fp of
    [ pn, v, pnF ]
        | Just pn' <- C.simpleParsec pn
        , Just v'  <- C.simpleParsec v
        , pnF == pn ++ ".cabal"
        -> Right (CabalFile pn' v')
    [ pn, v, pj ]
        | Just pn' <- C.simpleParsec pn
        , Just v'  <- C.simpleParsec v
        , pj == "package.json"
        -> Right (PackageJson pn' v')
    [ pn, pref ]
        | Just pn' <- C.simpleParsec pn
        , pref == "preferred-versions"
        -> Right (PreferredVersions pn')
    xs -> Left $ show xs

-------------------------------------------------------------------------------
-- Hashes
-------------------------------------------------------------------------------

-- | SHA256 result.
newtype SHA256 = SHA256 ByteString
  deriving (Eq, Ord)

-- | Hash strict 'ByteString'.
sha256 :: ByteString -> SHA256
sha256 = SHA256 . SHA256.hash

-- | Make SHA256 from base16-encoded string.
mkSHA256 :: Text -> Either String SHA256
mkSHA256 t = case Base16.decode (TE.encodeUtf8 t) of
    (bs, rest) | not (BS.null rest)  -> Left $ "Base16 encoding leftovers" ++ show rest
               | BS.length bs /= 32  -> Left $ "Base16 of wrong length, expected 32, got " ++ show (BS.length bs)
               | otherwise           -> Right (SHA256 bs)

-- | Unsafe variant of 'mkSHA256'.
unsafeMkSHA256 :: Text -> SHA256
unsafeMkSHA256 = either error id . mkSHA256

emptySHA256 :: SHA256
emptySHA256 = SHA256 BS.empty

-- | Check invariants of 'SHA256'
validSHA256 :: SHA256 -> Bool
validSHA256 (SHA256 bs) = BS.length bs == 32

-- | Get underlying 'ByteString' of 'SHA256'.
getSHA256 :: SHA256 -> ByteString
getSHA256 (SHA256 bs) = bs

instance Show SHA256 where
    showsPrec d (SHA256 bs)
        = showParen (d > 10)
        $ showString "unsafeMkSHA256 "
        . shows (Base16.encode bs)


newtype MD5 = MD5 ByteString
  deriving (Eq, Ord)

instance Show MD5 where
    showsPrec d (MD5 bs)
        = showParen (d > 10)
        $ showString "unsafeMkMD5 "
        . shows (Base16.encode bs)

-- | Make MD5 from base16-encoded string.
mkMD5 :: Text -> Either String MD5
mkMD5 t = case Base16.decode (TE.encodeUtf8 t) of
    (bs, rest) | not (BS.null rest)  -> Left $ "Base16 encoding leftovers" ++ show rest
               | BS.length bs /= 16  -> Left $ "Base16 of wrong length, expected 16, got " ++ show (BS.length bs)
               | otherwise           -> Right (MD5 bs)

{-
-- | Unsafe variant of 'mkMD5'.
unsafeMkMD5 :: Text -> MD5
unsafeMkMD5 = either error id . mkMD5

-- | Check invariants of 'MD5'
validMD5 :: MD5 -> Bool
validMD5 (MD5 bs) = BS.length bs == 16

-- | Get underlying 'ByteString' of 'MD5'.
getMD5 :: MD5 -> ByteString
getMD5 (MD5 bs) = bs
-}

-------------------------------------------------------------------------------
-- Metadata types
-------------------------------------------------------------------------------

-- | Package information.
data PackageInfo = PackageInfo
    { piVersions  :: Map C.Version ReleaseInfo  -- ^ individual package releases
    , piPreferred :: C.VersionRange             -- ^ preferred versions range
    }
  deriving (Show, Generic)

-- | Like 'piVersions', but return only 'piPreferred' versions.
piPreferredVersions :: PackageInfo -> Map C.Version ReleaseInfo
piPreferredVersions pi =
    Map.filterWithKey (\v _ -> v `C.withinRange` piPreferred pi) (piVersions pi)


-- | Package's release information.
data ReleaseInfo = ReleaseInfo
    { riRevision :: Word    -- ^ revision number
    , riCabal    :: SHA256  -- ^ hash of the last revision of @.cabal@ file
    , riTarball  :: SHA256  -- ^ hash of the @.tar.gz@ file.
    }
  deriving (Eq, Show, Generic)

-------------------------------------------------------------------------------
-- Metadata construction
-------------------------------------------------------------------------------

-- | Read index file and return the metadata about packages.
--
-- It takes about 6 seconds on my machine.
--
indexMetadata :: FilePath -> IO (Map C.PackageName PackageInfo)
indexMetadata indexFilepath = do
    result <- foldIndex indexFilepath Map.empty $ \p indexFile contents m -> case indexFile of
        CabalFile pn ver -> return $ Map.alter f pn m where
            f :: Maybe PackageInfo -> Maybe PackageInfo
            f Nothing = Just PackageInfo
                { piVersions  = Map.singleton ver (ReleaseInfo 0 (sha256 contents) emptySHA256)
                , piPreferred = C.anyVersion
                }
            f (Just pi) = Just pi { piVersions = Map.alter g ver (piVersions pi) }

            g :: Maybe ReleaseInfo -> Maybe ReleaseInfo
            g Nothing                           = Just $ ReleaseInfo 0        (sha256 contents) emptySHA256
            g (Just (ReleaseInfo r c t))
                | r == 0 && not (validSHA256 c) = Just $ ReleaseInfo 0        (sha256 contents) t
                | otherwise                     = Just $ ReleaseInfo (succ r) (sha256 contents) t

        PackageJson pn ver -> case A.eitherDecodeStrict contents of
                Left err -> throwIO $ MetadataParseError p err
                Right (PJ (Signed (Targets ts))) ->
                    case Map.lookup ("<repo>/package/" ++ C.prettyShow pn ++ "-" ++ C.prettyShow ver ++ ".tar.gz") ts of
                        Just t  -> return $ Map.alter (f t) pn m
                        Nothing -> throwIO $ MetadataParseError p $ "Invalid targets in " ++ p ++ " -- " ++ show ts
                  where
                    f :: Target -> Maybe PackageInfo -> Maybe PackageInfo
                    f t Nothing   = Just PackageInfo
                        { piVersions  = Map.singleton ver (ReleaseInfo 0 emptySHA256 (hashSHA256 (targetHashes t)))
                        , piPreferred = C.anyVersion
                        }
                    f t (Just pi) = Just pi { piVersions = Map.alter (g t) ver (piVersions pi) }

                    g :: Target -> Maybe ReleaseInfo -> Maybe ReleaseInfo
                    g t Nothing                    = Just $ ReleaseInfo 0 emptySHA256 (hashSHA256 (targetHashes t))
                    g t (Just (ReleaseInfo r c _)) = Just $ ReleaseInfo r c (hashSHA256 (targetHashes t))

        PreferredVersions pn
                | BS.null contents -> return m
                | otherwise        -> case explicitEitherParsecBS preferredP contents of
                    Right vr -> return $ Map.alter (f vr) pn m
                    Left err -> throwIO $ MetadataParseError p err
              where
                preferredP = do
                    _ <- C.string (C.prettyShow pn)
                    C.spaces
                    C.parsec

                f :: C.VersionRange -> Maybe PackageInfo -> Maybe PackageInfo
                f vr Nothing = Just PackageInfo
                    { piVersions  = Map.empty
                    , piPreferred = vr
                    }
                f vr (Just pi) = Just pi { piPreferred = vr }

    -- check invariants and return
    postCheck result
    return result


postCheck :: Map C.PackageName PackageInfo -> IO ()
postCheck meta = ifor_ meta $ \pn pi -> ifor_ (piVersions pi) $ \ver ri -> do
    unless (validSHA256 (riCabal ri))   $ throwIO $ InvalidHash pn ver "cabal"
    unless (validSHA256 (riTarball ri)) $ throwIO $ InvalidHash pn ver "tarball"
  where
    ifor_ :: Map k v -> (k -> v -> IO a) -> IO ()
    ifor_ xs f = Map.foldlWithKey' (\m k a -> m >> void (f k a)) (return ()) xs

-- | Thrown when we cannot parse @package.json@ or @preferred-versions@ files.
data MetadataParseError = MetadataParseError FilePath String
  deriving (Show)

instance Exception MetadataParseError

-- | Thrown if we fail consistency check, we don't know a hash for some file.
data InvalidHash = InvalidHash C.PackageName C.Version String
  deriving (Show)

instance Exception InvalidHash

-------------------------------------------------------------------------------
-- Cabal utils
-------------------------------------------------------------------------------

explicitEitherParsecBS :: C.ParsecParser a -> ByteString -> Either String a
explicitEitherParsecBS parser
    = either (Left . show) Right
    . C.runParsecParser (parser <* C.spaces) "<eitherParsec>"
    . C.fieldLineStreamFromBS

-------------------------------------------------------------------------------
-- package.json
-------------------------------------------------------------------------------

-- |
--
-- @
-- {
--   "signatures": [],
--   "signed": {
--     "_type": "Targets",
--     "expires": null,
--     "targets": {
--       "<repo>/package/gruff-0.2.1.tar.gz": {
--         "hashes": {
--           "md5":"f551ecaf18e8ec807a9f0f5b69c7ed5a",
--           "sha256":"727408b14173594bbe88dad4240cb884063a784b74afaeaad5fb56c9f042afbd"
--         },
--         "length": 75691
--       }
--     },
--     "version":0
--   }
-- }
-- @
newtype PJ = PJ (Signed Targets)
  deriving Show

newtype Signed a = Signed a
  deriving Show

newtype Targets = Targets (Map FilePath Target)
  deriving Show

data Target = Target
    { _targetLength :: Word
    , targetHashes :: Hashes
    }
  deriving Show

data Hashes = Hashes
    { _hashMD5    :: MD5
    , hashSHA256 :: SHA256
    }
  deriving Show

instance A.FromJSON PJ where
    parseJSON = A.withObject "package.json" $ \obj ->
        PJ <$> obj A..: "signed"

instance A.FromJSON a => A.FromJSON (Signed a) where
    parseJSON = A.withObject "signed (targets)" $ \obj -> do
        A.String "Targets" <- obj A..: "_type"
        A.Null             <- obj A..: "expires"
        Signed <$> obj A..: "targets"

instance A.FromJSON Targets where
    parseJSON = fmap Targets . A.parseJSON

instance A.FromJSON Target where
    parseJSON = A.withObject "Target" $ \obj -> Target
        <$> obj A..: "length"
        <*> obj A..: "hashes"

instance A.FromJSON Hashes where
    parseJSON = A.withObject "Hashes" $ \obj -> Hashes
        <$> (obj A..: "md5"    >>= either fail return . mkMD5)
        <*> (obj A..: "sha256" >>= either fail return . mkSHA256)
