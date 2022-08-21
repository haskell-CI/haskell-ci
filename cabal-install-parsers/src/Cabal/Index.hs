{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Cabal.Index (
    -- * Metadata construction
    indexMetadata,
    cachedHackageMetadata,
    -- ** Exceptions thrown
    MetadataParseError (..),
    InvalidHash (..),
    InvalidIndexFile (..),
    NoHackageRepository (..),
    -- * Metadata types
    PackageInfo (..),
    piPreferredVersions,
    ReleaseInfo (..),
    -- ** Hashes
    SHA256 (..),
    sha256,
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
    IndexEntry (..),
    IndexFileType (..),
    ) where

import Prelude hiding (pi)

import Control.Exception (Exception, IOException, bracket, evaluate, handle, throwIO)
import Data.Bits         (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString   (ByteString)
import Data.Int          (Int64)
import Data.Map.Strict   (Map)
import Data.Text         (Text)
import Data.Word         (Word32, Word64)
import GHC.Generics      (Generic)

import qualified Codec.Archive.Tar                   as Tar
import qualified Codec.Archive.Tar.Entry             as Tar
import qualified Codec.Archive.Tar.Index             as Tar
import qualified Crypto.Hash.SHA256                  as SHA256
import qualified Data.Aeson                          as A
import qualified Data.Binary                         as Binary
import qualified Data.Binary.Get                     as Binary.Get
import qualified Data.Binary.Put                     as Binary.Put
import qualified Data.ByteString                     as BS
import qualified Data.ByteString.Base16              as Base16
import qualified Data.ByteString.Lazy                as LBS
import qualified Data.ByteString.Unsafe              as BS.Unsafe
import qualified Data.Map.Strict                     as Map
import qualified Data.Text.Encoding                  as TE
import qualified Data.Time.Clock.POSIX               as Time
import qualified Distribution.Compat.CharParsing     as C
import qualified Distribution.Package                as C
import qualified Distribution.Parsec                 as C
import qualified Distribution.Parsec.FieldLineStream as C
import qualified Distribution.Pretty                 as C
import qualified Distribution.Utils.Generic          as C
import qualified Distribution.Version                as C
import qualified Lukko
import qualified System.Directory                    as D
import qualified System.FilePath                     as FP
import qualified Text.PrettyPrint                    as PP

import Data.Binary.Instances ()

import Cabal.Config (cfgRepoIndex, hackageHaskellOrg, readConfig)

-------------------------------------------------------------------------------
-- Generic folding
-------------------------------------------------------------------------------

-- | Fold over Hackage @01-index.tar@ file.
--
-- May throw 'Tar.FormatError' or 'InvalidIndexFile'.
foldIndex
    :: FilePath -- ^ path to the @01-index.tar@ file
    -> a        -- ^ initial value
    -> (IndexEntry -> ByteString -> a -> IO a)
    -> IO a
foldIndex fp ini action = do
    contents <- LBS.readFile fp
    Acc _ result <- foldEntries go throwIO (Acc 0 ini) (Tar.read contents)
    return result
  where
    go (Acc offset acc) entry = case Tar.entryContent entry of
        -- file entry
        Tar.NormalFile contents _ -> do
            bs <- evaluate $ LBS.toStrict contents
            idxFile <- either (throwIO . InvalidIndexFile) return (elaborateIndexFile fpath)
            let entry' = IndexEntry
                    { entryPath        = Tar.fromTarPath (Tar.entryTarPath entry)
                    , entryPermissions = Tar.entryPermissions entry
                    , entryOwnership   = Tar.entryOwnership entry
                    , entryTime        = Tar.entryTime entry
                    , entryType        = idxFile
                    , entryTarOffset   = offset
                    }
            next <- action entry' bs acc
            return (Acc (Tar.nextEntryOffset entry offset) next)

        -- all other entries
        _ -> return (Acc (Tar.nextEntryOffset entry offset) acc)
     where
       fpath = Tar.entryPath entry

data Acc a = Acc !Tar.TarEntryOffset !a

foldEntries :: (a -> Tar.Entry -> IO a) -> (e -> IO a) -> a -> Tar.Entries e -> IO a
foldEntries next fail' = go where
    go !acc (Tar.Next e es) = next acc e >>= \acc' -> go acc' es
    go  _   (Tar.Fail e)    = fail' e
    go  acc Tar.Done        = return acc

-------------------------------------------------------------------------------
-- IndexFile
-------------------------------------------------------------------------------

data IndexEntry = IndexEntry
    { entryPath        :: !FilePath
    , entryType        :: !IndexFileType
    , entryPermissions :: !Tar.Permissions
    , entryOwnership   :: !Tar.Ownership
    , entryTime        :: !Tar.EpochTime
    , entryTarOffset   :: !Tar.TarEntryOffset
    }
  deriving Show

-- | Varions files in @01-index.tar@.
data IndexFileType
    = CabalFile C.PackageName C.Version
    | PackageJson C.PackageName C.Version
    | PreferredVersions C.PackageName
  deriving (Show)

-- | Thrown when when not a @.cabal@, @package.json@ or @preferred-versions@
-- file is encountered.
newtype InvalidIndexFile = InvalidIndexFile String
  deriving (Show)

instance Exception InvalidIndexFile

elaborateIndexFile :: FilePath -> Either String IndexFileType
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
-- SHA256
-------------------------------------------------------------------------------

-- | SHA256 digest. 256 bytes.
data SHA256 = SHA256 !Word64 !Word64 !Word64 !Word64
  deriving (Eq, Ord)

-- | Hash strict 'ByteString'.
sha256 :: ByteString -> SHA256
sha256 = sha256Digest . check . SHA256.hash
  where
    check bs
        | BS.length bs == 32 = bs
        | otherwise          = error $ "panic! SHA256.hash returned ByteStrign of length " ++ show (BS.length bs) ++ " /= 32"

-- unsafe construct. You should check the length of bytestring beforehand.
sha256Digest :: ByteString -> SHA256
sha256Digest bs = SHA256
    (   shiftL (fromIntegral (BS.Unsafe.unsafeIndex bs  0)) 56
    .|. shiftL (fromIntegral (BS.Unsafe.unsafeIndex bs  1)) 48
    .|. shiftL (fromIntegral (BS.Unsafe.unsafeIndex bs  2)) 40
    .|. shiftL (fromIntegral (BS.Unsafe.unsafeIndex bs  3)) 32
    .|. shiftL (fromIntegral (BS.Unsafe.unsafeIndex bs  4)) 24
    .|. shiftL (fromIntegral (BS.Unsafe.unsafeIndex bs  5)) 16
    .|. shiftL (fromIntegral (BS.Unsafe.unsafeIndex bs  6))  8
    .|. shiftL (fromIntegral (BS.Unsafe.unsafeIndex bs  7))  0
    )
    (   shiftL (fromIntegral (BS.Unsafe.unsafeIndex bs  8)) 56
    .|. shiftL (fromIntegral (BS.Unsafe.unsafeIndex bs  9)) 48
    .|. shiftL (fromIntegral (BS.Unsafe.unsafeIndex bs 10)) 40
    .|. shiftL (fromIntegral (BS.Unsafe.unsafeIndex bs 11)) 32
    .|. shiftL (fromIntegral (BS.Unsafe.unsafeIndex bs 12)) 24
    .|. shiftL (fromIntegral (BS.Unsafe.unsafeIndex bs 13)) 16
    .|. shiftL (fromIntegral (BS.Unsafe.unsafeIndex bs 14))  8
    .|. shiftL (fromIntegral (BS.Unsafe.unsafeIndex bs 15))  0
    )
    (   shiftL (fromIntegral (BS.Unsafe.unsafeIndex bs 16)) 56
    .|. shiftL (fromIntegral (BS.Unsafe.unsafeIndex bs 17)) 48
    .|. shiftL (fromIntegral (BS.Unsafe.unsafeIndex bs 18)) 40
    .|. shiftL (fromIntegral (BS.Unsafe.unsafeIndex bs 19)) 32
    .|. shiftL (fromIntegral (BS.Unsafe.unsafeIndex bs 20)) 24
    .|. shiftL (fromIntegral (BS.Unsafe.unsafeIndex bs 21)) 16
    .|. shiftL (fromIntegral (BS.Unsafe.unsafeIndex bs 22))  8
    .|. shiftL (fromIntegral (BS.Unsafe.unsafeIndex bs 23))  0
    )
    (   shiftL (fromIntegral (BS.Unsafe.unsafeIndex bs 24)) 56
    .|. shiftL (fromIntegral (BS.Unsafe.unsafeIndex bs 25)) 48
    .|. shiftL (fromIntegral (BS.Unsafe.unsafeIndex bs 26)) 40
    .|. shiftL (fromIntegral (BS.Unsafe.unsafeIndex bs 27)) 32
    .|. shiftL (fromIntegral (BS.Unsafe.unsafeIndex bs 28)) 24
    .|. shiftL (fromIntegral (BS.Unsafe.unsafeIndex bs 29)) 16
    .|. shiftL (fromIntegral (BS.Unsafe.unsafeIndex bs 30))  8
    .|. shiftL (fromIntegral (BS.Unsafe.unsafeIndex bs 31))  0
    )

-- | Make SHA256 from base16-encoded string.
mkSHA256 :: Text -> Either String SHA256
mkSHA256 t = case Base16.decode (TE.encodeUtf8 t) of
    Left err                      -> Left $ "Base16 decoding failure: " ++ err
    Right bs | BS.length bs /= 32 -> Left $ "Base16 of wrong length, expected 32, got " ++ show (BS.length bs)
             | otherwise          -> Right (sha256Digest bs)

-- | Unsafe variant of 'mkSHA256'.
unsafeMkSHA256 :: Text -> SHA256
unsafeMkSHA256 = either error id . mkSHA256

-- | Get 'ByteString' representation of 'SHA256'.
getSHA256 :: SHA256 -> ByteString
getSHA256 (SHA256 a b c d) = BS.pack
    [ fromIntegral (shiftR a 56 .&. 0xff)
    , fromIntegral (shiftR a 48 .&. 0xff)
    , fromIntegral (shiftR a 40 .&. 0xff)
    , fromIntegral (shiftR a 32 .&. 0xff)
    , fromIntegral (shiftR a 24 .&. 0xff)
    , fromIntegral (shiftR a 16 .&. 0xff)
    , fromIntegral (shiftR a  8 .&. 0xff)
    , fromIntegral (shiftR a  0 .&. 0xff)

    , fromIntegral (shiftR b 56 .&. 0xff)
    , fromIntegral (shiftR b 48 .&. 0xff)
    , fromIntegral (shiftR b 40 .&. 0xff)
    , fromIntegral (shiftR b 32 .&. 0xff)
    , fromIntegral (shiftR b 24 .&. 0xff)
    , fromIntegral (shiftR b 16 .&. 0xff)
    , fromIntegral (shiftR b  8 .&. 0xff)
    , fromIntegral (shiftR b  0 .&. 0xff)

    , fromIntegral (shiftR c 56 .&. 0xff)
    , fromIntegral (shiftR c 48 .&. 0xff)
    , fromIntegral (shiftR c 40 .&. 0xff)
    , fromIntegral (shiftR c 32 .&. 0xff)
    , fromIntegral (shiftR c 24 .&. 0xff)
    , fromIntegral (shiftR c 16 .&. 0xff)
    , fromIntegral (shiftR c  8 .&. 0xff)
    , fromIntegral (shiftR c  0 .&. 0xff)

    , fromIntegral (shiftR d 56 .&. 0xff)
    , fromIntegral (shiftR d 48 .&. 0xff)
    , fromIntegral (shiftR d 40 .&. 0xff)
    , fromIntegral (shiftR d 32 .&. 0xff)
    , fromIntegral (shiftR d 24 .&. 0xff)
    , fromIntegral (shiftR d 16 .&. 0xff)
    , fromIntegral (shiftR d  8 .&. 0xff)
    , fromIntegral (shiftR d  0 .&. 0xff)
    ]

instance C.Pretty SHA256 where
    pretty = PP.text . C.fromUTF8BS . Base16.encode . getSHA256

instance Show SHA256 where
    showsPrec d h
        = showParen (d > 10)
        $ showString "unsafeMkSHA256 "
        . shows (Base16.encode (getSHA256 h))

instance Binary.Binary SHA256 where
    put (SHA256 a b c d) = do
        Binary.Put.putWord64be a
        Binary.Put.putWord64be b
        Binary.Put.putWord64be c
        Binary.Put.putWord64be d
    get = do
        a <- Binary.Get.getWord64be
        b <- Binary.Get.getWord64be
        c <- Binary.Get.getWord64be
        d <- Binary.Get.getWord64be
        return (SHA256 a b c d)

-------------------------------------------------------------------------------
-- MD5
-------------------------------------------------------------------------------

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
    Left err                      -> Left $ "Base16 decoding failure: " ++ err
    Right bs | BS.length bs /= 16 -> Left $ "Base16 of wrong length, expected 16, got " ++ show (BS.length bs)
             | otherwise          -> Right (MD5 bs)

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
  deriving (Eq, Show, Generic)

instance Binary.Binary PackageInfo

-- | Like 'piVersions', but return only 'piPreferred' versions.
piPreferredVersions :: PackageInfo -> Map C.Version ReleaseInfo
piPreferredVersions pi =
    Map.filterWithKey (\v _ -> v `C.withinRange` piPreferred pi) (piVersions pi)

-- | Package's release information.
data ReleaseInfo = ReleaseInfo
    { riRevision  :: !Word32              -- ^ revision number
    , riTarOffset :: !Tar.TarEntryOffset  -- ^ offset into tar file
    , riCabal     :: !SHA256              -- ^ hash of the last revision of @.cabal@ file
    , riTarball   :: !SHA256              -- ^ hash of the @.tar.gz@ file.
    }
  deriving (Eq, Show, Generic)

instance Binary.Binary ReleaseInfo

-------------------------------------------------------------------------------
-- Metadata construction
-------------------------------------------------------------------------------

-- | Read index file and return the metadata about packages.
--
-- It takes about 6 seconds on my machine. Consider using 'cachedHackageMetadata'.
--
indexMetadata
    :: FilePath             -- ^ location
    -> Maybe Tar.EpochTime  -- ^ index state to stop
    -> IO (Map C.PackageName PackageInfo)
indexMetadata indexFilepath mindexState = do
    let shouldStop :: Tar.EpochTime -> Bool
        shouldStop = case mindexState of
            Nothing         -> \_ -> False
            Just indexState -> \t -> t >= indexState

    result <- foldIndex indexFilepath Map.empty $ \indexEntry contents !m ->
        if shouldStop (entryTime indexEntry)
        then return m
        else case entryType indexEntry of
            CabalFile pn ver -> return (Map.alter f pn m) where
                digest :: SHA256
                digest = sha256 contents

                offset :: Tar.TarEntryOffset
                offset = entryTarOffset indexEntry

                f :: Maybe TmpPackageInfo -> Maybe TmpPackageInfo
                f Nothing = Just TmpPackageInfo
                    { tmpPiVersions  = Map.singleton ver (TmpReleaseInfo 0 offset (Just digest) Nothing)
                    , tmpPiPreferred = C.anyVersion
                    }
                f (Just pi) = Just pi { tmpPiVersions = Map.alter g ver (tmpPiVersions pi) }

                g :: Maybe TmpReleaseInfo -> Maybe TmpReleaseInfo
                g Nothing                                 = Just $ TmpReleaseInfo 0        offset (Just digest) Nothing
                g (Just (TmpReleaseInfo _r _o Nothing t)) = Just $ TmpReleaseInfo 0        offset (Just digest) t
                g (Just (TmpReleaseInfo  r _o _c      t)) = Just $ TmpReleaseInfo (succ r) offset (Just digest) t

            PackageJson pn ver -> case A.eitherDecodeStrict contents of
                    Left err -> throwIO $ MetadataParseError (entryPath indexEntry) err
                    Right (PJ (Signed (Targets ts))) ->
                        case Map.lookup ("<repo>/package/" ++ C.prettyShow pn ++ "-" ++ C.prettyShow ver ++ ".tar.gz") ts of
                            Just t  -> return (Map.alter (f t) pn m)
                            Nothing -> throwIO $ MetadataParseError (entryPath indexEntry) $ "Invalid targets in " ++ entryPath indexEntry ++ " -- " ++ show ts
                      where
                        f :: Target -> Maybe TmpPackageInfo -> Maybe TmpPackageInfo
                        f t Nothing   = Just TmpPackageInfo
                            { tmpPiVersions  = Map.singleton ver (TmpReleaseInfo 0 0 Nothing (Just (hashSHA256 (targetHashes t))))
                            , tmpPiPreferred = C.anyVersion
                            }
                        f t (Just pi) = Just pi { tmpPiVersions = Map.alter (g t) ver (tmpPiVersions pi) }

                        g :: Target -> Maybe TmpReleaseInfo -> Maybe TmpReleaseInfo
                        g t Nothing                         = Just $ TmpReleaseInfo 0 0 Nothing (Just (hashSHA256 (targetHashes t)))
                        g t (Just (TmpReleaseInfo r o c _)) = Just $ TmpReleaseInfo r o c       (Just (hashSHA256 (targetHashes t)))

            PreferredVersions pn
                    | BS.null contents -> return m
                    | otherwise        -> case explicitEitherParsecBS preferredP contents of
                        Right vr -> return (Map.alter (f vr) pn m)
                        Left err -> throwIO $ MetadataParseError (entryPath indexEntry) err
                  where
                    preferredP = do
                        _ <- C.string (C.prettyShow pn)
                        C.spaces
                        C.parsec

                    f :: C.VersionRange -> Maybe TmpPackageInfo -> Maybe TmpPackageInfo
                    f vr Nothing = Just TmpPackageInfo
                        { tmpPiVersions  = Map.empty
                        , tmpPiPreferred = vr
                        }
                    f vr (Just pi) = Just pi { tmpPiPreferred = vr }

    -- check invariants and return
    postCheck result

postCheck :: Map C.PackageName TmpPackageInfo -> IO (Map C.PackageName PackageInfo)
postCheck meta = ifor meta $ \pn pi -> do
    versions <- ifor (tmpPiVersions pi) $ \ver ri -> do
        cabal   <- maybe (throwIO $ InvalidHash pn ver "cabal")   return (tmpRiCabal   ri)
        tarball <- maybe (throwIO $ InvalidHash pn ver "tarball") return (tmpRiTarball ri)
        return ReleaseInfo
            { riRevision  = tmpRiRevision ri
            , riTarOffset = tmpRiTarOffset ri
            , riCabal     = cabal
            , riTarball   = tarball
            }

    return PackageInfo
        { piPreferred = tmpPiPreferred pi
        , piVersions  = versions
        }
  where
    ifor :: Map k v -> (k -> v -> IO v') -> IO (Map k v')
    ifor = flip Map.traverseWithKey

-- | Thrown when we cannot parse @package.json@ or @preferred-versions@ files.
data MetadataParseError = MetadataParseError FilePath String
  deriving (Show)

instance Exception MetadataParseError

-- | Thrown if we fail consistency check, we don't know a hash for some file.
data InvalidHash = InvalidHash C.PackageName C.Version String
  deriving (Show)

instance Exception InvalidHash

-------------------------------------------------------------------------------
-- Temporary types for indexMetadata
-------------------------------------------------------------------------------

data TmpPackageInfo = TmpPackageInfo
    { tmpPiVersions  :: Map C.Version TmpReleaseInfo  -- ^ individual package releases
    , tmpPiPreferred :: C.VersionRange                -- ^ preferred versions range
    }

data TmpReleaseInfo = TmpReleaseInfo
    { tmpRiRevision  :: !Word32              -- ^ revision number
    , tmpRiTarOffset :: !Tar.TarEntryOffset  -- ^ offset into tar file
    , tmpRiCabal     :: !(Maybe SHA256)      -- ^ hash of the last revision of @.cabal@ file
    , tmpRiTarball   :: !(Maybe SHA256)      -- ^ hash of the @.tar.gz@ file.
    }

-------------------------------------------------------------------------------
-- Hackage
-------------------------------------------------------------------------------

-- | Read the config and then Hackage index metadata.
--
-- This method caches the result in @XDG_CACHE/cabal-parsers@ directory.
--
-- Returns the location of index tarball and its contents.
--
cachedHackageMetadata :: IO (FilePath, Map C.PackageName PackageInfo)
cachedHackageMetadata = do
    -- read config
    cfg <- readConfig
    indexPath <- maybe
        (throwIO NoHackageRepository)
        return
        (cfgRepoIndex cfg hackageHaskellOrg)

    -- cache directory
    cacheDir <- D.getXdgDirectory D.XdgCache "cabal-parsers"
    D.createDirectoryIfMissing True cacheDir
    let cacheFile = cacheDir FP.</> "hackage.binary"

    -- lock the cache
    bracket (takeLock supported cacheDir) (releaseLock supported) $ \_ -> do
        (size, time) <- getStat indexPath

        mcache <- readCache cacheFile
        case mcache of
            Just cache | cacheSize cache == size && cacheTime cache == time ->
                return (indexPath, cacheData cache)
            _ -> do
                meta <- indexMetadata indexPath Nothing
                LBS.writeFile cacheFile $ Binary.encode Cache
                    { cacheMagic = Magic
                    , cacheTime  = time
                    , cacheSize  = size
                    , cacheData  = meta
                    }
                return (indexPath, meta)

  where
    readCache :: FilePath -> IO (Maybe Cache)
    readCache fp = handle onIOError $ do
        contents <- LBS.readFile fp
        case Binary.decodeOrFail contents of
            Right (lo,_,x) | LBS.null lo -> return (Just x)
            _                            -> return Nothing

    onIOError :: IOException -> IO (Maybe a)
    onIOError _ = return Nothing

    supported :: SBool Lukko.FileLockingSupported
    supported = sbool

    takeLock :: SBool b -> FilePath -> IO (FDType b)
    takeLock STrue  dir = do
        fd <- Lukko.fdOpen (dir FP.</> "lock")
        Lukko.fdLock fd Lukko.ExclusiveLock
        return fd
    takeLock SFalse _   = return ()

    releaseLock :: SBool b -> FDType b -> IO ()
    releaseLock STrue  fd = Lukko.fdUnlock fd >> Lukko.fdClose fd
    releaseLock SFalse () = return ()

    getStat :: FilePath -> IO (Int64, Int64)
    getStat p = do
        size <- D.getFileSize p
        time <- D.getModificationTime p
        return (fromIntegral size, truncate (Time.utcTimeToPOSIXSeconds time))

data NoHackageRepository = NoHackageRepository
  deriving Show

instance Exception NoHackageRepository

data Cache = Cache
    { cacheMagic :: !Magic
    , cacheSize  :: !Int64
    , cacheTime  :: !Int64
    , cacheData  :: Map C.PackageName PackageInfo
    }
  deriving Generic

instance Binary.Binary Cache

-- special type to make binary fail early
data Magic = Magic

instance Binary.Binary Magic where
    put _ = Binary.put magicNumber
    get = do
        m <- Binary.get
        if m == magicNumber then return Magic else fail "Got wrong magic number"

magicNumber :: Word64
magicNumber = 0xF000F000F0004000

-------------------------------------------------------------------------------
-- mini bool-singetons
-------------------------------------------------------------------------------

class SBoolI (b :: Bool) where
    type FDType b
    sbool :: SBool b

instance SBoolI 'True where
    type FDType 'True = Lukko.FD
    sbool = STrue

instance SBoolI 'False where
    type FDType 'False = ()
    sbool = SFalse

data SBool (b :: Bool) where
    STrue  :: SBool 'True
    SFalse :: SBool 'False

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
