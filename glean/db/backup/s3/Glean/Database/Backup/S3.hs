{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}
{-# LANGUAGE TypeApplications #-}

{- | S3 backup backend for Glean.

Requirements:
- S3 bucket in a region which supports metadata (available in the [largest regions](https://docs.aws.amazon.com/AmazonS3/latest/userguide/metadata-tables-restrictions.html#metadata-tables-regions)).
- AWS credential discovery configured on the machine ([See Amazonka docs](https://hackage.haskell.org/package/amazonka-2.0/docs/Amazonka-Auth.html#v:discover))

Usage example:

@
glean --tier test --db-root ~/glean backup myrepo/0 s3:mybucket/my-dir
glean --tier test --db-root ~/glean2 restore s3:mybucket/my-dir/myrepo.0
@
-}
module Glean.Database.Backup.S3 (withS3Backups, fakeS3Backend) where

import Control.Arrow (Arrow (..))
import Control.Exception.Safe (throwIO)
import Control.Monad ((<=<), (>=>))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LBS
import Data.Functor (void)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import qualified Network.HTTP.Client as HTTP
import UnliftIO (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import UnliftIO.Async (forConcurrently)
import UnliftIO.Exception.Lens

import qualified Amazonka as AWS
import qualified Amazonka.S3 as S3
import Amazonka.S3.DeleteObject
import Amazonka.S3.GetObject
import Amazonka.S3.HeadObject
import Amazonka.S3.ListObjectsV2
import Amazonka.S3.PutObject
import Amazonka.S3.Types.Object
import Conduit
import Lens.Micro
import Lens.Micro.Extras

import Glean.Database.Backup.Backend
import qualified Glean.Database.Config as DBConfig
import Glean.Database.Exception
import Glean.Internal.Types (Meta (..))
import Glean.Types (Repo (..))
import qualified Glean.Types as Thrift hiding (Exception)
import Glean.Util.Some
import qualified Thrift.Protocol.JSON as Thrift
import Util.Concurrent (cacheSuccess)
import Util.Log.Text

withS3Backups :: DBConfig.Config -> IO DBConfig.Config
withS3Backups cfg@DBConfig.Config{..} = do
  s3AwsEnvLazy <- cacheSuccess $ withLogging <$> AWS.newEnv AWS.discover
  pure cfg{DBConfig.cfgBackupBackends = cfgBackupBackends <> HashMap.fromList [("s3", Some (genuineS3Backend s3AwsEnvLazy))]}

withLogging :: AWS.Env' withAuth -> AWS.Env' withAuth
withLogging env = env{AWS.logger = \lvl -> ourLogger lvl . convertString}
 where
  convertString = Text.decodeUtf8With Text.lenientDecode . BS.toStrict . BB.toLazyByteString

  ourLogger AWS.Error msg = logError msg
  ourLogger AWS.Info msg = logInfo msg
  -- These are really spammy (especially at trace) so make them -v 2 only.
  ourLogger AWS.Debug msg = vlog 2 msg
  ourLogger AWS.Trace msg = vlog 3 msg

-- | S3 backup backend, which auto discovers its environment configuration.
data S3Backend = S3Backend {s3BucketFactory :: (Text -> Some S3BucketApi)}

-- | Creates a real S3 based backend.
genuineS3Backend :: IO AWS.Env -> S3Backend
genuineS3Backend awsEnv = newS3Backend (\name -> GenuineS3Bucket awsEnv (S3.BucketName name))

newS3Backend :: (S3BucketApi bucket) => (Text -> bucket) -> S3Backend
newS3Backend factory = S3Backend{s3BucketFactory = \name -> Some (factory name)}

-- | Creates a new fake in-memory S3 backend.
fakeS3Backend :: (MonadIO m) => m S3Backend
fakeS3Backend = do
  fakeFiles <- newIORef Map.empty
  pure $ newS3Backend (\name -> FakeS3Bucket{fakeBucketName = name, fakeFiles})

instance Backend S3Backend where
  fromPath S3Backend{s3BucketFactory} path = do
    let (bucketName, bucketBasePath') = Text.breakOn "/" path
    (_, bucketBasePath) <- Text.uncons bucketBasePath'
    Just . Some $ S3Site{s3Client = s3BucketFactory bucketName, bucketBasePath}

-- | File in an S3 bucket.
data Item = Item
  { itemPath :: Text
  }

type Metadata = HashMap.HashMap Text Text

-- | A client to an S3 bucket, as a seam for unit testing purposes.
class S3BucketApi a where
  -- | Gets the name of the bucket this is a client for.
  bucketName :: a -> Text

  {- | Lists items with a prefix.

  We aren't using the delimiter functionality (which does dedupe by common
  prefixes) as we need both the meta and the file itself to exist for
  consistency, so we need to catch both of those.

  This also doesn't concern itself with pagination because our consuming API
  doesn't either.
  -}
  listItemsWithPrefix :: (MonadResource m, MonadUnliftIO m) => a -> Text -> m [Item]

  -- | Uploads a file on disk to the given path in the bucket.
  uploadFile :: (MonadResource m, MonadUnliftIO m) => a -> Text -> Metadata -> AWS.RequestBody -> m ()

  {- | Downloads a file from S3.

  Throws for any error but absent key.
  -}
  downloadFile :: (MonadResource m, MonadUnliftIO m) => a -> Text -> m (Maybe (Metadata, AWS.ResponseBody))

  {- | Checks a file exists on S3.

  Throws for any error but absent key.
  -}
  headFile :: (MonadResource m, MonadUnliftIO m) => a -> Text -> m (Maybe Metadata)

  -- | Deletes a file in the bucket.
  deleteFile :: (MonadResource m, MonadUnliftIO m) => a -> Text -> m ()

instance S3BucketApi (Some S3BucketApi) where
  bucketName (Some bucket) = bucketName bucket
  listItemsWithPrefix (Some bucket) = listItemsWithPrefix bucket
  uploadFile (Some bucket) = uploadFile bucket
  downloadFile (Some bucket) = downloadFile bucket
  headFile (Some bucket) = headFile bucket
  deleteFile (Some bucket) = deleteFile bucket

data GenuineS3Bucket = GenuineS3Bucket
  { awsEnvLazy :: IO AWS.Env
  , s3BucketName :: S3.BucketName
  }

instance S3BucketApi GenuineS3Bucket where
  bucketName = view S3._BucketName . s3BucketName

  listItemsWithPrefix GenuineS3Bucket{awsEnvLazy, s3BucketName} prefix = do
    awsEnv <- liftIO awsEnvLazy
    let request =
          newListObjectsV2 s3BucketName
            & listObjectsV2_prefix
            ?~ prefix
    runConduit $
      AWS.paginate awsEnv request
        .| concatMapC (^. listObjectsV2Response_contents)
        .| concatC
        .| mapC (\obj -> Item{itemPath = obj ^. object_key . S3._ObjectKey})
        .| sinkList

  uploadFile GenuineS3Bucket{awsEnvLazy, s3BucketName} name metadata body = do
    awsEnv <- liftIO awsEnvLazy
    let req =
          newPutObject s3BucketName (S3.ObjectKey name) body
            & (putObject_metadata .~ metadata)
    void . AWS.send awsEnv $ req

  headFile GenuineS3Bucket{awsEnvLazy, s3BucketName} name = do
    awsEnv <- liftIO awsEnvLazy
    let req = newHeadObject s3BucketName (S3.ObjectKey name)
    handling_ S3._NoSuchKey (pure Nothing) $
      Just . view headObjectResponse_metadata <$> AWS.send awsEnv req

  downloadFile GenuineS3Bucket{awsEnvLazy, s3BucketName} name = do
    awsEnv <- liftIO awsEnvLazy
    let req = newGetObject s3BucketName (S3.ObjectKey name)
    handling_ S3._NoSuchKey (pure Nothing) $
      Just . (view getObjectResponse_metadata &&& view getObjectResponse_body)
        <$> AWS.send awsEnv req

  deleteFile GenuineS3Bucket{awsEnvLazy, s3BucketName} name = do
    awsEnv <- liftIO awsEnvLazy
    let req = newDeleteObject s3BucketName (S3.ObjectKey name)
    void $ AWS.send awsEnv req

data FakeS3Bucket = FakeS3Bucket
  { fakeFiles :: IORef (Map.Map Text (Metadata, ByteString))
  , fakeBucketName :: Text
  }

-- FIXME(jadel): upstream this to http-client
materializeRequestBody :: HTTP.RequestBody -> IO LBS.ByteString
materializeRequestBody = \case
  HTTP.RequestBodyLBS lbs -> pure lbs
  HTTP.RequestBodyBS bs -> pure (BS.fromStrict bs)
  HTTP.RequestBodyBuilder _ b -> pure (BB.toLazyByteString b)
  HTTP.RequestBodyStream _ g -> withPopper g
  HTTP.RequestBodyStreamChunked g -> withPopper g
  HTTP.RequestBodyIO io -> materializeRequestBody =<< io
 where
  withPopper :: HTTP.GivesPopper () -> IO LBS.ByteString
  withPopper giver = do
    r <- newIORef []
    giver $ do writeIORef r <=< getChunks
    LBS.fromChunks <$> readIORef r

  getChunks :: IO ByteString -> IO [ByteString]
  getChunks io =
    io >>= \case
      "" -> pure []
      chunk -> (chunk :) <$> getChunks io

instance S3BucketApi FakeS3Bucket where
  bucketName = fakeBucketName

  listItemsWithPrefix FakeS3Bucket{fakeFiles} prefix = do
    fmap Item . filter (prefix `Text.isPrefixOf`) . Map.keys <$> readIORef fakeFiles

  uploadFile FakeS3Bucket{fakeFiles} name metadata body = do
    body <- liftIO . materializeRequestBody $ AWS.toRequestBody body
    atomicModifyIORef' fakeFiles (\files -> (files <> Map.singleton name (metadata, BS.toStrict body), ()))

  headFile FakeS3Bucket{fakeFiles} name = do
    fmap fst . Map.lookup name <$> readIORef fakeFiles

  downloadFile FakeS3Bucket{fakeFiles} name = do
    fmap (\(meta, body) -> (meta, AWS.ResponseBody $ yield body)) . Map.lookup name <$> readIORef fakeFiles

  deleteFile FakeS3Bucket{fakeFiles} name = do
    atomicModifyIORef' fakeFiles ((,()) . (`Map.withoutKeys` (Set.singleton name)))

data S3Site = S3Site
  { s3Client :: Some S3BucketApi
  , bucketBasePath :: Text
  }

makeRepoPath :: Text -> Repo -> Text
makeRepoPath bucketBasePath Repo{repo_name, repo_hash} = Text.intercalate "/" [bucketBasePath, repo_name, repo_hash]

dbPath :: Text -> Text
dbPath = (<> ".tar.gz")

metadataKey :: Text
metadataKey = "glean-metadata"

parseMeta :: (MonadIO m, MonadThrow m) => Repo -> Text -> m Meta
parseMeta repo = either (dbError repo) pure . Thrift.deserializeJSON . Text.encodeUtf8

instance Site S3Site where
  backup S3Site{s3Client, bucketBasePath} repo meta _ttl fromPath = runResourceT $ do
    let repoPath = makeRepoPath bucketBasePath repo
    body <- AWS.chunkedFile AWS.defaultChunkSize fromPath

    -- https://docs.aws.amazon.com/AmazonS3/latest/userguide/UsingMetadata.html#UserMetadata
    -- Arbitrary printable Unicode characters only means that any binary
    -- encoding would force us to base64 it, which seems like a hassle and
    -- maybe not even more compact.
    let meta' = HashMap.singleton metadataKey (Text.decodeUtf8 $ Thrift.serializeJSON meta)
    _ <- uploadFile s3Client (dbPath repoPath) meta' body
    pure $ Data (fromIntegral $ AWS.contentLength body)

  delete S3Site{s3Client, bucketBasePath} repo = runResourceT $ do
    let repoPath = makeRepoPath bucketBasePath repo
    deleteFile s3Client (dbPath repoPath)

  restore S3Site{s3Client, bucketBasePath} repo intoPath = runResourceT $ do
    let repoPath = makeRepoPath bucketBasePath repo
    res <- downloadFile s3Client (dbPath repoPath)
    case res of
      Just (meta, repoStream)
        | Just metaJson <- HashMap.lookup metadataKey meta -> do
            meta <- parseMeta repo metaJson
            runConduit $ AWS.sinkBody repoStream (sinkFile intoPath)
            pure meta
      _ -> throwIO . Thrift.InvalidLocator $ "locator is missing either metadata or db.tar.gz" <> (Text.pack . show) repo

  inspect S3Site{s3Client, bucketBasePath} repo = runResourceT $ do
    let repoPath = makeRepoPath bucketBasePath repo
    res <- headFile s3Client (dbPath repoPath)

    case res of
      Just meta
        | Just metaJson <- HashMap.lookup metadataKey meta ->
            parseMeta repo metaJson
      _ -> throwIO . Thrift.InvalidLocator $ "locator is missing either metadata or db.tar.gz: " <> (Text.pack . show) repo

  enumerate site@S3Site{s3Client, bucketBasePath} = runResourceT $ do
    items <- listItemsWithPrefix s3Client bucketBasePath
    let parsed = catMaybes $ map parseItemFilename items
    forConcurrently parsed $ \repo -> (repo,) <$> liftIO (inspect site repo)
   where
    parseItemFilename =
      (Text.stripPrefix (bucketBasePath <> "/") . itemPath)
        >=> Text.stripSuffix ".tar.gz"
        >=> splitFilename
    -- >>> splitFilename "myrepo/123"
    -- Just (Repo "myrepo" "123")
    splitFilename name
      | let (withTrailingSlash, repo_hash) = Text.breakOnEnd "/" name
      , Just (repo_name, _slash) <- Text.unsnoc withTrailingSlash =
          Just Repo{repo_name, repo_hash}
    splitFilename _name = Nothing

  toPath S3Site{s3Client, bucketBasePath} = bucketName s3Client <> "/" <> bucketBasePath
