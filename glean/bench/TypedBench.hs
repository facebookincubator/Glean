-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE TypeApplications #-}

-- | Benchmarks decoding of Glean binary data in Haskell
module TypedBench (main)
where

import Criterion.Main
import Data.IORef

import Glean.RTS
import Glean.RTS.Builder
import qualified Glean.Schema.Src.Types as Src
import qualified Glean.Schema.Thrift.Types as Thrift_
import Glean.Typed
import qualified Glean.Types as Thrift -- generated

encodeFact :: Predicate p => p -> IO Thrift.Fact
{-# NOINLINE encodeFact #-}
encodeFact p
  | Just key <- getFactKey p = do
      k <- withBuilder $ \b -> buildRtsValue b key >> finishBuilder b
      v <- case getFactValue p of
        Just value ->
          withBuilder $ \b -> buildRtsValue b value >> finishBuilder b
        Nothing -> return mempty
      return $ Thrift.Fact 1234 k v
  | otherwise = fail "encodeFact: missing key"

main :: IO ()
main = do
  src_file <- encodeFact Src.File
    { file_id = 1234
    , file_key = Just "goodbye/cruel/world"
    }
  thrift_file <- encodeFact Thrift_.File
    { file_id = 1235
    , file_key = Just (justId $ IdOf $ Fid 1234)
    }
  r <- newIORef mempty

  let fid = IdOf (Fid 1234)

      -- This shouldn't be inlined and hence monomorphised.
      decodePoly :: Predicate p => Thrift.Fact -> IO p
      {-# NOINLINE decodePoly #-}
      decodePoly x = decodeFact mempty r fid x

      -- Will be inlined and made monomorphic.
      decodeMono :: Predicate p => Thrift.Fact -> IO p
      {-# INLINE decodeMono #-}
      decodeMono x = decodeFact mempty r fid x

  -- NOTE: evaluating to whnf is enough here because we're creating all parts of
  -- the fact in IO, anyway
  defaultMain
    [ bgroup "poly"
      [ bench "Src.File"
          $ whnfIO (decodePoly @Src.File src_file)
      , bench "Thrift.File"
          $ whnfIO (decodePoly @Thrift_.File thrift_file)
      ]
    , bgroup "mono"
      [ bench "Src.File"
          $ whnfIO (decodeMono @Src.File src_file)
      , bench "Thrift.File"
          $ whnfIO (decodeMono @Thrift_.File thrift_file)
      ]
    ]
