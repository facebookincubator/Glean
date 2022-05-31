{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module RTSTest where

import qualified Data.ByteString as BS
import qualified Data.Text as Text
import Data.Text.Arbitrary ()
import qualified Data.Text.Encoding as Text
import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.Monadic as QuickCheck

import TestRunner
import Util.Testing

import qualified Glean.Angle.Types as T
import Glean.Init
import Glean.RTS
import Glean.RTS.Foreign.Typecheck as Bytecode
import Glean.RTS.Typecheck
import Glean.RTS.Types
import Glean.RTS.Term

instance Arbitrary Fid where
  arbitrary = Fid <$> choose (1024,0x1FFFFFFFFFFF)

instance Arbitrary Pid where
  arbitrary = Pid <$> choose (1024,0x1FFFFFFFFFFF)

instance Arbitrary PidRef where
  arbitrary = do
    pid <- arbitrary
    return $ PidRef pid $ T.PredicateRef ("P" <> Text.pack (show pid)) 1

instance Arbitrary Type where
  arbitrary = oneof
    [ pure T.ByteTy
    , pure T.NatTy
    , pure T.StringTy
    , T.ArrayTy <$> arbitrary
    , T.RecordTy . fields <$> children 0
    , T.SumTy . fields <$> children 1
    , T.PredicateTy <$> arbitrary
    , T.MaybeTy <$> arbitrary
    , sized $ \n -> do
        k <- choose (0, max 0 n)
        return $ T.EnumeratedTy ["E" <> Text.pack (show i) | i <- [0 .. k]]
    , pure T.BooleanTy
    ]
    where
      children i = sized $ \n -> do
        k <- choose (i, max i n)
        vectorOf k $ resize (n `div` k) arbitrary

      fields tys =
        [ T.FieldDef (Text.pack $ 'x' : show (i :: Int)) ty
          | (i,ty) <- zip [0..] tys ]

valueFor :: Type -> Gen Value
valueFor T.ByteTy = Byte <$> arbitrary
valueFor T.NatTy = Nat <$> arbitrary
valueFor T.StringTy = String . Text.encodeUtf8 <$> arbitrary
valueFor (T.ArrayTy ty)
  | T.ByteTy <- derefType ty = ByteArray . BS.pack <$> arbitrary
  | otherwise = fmap Array $ sized $ \n -> do
      k <- choose (0,n)
      vectorOf k $ resize (n `div` k) $ valueFor ty
valueFor (T.RecordTy fields) =
  Tuple <$> mapM (valueFor . T.fieldDefType) fields
valueFor (T.SumTy fields) = do
  (i, field) <- elements $ zip [0..] fields
  Alt i <$> valueFor (T.fieldDefType field)
valueFor T.PredicateTy{} = Ref <$> arbitrary
valueFor (T.NamedTy (ExpandedType _ ty)) = valueFor ty
valueFor (T.MaybeTy ty) = do
  b <- arbitrary
  if b then Alt 1 <$> valueFor ty else return $ Alt 0 $ Tuple []
valueFor (T.EnumeratedTy names) = do
  i <- choose (0, fromIntegral (length names - 1))
  return $ Alt i $ Tuple []
valueFor T.BooleanTy = do
  i <- choose (0,1)
  return $ Alt i $ Tuple []

prop_roundtripValue :: Type -> Value -> Property
prop_roundtripValue ty val = val === toValue (repType ty) (fromValue val)

prop_typecheckValue :: Type -> Value -> Property
prop_typecheckValue ty val = monadicIO $ do
  tc <- run $ checkType ty
  QuickCheck.assert $
    val == toValue (repType ty) (Bytecode.invokeTypechecker tc (fromValue val))

main :: IO ()
main = withUnitTest $ testRunner $ TestList
  [ TestLabel "value roundtrip" $ TestCase $ assertProperty "mismatch" $
      forAll arbitrary $ \ty ->
      forAll (valueFor ty) $ \val -> prop_roundtripValue ty val

  , TestLabel "value typecheck" $ TestCase $ assertProperty "mismatch" $
      forAll arbitrary $ \ty ->
      forAll (valueFor ty) $ \val -> prop_typecheckValue ty val

    -- test strings more thoroughly as they are quite complicated
  , TestLabel "string typecheck" $ TestCase $ assertProperty "mismatch" $
      forAll (valueFor T.StringTy) $ \val ->
        prop_typecheckValue T.StringTy val
  ]
