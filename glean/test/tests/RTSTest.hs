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
    [ pure T.Byte
    , pure T.Nat
    , pure T.String
    , T.Array <$> arbitrary
    , T.Record . fields <$> children 0
    , T.Sum . fields <$> children 1
    , T.Predicate <$> arbitrary
    , T.Maybe <$> arbitrary
    , sized $ \n -> do
        k <- choose (0, max 0 n)
        return $ T.Enumerated ["E" <> Text.pack (show i) | i <- [0 .. k]]
    , pure T.Boolean
    ]
    where
      children i = sized $ \n -> do
        k <- choose (i, max i n)
        vectorOf k $ resize (n `div` k) arbitrary

      fields tys =
        [ T.FieldDef (Text.pack $ 'x' : show (i :: Int)) ty
          | (i,ty) <- zip [0..] tys ]

valueFor :: Type -> Gen Value
valueFor T.Byte = Byte <$> arbitrary
valueFor T.Nat = Nat <$> arbitrary
valueFor T.String = String . Text.encodeUtf8 <$> arbitrary
valueFor (T.Array ty)
  | T.Byte <- derefType ty = ByteArray . BS.pack <$> arbitrary
  | otherwise = fmap Array $ sized $ \n -> do
      k <- choose (0,n)
      vectorOf k $ resize (n `div` k) $ valueFor ty
valueFor (T.Record fields) = Tuple <$> mapM (valueFor . T.fieldDefType) fields
valueFor (T.Sum fields) = do
  (i, field) <- elements $ zip [0..] fields
  Alt i <$> valueFor (T.fieldDefType field)
valueFor T.Predicate{} = Ref <$> arbitrary
valueFor (T.NamedType (ExpandedType _ ty)) = valueFor ty
valueFor (T.Maybe ty) = do
  b <- arbitrary
  if b then Alt 1 <$> valueFor ty else return $ Alt 0 $ Tuple []
valueFor (T.Enumerated names) = do
  i <- choose (0, fromIntegral (length names - 1))
  return $ Alt i $ Tuple []
valueFor T.Boolean = do
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
      forAll (valueFor T.String) $ \val -> prop_typecheckValue T.String val
  ]
