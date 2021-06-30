-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE ConstraintKinds #-}
module Glean.Util.Some (Some(..)) where

data Some c where
  Some :: c a => a -> Some c
