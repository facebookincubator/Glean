-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE ApplicativeDo #-}
module Glean.Regression.Driver.External
  ( Ext(..)
  , extOptions
  , execExternal
  , main
  )
where

import Data.Default

import Glean.Init (withUnitTestOptions)
import Glean.Regression.Config
import Glean.Regression.Driver.ExternalLib
import Glean.Regression.Test

main :: IO ()
main = withUnitTestOptions (optionsWith extOptions) $ \(mkcfg,ext) -> do
  cfg <- mkcfg
  testAll cfg def
    { driverGenerator = execExternal ext
    , driverGroups = if null (extGroups ext) then [] else extGroups ext
    }
