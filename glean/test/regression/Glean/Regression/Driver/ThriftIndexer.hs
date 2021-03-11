module Glean.Regression.Driver.ThriftIndexer (main) where

import qualified Glean.ThriftIndexer.Test as ThriftIndexer
import Glean.Regression.Test

main :: IO ()
main = testMain ThriftIndexer.driver
