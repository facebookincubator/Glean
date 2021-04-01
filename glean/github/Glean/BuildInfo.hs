module Glean.BuildInfo (module Glean.BuildInfo) where

import Data.Text (Text)

buildRule :: Text
buildRule = "glean"

buildMode :: Text
buildMode = "cabal"

buildTimeISO8601 :: Text
buildTimeISO8601 = "<time>"

buildRevision :: Text
buildRevision = "<unknown>"
