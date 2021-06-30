-- Copyright (c) Facebook, Inc. and its affiliates.


module Glean.Util.URI
  ( getURI_S
  , getURI
  , relURI
  , diffusionAbs
  , diffusionURI
  , fbsDiffusionURI
  , fbsDiffusionURIForRange
  , escSafeU_S
  , escSafeU_T
  , qpS
  , qpT
  , uriQuerySet
  ) where

import Data.Char ( isAsciiUpper, isAsciiLower, isDigit )
import Data.List
import Data.Maybe
import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Text as T
import Network.URI

import Glean
import Glean.Schema.Src.Types as Src

-- | Applies normalization
getURI_S :: URI -> String
getURI_S uri = uriToString id uri ""

-- | Applies normalization
getURI :: URI -> Text
getURI = T.pack . getURI_S

-- | Escape and append to URI path (no special handing of / characters)
appendURI :: URI -> String -> URI
appendURI uri s = appendURI' uri (escapeURIString isUnescapedInURI s)

-- | Append to URI path (no special handing of / characters)
appendURI' :: URI -> String -> URI
appendURI' uri s = uri{ uriPath = uriPath uri <> s }

-- | Only the 'pathSegments' from the 'uriPath' of the second argument are used
glue :: URI -> URI -> URI
glue a b = slashURI' a (intercalate "/" (pathSegments b))

-- | Helper for use on static strings
absURI :: Text -> URI
absURI t = fromMaybe (error ("impossible absURI failed on " <> T.unpack t))
          (parseAbsoluteURI (T.unpack t))

-- | Helper for use on static strings
relURI :: Text -> URI
relURI t = fromMaybe (error ("impossible relURI failed on " <> T.unpack t))
          (parseRelativeReference (T.unpack t))

-- | Escape and append to URI path (ensure / character separator)
slashURI :: URI -> String -> URI
slashURI uri s = slashURI' uri (escapeURIString isUnescapedInURI s)

-- | Append to URI path (ensure / character separator)
slashURI' :: URI -> String -> URI
slashURI' uri s = uri{ uriPath = uriPath uri // s }
  where
    (//) [] [] = "/"
    (//) [] b@(_:_) = if head b == '/'
      then b
      else '/':b
    (//) a@(_:_) [] = if last a == '/'
      then a
      else a <> "/"
    (//) a@(_:_) b@(_:_) = case (last a == '/', head b == '/') of
      (True, True) -> a <> tail b
      (False, True) -> a <> b
      (True, False) -> a <> b
      (False, False) -> a <> ('/':b)

-- | Example: For line 14 add $14 to end of URL path
addLineURL :: (Show w, Enum w) => URI -> w -> URI
addLineURL uri line = appendURI uri ('$' : show (fromEnum line))

-- | This is the absolute root for oridinary phabricator server
diffusionAbs :: URI
diffusionAbs = absURI "https://phabricator.internmc.facebook.com/diffusion/"

-- | Used to build diffusion URIs, given a
--
--   * base diffusion URI (which would include host name and the first part
--     of the URI path which contains repo sign and branch)
--   * file path
--   * optional line number to highlight
diffusionURI :: URI -> Text -> Maybe Int32 -> URI
diffusionURI diffusion betterFile mLine =
  let url1 = diffusion `slashURI` T.unpack betterFile
      url2 = maybe url1 (addLineURL url1) mLine
  in diffusionAbs `glue` url2

-- | Returns the diffusion URI for an fbsource path and optional line number
fbsDiffusionURI :: Text -> Maybe Int32 -> URI
fbsDiffusionURI path mLine =
  diffusionURI (relURI "FBS/browse/master") path mLine

fbsDiffusionURIForRange :: Src.Range -> Maybe URI
fbsDiffusionURIForRange Src.Range
  { range_file = File { file_key = Just filepath }
  , range_lineBegin = Nat { unNat = lineBegin } } =
    Just $ fbsDiffusionURI filepath (Just $ fromIntegral lineBegin)
fbsDiffusionURIForRange _ = Nothing

-- -----------------------------------------------------------------------------
-- Safer escaping for uriQuerySet

-- | Make this only with 'escSafeU_S' and 'escSafeU_T' in 'qpS' and 'qpT'.
-- Uses 'isUnescapedInURIComponent' replacement 'myIsUnescaped' because
-- 'isReserved' was over 50% of the profiled time.
newtype SafeU = SafeU { safeU :: String } deriving (Eq, Ord, Show)

escSafeU_S :: String -> SafeU
escSafeU_S = SafeU . escapeURIString myIsUnescaped
  where
    myIsUnescaped :: Char -> Bool
    myIsUnescaped c
      | isAsciiUpper c || isAsciiLower c || isDigit c = True
      | otherwise = case c of
          '-' -> True
          '.' -> True
          '_' -> True
          '~' -> True
          _ -> False

escSafeU_T :: Text -> SafeU
escSafeU_T = escSafeU_S . T.unpack

-- | Make this only with 'qpS' and 'qpT'
data QueryParam = QueryParam { qp_name :: SafeU,  qp_val :: SafeU }
  deriving (Eq, Ord, Show)

qpS :: String -> String -> QueryParam
qpS name val = QueryParam{ qp_name = escSafeU_S name
                         , qp_val = escSafeU_S val }

qpT :: String -> Text -> QueryParam
qpT name val = QueryParam{ qp_name = escSafeU_S name
                         , qp_val = escSafeU_T val }

-- | The one true consumer of QueryParam
uriQuerySet :: URI -> [QueryParam] -> URI
uriQuerySet base [] = base{ uriQuery = "" }
uriQuerySet base qps =
    base{ uriQuery = "?" <> intercalate "&" (map fromQP qps) }
  where fromQP qp = safeU (qp_name qp) <> "=" <> safeU (qp_val qp)
