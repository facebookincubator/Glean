{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

-- | Format Angle errors to dispay in the terminal
module Glean.Shell.Error
  ( Ann
  , BadQuery(..)
  , prettyBadQuery
  ) where

import Control.Exception
import Control.Applicative ((<|>))
import Text.Read (readMaybe)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc as Pretty hiding ((<>), pageWidth)

import qualified Data.Text as Text
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty
import qualified Text.Parsec as Parsec

import Glean.Angle.Types

type Ann = Pretty.AnsiStyle

data BadQuery = BadQuery { angleSource :: Text, angleError :: Text }
  deriving Show

instance Exception BadQuery

prettyBadQuery :: BadQuery -> Doc Ann
prettyBadQuery (BadQuery source rawErr) =
    case parseSrcSpan rawErr of
      Nothing -> pretty rawErr
      Just (span, err) -> prettyErrorAt span source err

parseSrcSpan :: Text -> Maybe (SrcSpan, Text)
parseSrcSpan  e =
  let (s, description) = Text.drop 1 <$> Text.breakOn "\n" e

      number = do
        n <- Parsec.many Parsec.digit
        maybe (fail "no parse") return $ readMaybe n

      srcLoc = SrcLoc
        <$> (Parsec.string "line " *> number)
        <*> (Parsec.string ", column " *> number)

      srcSpan = do
        start <- srcLoc
        end <- (Parsec.string " - " *> srcLoc) <|> return start
        return $ SrcSpan start end
  in
  case Parsec.runParser srcSpan () "<input>" s of
    Left _ -> Nothing
    Right span -> Just (span, description)

-- | Number of lines to show before and after the target
-- source span to give more context to the error.
newtype Context = Context { unContext :: Int }

prettyErrorAt :: SrcSpan -> Text -> Text -> Doc Ann
prettyErrorAt span source err = vcat
  [ pretty err
  , prettySource
  ]
  where
    prettySource = vcat
      [ numberCol ctx span Nothing
      , relevantLines
      , numberCol ctx span Nothing <>
        if isSingleLine
          then underline span
          else mempty
      ]
    -- number of lines before and after the error to show
    ctx = Context $ if isSingleLine then 0 else 1
    isSingleLine    = sline span == eline span
    firstLineToShow = sline span - unContext ctx
    lastLineToShow  = eline span + unContext ctx
    shouldShow line = firstLineToShow <= line && line <= lastLineToShow
    relevantLines = vcat
      [ prettyLine ctx span n line
      | (n, line) <- zip [1..] (Text.lines source)
      , shouldShow n
      ]

prettyLine :: Context -> SrcSpan -> Int -> Text -> Doc Ann
prettyLine ctx span n line =
  numberCol ctx span (Just n)
  <> pretty before
  <> red (pretty within)
  <> pretty after
  where
    before
      | n == sline span = Text.take (scol span - 1) line
      | n < sline span = line
      | otherwise = mempty
    after
      | n == eline span = Text.drop (ecol span - 1) line
      | n > eline span = line
      | otherwise = mempty
    within
      = Text.dropEnd (Text.length after)
      $ Text.drop (Text.length before) line

-- | only to be used for single line SrcSpan
underline :: SrcSpan -> Doc Ann
underline span = pretty spaces <> red (pretty zigzag)
  where
    spaces = Text.replicate (scol span - 1) " "
    zigzag = Text.replicate (max 1 $ ecol span - scol span) "^"

numberCol :: Context -> SrcSpan -> Maybe Int -> Doc Ann
numberCol (Context ctx) span mline =
  pretty spaces <> pretty number <> divider <> indicator
  where
    hasContext = ctx > 0
    isSourceLine = isJust mline
    isProblemLine = case mline of
      Nothing -> False
      Just n -> sline span <= n && n <= eline span

    maxNumber = show (eline span)
    divider = if isSourceLine then " |" else "  "
    number = maybe "" show mline
    spaces = Text.replicate (length maxNumber - length number) " "
    indicator =
      if hasContext && isProblemLine
         then red "> "
         else "  " -- no context. No need to point out which lines have errors.

red :: Doc Ann -> Doc Ann
red = Pretty.annotate (Pretty.color Pretty.Red)

sline, eline, scol, ecol :: SrcSpan -> Int
sline (SrcSpan (SrcLoc l _) _) = l
eline (SrcSpan _ (SrcLoc l _)) = l
scol (SrcSpan (SrcLoc _ c) _) = c
ecol (SrcSpan _ (SrcLoc _ c)) = c
