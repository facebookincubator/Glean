{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE DeriveTraversable #-}
module Glean.Shell.Types (
  Parse(..), Statement(..), JSONQuery(..), AngleQuery(..),
  SchemaQuery(..),
  Stats(..),
  ShellState(..),
  Eval(..),
  ExpandResults(..),
  withBackend,
  getState,
  getRepo,
  setRepo,
) where

import Control.Concurrent
import Control.Exception
import Data.Default
import Data.Bifunctor
import Data.Functor (($>))
import qualified Control.Monad.Catch as C
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Monad.Trans.State.Strict as State
import Data.Int
import Data.List
import qualified Data.Map as Map
import Data.Text.Prettyprint.Doc as Pretty
import qualified System.Console.Haskeline as Haskeline
import System.IO
import qualified Text.JSON as JSON
import qualified Text.Parsec as P
import qualified Text.Parsec.Language as P
import qualified Text.Parsec.Token as P

import Glean
import qualified Glean.Types as Thrift
import Glean.LocalOrRemote (LocalOrRemote)
import Glean.Database.Config (ProcessedSchema(..), processSchema)
import Glean.Schema.Util (SourceRef)
import Glean.Util.Some

data Statement pat
  = Command String String
  | Pattern pat
  | FactRef Fid

data ExpandResults
  = ExpandRecursive
  | ExpandPredicates [SourceRef]

type Parser = P.Parsec String ()

class Parse a where
  parse :: Parser a

instance Parse Fid where
  parse = Fid . fromInteger <$> P.braces lexer (P.natural lexer)

instance Parse pat => Parse (Statement pat) where
  parse = P.choice [command, P.try factref, ptrn]
    where
      command = split <$> (P.char ':' *> P.getInput <* P.setInput "")
      ptrn = Pattern <$>  parse
      factref = FactRef <$> parse
      split s
        | (cmd,' ':arg) <- break (==' ') s = Command cmd arg
        | otherwise = Command s ""

data AngleQuery = AngleQuery
  { angleQueryDeprecatedRec :: Bool
  , angleQueryStored :: Bool
  , angleQuery :: String
  }

instance Parse AngleQuery where
  parse = AngleQuery
    <$> P.option False (P.char '!' $> True)
    <*> P.option False (P.char '*' $> True)
    <*> P.many P.anyChar

data JSONQuery = JSONQuery
  { jsonQueryPred :: String
  , jsonQueryDeprecatedRec :: Bool
  , jsonQueryStored :: Bool
  , jsonQuery :: String
  }

instance Parse JSONQuery where
  parse = JSONQuery
    <$> P.identifier lexer
    <*> P.option False (P.char '!' $> True)
    <*> P.option False (P.char '*' $> True)
    <*> P.many P.anyChar

lexer :: P.TokenParser st
lexer = P.makeTokenParser P.emptyDef
  { P.identLetter = P.alphaNum P.<|> P.oneOf "_." }


data SchemaQuery = SchemaQuery
  { sqPredicate :: String
  , sqRecursive :: ExpandResults
  , sqStored :: Bool
  , sqQuery :: String
  , sqCont :: Maybe Thrift.UserQueryCont
  , sqTransform :: Maybe (JSON.JSValue -> JSON.Result JSON.JSValue)
  , sqSyntax :: Thrift.QuerySyntax
  , sqOmitResults :: Bool
  , sqContinue :: Bool
  }

data Stats = NoStats | SummaryStats | FullStats
  deriving Eq

data ShellState = ShellState
  { backend :: Some LocalOrRemote
  , repo :: Maybe Repo
  , schemas :: Maybe ProcessedSchema
  , schemaInfo :: Maybe Thrift.SchemaInfo
  , useSchemaId :: Thrift.SelectSchema
  , limit :: Int64
  , timeout :: Maybe Int64
  , stats :: Stats
  , lastSchemaQuery :: Maybe SchemaQuery
  , updateSchema :: Maybe (Eval ())
  , isTTY :: Bool
  , pageWidth :: Maybe PageWidth
  , expandResults :: ExpandResults
  , pager :: Bool
  , outputHandle :: MVar System.IO.Handle
  , debug :: Thrift.QueryDebugOptions
  , client_info :: Thrift.UserQueryClientInfo
  , query_file :: FilePath
  }

newtype Eval a = Eval
  { unEval :: State.StateT ShellState IO a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , Haskeline.MonadException
    , MonadIO
    , C.MonadCatch
    , C.MonadMask
    , C.MonadThrow )

withBackend :: (forall b . LocalOrRemote b => b -> Eval a) -> Eval a
withBackend f = do
  state <- getState
  case backend state of
    Some b -> f b

getState :: Eval ShellState
getState = Eval State.get

getRepo :: Eval (Maybe Repo)
getRepo = repo <$> getState

setRepo :: Repo -> Eval ()
setRepo r = do
  sel <- useSchemaId <$> getState
  withBackend $ \backend -> do
    info@SchemaInfo{..} <- liftIO $
      getSchemaInfo backend (Just r) def { getSchemaInfo_select = sel }
    let sids = map (first Thrift.SchemaId) $ Map.toList $ case sel of
          Thrift.SelectSchema_current{} -> schemaInfo_schemaIds
          Thrift.SelectSchema_stored{} -> schemaInfo_dbSchemaIds
          Thrift.SelectSchema_schema_id id
            | Map.member (unSchemaId id) schemaInfo_schemaIds ->
              schemaInfo_schemaIds
            | Map.member (unSchemaId id) schemaInfo_dbSchemaIds ->
              schemaInfo_dbSchemaIds
            | Just ids <- find (unSchemaId id `Map.member`)
              schemaInfo_otherSchemaIds -> ids
          _otherwise -> Map.empty
    Eval $ State.modify $ \s -> s
      { repo = Just r
      , schemaInfo = Just info }
    proc <- either (liftIO . throwIO . ErrorCall) return $
      processSchema (Map.fromList sids) schemaInfo_schema
    Eval $ State.modify $ \s -> s { schemas = Just proc }
