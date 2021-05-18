{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Glean.Shell.Types (
  Parse(..), Statement(..), JSONQuery(..), AngleQuery(..),
  ShellMode(..),
  SchemaQuery(..),
  Stats(..),
  ShellState(..),
  Eval(..),
  withBackend,
  getState,
  getRepo,
  setRepo,
  getMode,
  setMode,
) where

import Control.Concurrent
import Control.Exception
import qualified Control.Monad.Catch as C
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Monad.Trans.State.Strict as State
import Data.Int
import Data.Text.Prettyprint.Doc as Pretty
import qualified System.Console.Haskeline as Haskeline
import System.IO
import qualified Text.JSON as JSON
import qualified Text.Parsec as P
import qualified Text.Parsec.Language as P
import qualified Text.Parsec.Token as P

import Glean
import Glean.Schema.Resolve
import Glean.Types as Thrift
import Glean.Util.Some

data Statement pat
  = Command String String
  | Pattern pat
  | FactRef Bool Fid

type Parser = P.Parsec String ()

class Parse a where
  parse :: Parser a

instance Parse Fid where
  parse = (Fid . fromInteger) <$> P.braces lexer (P.natural lexer)

instance Parse pat => Parse (Statement pat) where
  parse = P.choice [command, P.try factref, ptrn]
    where
      command = split <$> (P.char ':' *> P.getInput <* P.setInput "")
      ptrn = Pattern <$> parse
      factref = FactRef
        <$> P.option False (P.char '!' *> pure True)
        <*> parse
      split s
        | (cmd,' ':arg) <- break (==' ') s = Command cmd arg
        | otherwise = Command s ""

data AngleQuery = AngleQuery
  { angleQueryRec :: Bool
  , angleQueryStored :: Bool
  , angleQuery :: String
  }

instance Parse AngleQuery where
  parse = AngleQuery
    <$> P.option False (P.char '!' *> pure True)
    <*> P.option False (P.char '*' *> pure True)
    <*> P.many P.anyChar

data JSONQuery = JSONQuery
  { jsonQueryPred :: String
  , jsonQueryRec :: Bool
  , jsonQueryStored :: Bool
  , jsonQuery :: String
  }

instance Parse JSONQuery where
  parse = JSONQuery
    <$> P.identifier lexer
    <*> P.option False (P.char '!' *> pure True)
    <*> P.option False (P.char '*' *> pure True)
    <*> P.many P.anyChar

lexer :: P.TokenParser st
lexer = P.makeTokenParser P.emptyDef
  { P.identLetter = P.alphaNum P.<|> P.oneOf "_." }


data ShellMode = ShellJSON | ShellAngle
  deriving Eq

data SchemaQuery = SchemaQuery
  { sqPredicate :: String
  , sqRecursive :: Bool
  , sqStored :: Bool
  , sqQuery :: String
  , sqCont :: Maybe Thrift.UserQueryCont
  , sqTransform :: Maybe (JSON.JSValue -> JSON.Result JSON.JSValue)
  , sqSyntax :: Thrift.QuerySyntax
  , sqOmitResults :: Bool
  }

data Stats = NoStats | SummaryStats | FullStats
  deriving Eq

data ShellState = ShellState
  { backend :: Some LocalOrRemote
  , repo :: Maybe Thrift.Repo
  , mode :: ShellMode
  , schemas :: Schemas
  , schemaInfo :: Thrift.SchemaInfo
  , limit :: Int64
  , timeout :: Maybe Int64
  , stats :: Stats
  , lastSchemaQuery :: Maybe SchemaQuery
  , updateSchema :: Maybe (Eval ())
  , isTTY :: Bool
  , pageWidth :: Maybe PageWidth
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

getRepo :: Eval (Maybe Thrift.Repo)
getRepo = repo <$> getState

setRepo :: Thrift.Repo -> Eval ()
setRepo r =
  withBackend $ \backend -> do
    schema <- liftIO $ getSchemaInfo backend r
    resolved <- either (liftIO . throwIO . ErrorCall) (return . snd) $
      parseAndResolveSchema (Thrift.schemaInfo_schema schema)
    Eval $ State.modify $ \s -> s
      { repo = Just r
      , schemaInfo = schema
      , schemas = resolved }

getMode :: Eval ShellMode
getMode = mode <$> getState

setMode :: ShellMode -> Eval ()
setMode m = Eval $ State.modify $ \s -> s{ mode = m }
