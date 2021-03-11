module Facebook.Fb303
  ( Fb303State(..)
  , newFb303
  , withFb303
  , fb303BaseHandler
  , fb303Handler
  ) where

import Data.Text (Text)
import Data.IORef

import qualified Util.TimeSec as Time

import Fb303Core.BaseService.Service
import Fb303Core.Types
import Fb303.FacebookService.Service
import Fb303.Types


data Fb303State = Fb303State
  { fb303_name :: Text
  , fb303_status :: IORef Fb_status
  , fb303_startTime :: Time.Time
  }

-- | Constructs all fb303 state given the name to be returned by getName
newFb303 :: Text -> IO Fb303State
newFb303 name = do
  currTime <- Time.now
  statusRef <- newIORef Fb303_status_DEAD
  return Fb303State
    { fb303_name = name
    , fb303_status = statusRef
    , fb303_startTime = currTime
    }

-- | 'with' equivalent of newFb303
withFb303 :: Text -> (Fb303State -> IO a) -> IO a
withFb303 name f = do
  fb303 <- newFb303 name
  f fb303

-- | Handler for fb303 core methods.
fb303BaseHandler :: Fb303State -> BaseServiceCommand a -> IO a
fb303BaseHandler Fb303State{..} GetName = return fb303_name
fb303BaseHandler Fb303State{..} GetStatus = readIORef fb303_status
fb303BaseHandler Fb303State{..} AliveSince = return $ fromIntegral t
  where
    Time.Time t = fb303_startTime

-- | Handler for all things fb303. All services should send `Fb303State`
-- commands through this handler
fb303Handler :: Fb303State -> FacebookServiceCommand a -> IO a
fb303Handler state@Fb303State{..} (SuperBaseService c) =
  fb303BaseHandler state c
