module Glean.Username (getUsername) where

import System.Posix.User

getUsername :: IO (Maybe String)
getUsername = do
    user <- getUserEntryForID =<< getRealUserID
    return $ Just $ userName user
