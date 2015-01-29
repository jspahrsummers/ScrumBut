{-# LANGUAGE OverloadedStrings #-}

module GitHub ( Client
              , newClient
              , fetchPath
              ) where

import Data.Maybe (fromJust)
import Import

data Client = Client { getToken :: Text
                     , getManager :: Manager
                     }

-- Creates a GitHub client with the given OAuth token.
newClient :: MonadIO m => Text -> m Client
newClient token = do
    manager <- liftIO newManager
    return $ Client { getToken = token, getManager = manager }

-- Executes a GET request to the given relative path.
fetchPath client path = do
    -- FIXME: fromJust
    let req = fromJust $ parseUrl $ "https://api.github.com/" ++ path ++ "?access_token=" ++ (unpack $ getToken client)
        req' = req { requestHeaders = [ ("User-Agent", "ScrumBut") ] }

    httpLbs req'
