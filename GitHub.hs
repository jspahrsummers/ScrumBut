module GitHub ( Client
              , newClient
              , fetchPath
              , fetchJSON
              , fetchRepos
              , User(..)
              , Repository(..)
              ) where

import ClassyPrelude
import Control.Monad.Trans.Resource
import Data.Aeson
import Data.Conduit
import Data.Conduit.Attoparsec
import Data.Default
import Network.HTTP.Conduit
import Network.HTTP.Types

data Client = Client
    { getToken :: Text
    , getManager :: Manager
    }

data User = User
    { userId :: Integer
    , userLogin :: Text
    } deriving (Eq, Show)

instance FromJSON User where
    parseJSON (Object v) = User <$>
                            v .: "id" <*>
                            v .: "login"
    parseJSON _ = mzero

data Repository = Repository
    { repoId :: Integer
    , repoOwner :: User
    , repoName :: Text
    , repoDescription :: Text
    , repoApiUrl :: String
    , repoHtmlUrl :: String
    } deriving (Eq, Show)

instance FromJSON Repository where
    parseJSON (Object v) = Repository <$>
                            v .: "id" <*>
                            v .: "owner" <*>
                            v .: "name" <*>
                            v .:? "description" .!= "" <*>
                            v .: "url" <*>
                            v .: "html_url"
    parseJSON _ = mzero

-- Creates a GitHub client with the given OAuth token.
newClient :: MonadIO m => Text -> m Client
newClient token = do
    manager <- liftIO $ newManager conduitManagerSettings
    return $ Client { getToken = token, getManager = manager }

-- Executes a GET request to the given relative path.
fetchPath :: MonadResource m => Text -> Client -> m (Response (ResumableSource m ByteString))
fetchPath path client =
    let req = def
                { method = methodGet
                , secure = True
                , host = "api.github.com"
                , path = encodeUtf8 path
                , queryString = "access_token=" ++ encodeUtf8 (getToken client)
                , requestHeaders = [ ("User-Agent", "ScrumBut") ]
                }
    in http req $ getManager client

-- Executes a GET request, and automatically deserializes the resulting JSON.
fetchJSON :: (MonadResource m, FromJSON a) => Text -> Client -> m a
fetchJSON path client = do
    response <- fetchPath path client
    value <- responseBody response $$+- sinkParser json

    case fromJSON value of
        Success a -> return a
        Error str -> fail str

-- Fetches repositories of the current user.
fetchRepos :: MonadResource m => Client -> m [Repository]
fetchRepos = fetchJSON "user/repos"
