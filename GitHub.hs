module GitHub ( Client
              , newClient
              , fetchPath
              , fetchJSON
              , User(..)
              , Repository(..)
              , fetchRepos
              , Organization(..)
              , fetchOrgs
              , fetchOrgRepos
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
    , userAvatarUrl :: String
    , userName :: Maybe Text
    , userHtmlUrl :: String
    } deriving (Eq, Show)

instance FromJSON User where
    parseJSON (Object v) = User <$>
                            v .: "id" <*>
                            v .: "login" <*>
                            v .: "avatar_url" <*>
                            v .:? "name" <*>
                            v .: "html_url"
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

data Organization = Organization
    { orgId :: Integer
    , orgLogin :: Text
    , orgDescription :: Text
    } deriving (Eq, Show)

instance FromJSON Organization where
    parseJSON (Object v) = Organization <$>
                            v .: "id" <*>
                            v .: "login" <*>
                            v .:? "description" .!= ""
    parseJSON _ = mzero

-- Creates a GitHub client with the given OAuth token.
newClient :: MonadIO m => Text -> m Client
newClient token = do
    manager <- liftIO $ newManager conduitManagerSettings
    return $ Client { getToken = token, getManager = manager }

-- Executes a GET request to the given relative path.
fetchPath :: MonadResource m => Client -> Text -> m (Response (ResumableSource m ByteString))
fetchPath client path =
    let req = def
                { method = methodGet
                , secure = True
                , host = "api.github.com"
                , port = 443
                , path = encodeUtf8 path
                , requestHeaders =
                    [ ("User-Agent", "ScrumBut")
                    , ("Authorization", "token " ++ encodeUtf8 (getToken client))
                    ]
                }
    in http req $ getManager client

-- Executes a GET request, and automatically deserializes the resulting JSON.
fetchJSON :: (MonadResource m, FromJSON a) => Client -> Text -> m a
fetchJSON client path = do
    response <- fetchPath client path
    value <- responseBody response $$+- sinkParser json

    case fromJSON value of
        Success a -> return a
        Error str -> fail str

-- Fetches repositories of the current user.
fetchRepos :: MonadResource m => Client -> m [Repository]
fetchRepos client = fetchJSON client "user/repos"

-- Fetches orgs that the current user is a member of.
fetchOrgs :: MonadResource m => Client -> m [Organization]
fetchOrgs client = fetchJSON client "user/orgs"

-- Fetches repositories in the given org.
fetchOrgRepos :: MonadResource m => Client -> Organization -> m [Repository]
fetchOrgRepos client org = fetchJSON client $ "orgs/" ++ orgLogin org ++ "/repos"
