module GitHub ( Client
              , newClient
              , fetchPath
              , fetchJSON
              , User(..)
              , Repository(..)
              , fetchRepos
              , fetchRepo
              , repoNWO
              , Organization(..)
              , fetchOrgs
              , fetchOrgRepos
              , IssueState(..)
              , Issue(..)
              , fetchIssues
              , fetchIssuesInMilestone
              , MilestoneState(..)
              , Milestone(..)
              , fetchMilestones
              , fetchMilestonesInState
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

instance Ord User where
    compare = compare `on` toCaseFold . userLogin

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

instance Ord Repository where
    compare = compare `on` toCaseFold . repoNWO

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

instance Ord Organization where
    compare = compare `on` toCaseFold . orgLogin

data IssueState = IssueOpen | IssueClosed
    deriving (Eq, Show, Ord)

instance FromJSON IssueState where
    parseJSON (String "open") = return IssueOpen
    parseJSON (String "closed") = return IssueClosed
    parseJSON _ = mzero

data Issue = Issue
    { issueId :: Integer
    , issueNumber :: Integer
    , issueTitle :: Text
    , issueBody :: Text
    , issueCreator :: User
    , issueAssignee :: Maybe User
    , issueState :: IssueState
    , issueHtmlUrl :: String
    , issueApiUrl :: String
    , issueMilestone :: Maybe Milestone
    } deriving (Eq, Show)

instance FromJSON Issue where
    parseJSON (Object v) = Issue <$>
                            v .: "id" <*>
                            v .: "number" <*>
                            v .: "title" <*>
                            v .:? "body" .!= "" <*>
                            v .: "user" <*>
                            v .:? "assignee" <*>
                            v .: "state" <*>
                            v .: "html_url" <*>
                            v .: "url" <*>
                            v .:? "milestone"
    parseJSON _ = mzero

instance Ord Issue where
    compare = compare `on` issueNumber

data MilestoneState = MilestoneOpen | MilestoneClosed
    deriving (Eq, Show, Ord)

instance FromJSON MilestoneState where
    parseJSON (String "open") = return MilestoneOpen
    parseJSON (String "closed") = return MilestoneClosed
    parseJSON _ = mzero

data Milestone = Milestone
    { milestoneId :: Integer
    , milestoneTitle :: Text
    , milestoneDescription :: Text
    , milestoneApiUrl :: String
    , milestoneCreator :: User
    , milestoneState :: MilestoneState
    } deriving (Eq, Show)

instance FromJSON Milestone where
    parseJSON (Object v) = Milestone <$>
                            v .: "number" <*>
                            v .: "title" <*>
                            v .:? "description" .!= "" <*>
                            v .: "url" <*>
                            v .: "creator" <*>
                            v .: "state"
    parseJSON _ = mzero

instance Ord Milestone where
    compare = compare `on` toCaseFold . milestoneTitle

-- The fully qualified name of a repository.
repoNWO :: Repository -> Text
repoNWO repo =
    let ownerLogin = userLogin $ repoOwner repo
    in ownerLogin ++ "/" ++ repoName repo

-- Creates a GitHub client with the given OAuth token.
newClient :: MonadIO m => Text -> m Client
newClient token = do
    manager <- liftIO $ newManager conduitManagerSettings
    return $ Client { getToken = token, getManager = manager }

-- Executes a GET request to the given relative path.
fetchPath :: MonadResource m => Client -> Text -> [Text] -> m (Response (ResumableSource m ByteString))
fetchPath client path args =
    let req = def
                { method = methodGet
                , secure = True
                , host = "api.github.com"
                , port = 443
                , path = encodeUtf8 path
                , queryString = encodeUtf8 $ intercalate "&" $ "per_page=100" : args
                , requestHeaders =
                    [ ("User-Agent", "ScrumBut")
                    , ("Authorization", "token " ++ encodeUtf8 (getToken client))
                    ]
                }
    in http req $ getManager client

-- Executes a GET request, and automatically deserializes the resulting JSON.
fetchJSON :: (MonadResource m, FromJSON a) => Client -> Text -> [Text] -> m a
fetchJSON client path args = do
    response <- fetchPath client path args
    value <- responseBody response $$+- sinkParser json

    case fromJSON value of
        Success a -> return a
        Error str -> fail str

-- Creates a path relative to the repos/ namespace.
repoRelativePath :: Repository -> Text -> Text
repoRelativePath repo subpath = "repos/" ++ repoNWO repo ++ "/" ++ subpath

-- Fetches repositories of the current user.
fetchRepos :: MonadResource m => Client -> m [Repository]
fetchRepos client = fetchJSON client "user/repos" []

-- Fetches a repository by NWO.
fetchRepo :: MonadResource m => Client -> Text -> Text -> m Repository
fetchRepo client ownerLogin name = fetchJSON client ("repos/" ++ ownerLogin ++ "/" ++ name) []

-- Fetches orgs that the current user is a member of.
fetchOrgs :: MonadResource m => Client -> m [Organization]
fetchOrgs client = fetchJSON client "user/orgs" []

-- Fetches repositories in the given org.
fetchOrgRepos :: MonadResource m => Client -> Organization -> m [Repository]
fetchOrgRepos client org = fetchJSON client ("orgs/" ++ orgLogin org ++ "/repos") []

-- Fetches issues in the given repository.
fetchIssues :: MonadResource m => Client -> Repository -> m [Issue]
fetchIssues client repo = fetchJSON client (repoRelativePath repo "issues") []

-- Fetches issues in the given repository and milestone.
fetchIssuesInMilestone :: MonadResource m => Client -> Repository -> Maybe Milestone -> m [Issue]
fetchIssuesInMilestone client repo milestone =
    let value = case milestone of
                (Just m) -> pack $ show $ milestoneId m
                Nothing -> "none"
    -- TODO: Factor out commonalities.
    in fetchJSON client (repoRelativePath repo "issues") [ "milestone=" ++ value ]

-- Fetches milestones in the given repository.
fetchMilestones :: MonadResource m => Client -> Repository -> m [Milestone]
fetchMilestones client repo = fetchJSON client (repoRelativePath repo "milestones") []

-- Fetches milestones in the given repository that have the specified state.
fetchMilestonesInState :: MonadResource m => Client -> Repository -> Maybe MilestoneState -> m [Milestone]
fetchMilestonesInState client repo state =
    let value = case state of
                (Just MilestoneOpen) -> "open"
                (Just MilestoneClosed) -> "closed"
                Nothing -> "all"
    -- TODO: Factor out commonalities.
    in fetchJSON client (repoRelativePath repo "milestones") [ "state=" ++ value ]
