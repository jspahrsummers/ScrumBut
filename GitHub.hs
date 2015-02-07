module GitHub ( Client
              , newClient
              , Request
              , request
              , httpRequest
              , fetch
              , fetchJSON
              , fetchSingle
              , fetchAll
              , User(..)
              , Repository(..)
              , fetchRepos
              , fetchRepo
              , repoNWO
              , Organization(..)
              , fetchOrgs
              , fetchOrgRepos
              , MilestoneFilter(..)
              , StateFilter(..)
              , IssueState(..)
              , Issue(..)
              , fetchIssue
              , fetchIssues
              , MilestoneState(..)
              , Milestone(..)
              , fetchMilestone
              , fetchMilestones
              ) where

import ClassyPrelude
import Control.Monad.Trans.Resource
import Data.Aeson
import Data.Attoparsec.ByteString.Char8
import Data.Conduit
import Data.Conduit.Attoparsec
import Data.Default
import qualified Network.HTTP.Conduit as HTTP
import Network.HTTP.Types

data Client = Client
    { getToken :: Text
    , getManager :: HTTP.Manager
    }

data Request = Request
    { path :: Text
    , parameters :: [Text]
    , client :: Client
    }

instance Show Request where
    show req = "Request \"" ++ show (path req) ++ "\" " ++ show (parameters req)

-- Creates a request to the given path.
request :: Client -> Text -> Request
request client path = Request
    { path = path
    , parameters = []
    , client = client
    }

-- Adds required GitHub headers to any request.
decorateHttpRequest :: Client -> HTTP.Request -> HTTP.Request
decorateHttpRequest client req =
    let token = getToken client
    in req
        { HTTP.requestHeaders =
            [ ("User-Agent", "ScrumBut")
            , ("Authorization", "token " ++ encodeUtf8 token)
            ]
        }

-- Creates a Conduit HTTP request for a GitHub resource.
httpRequest :: Request -> HTTP.Request
httpRequest req =
    let params = "per_page=100" : parameters req
    in decorateHttpRequest (client req) $ def
        { HTTP.method = methodGet
        , HTTP.secure = True
        , HTTP.host = "api.github.com"
        , HTTP.port = 443
        , HTTP.path = encodeUtf8 $ path req
        , HTTP.queryString = encodeUtf8 $ intercalate "&" params
        }

-- Sends a request and streams the results.
fetch :: MonadResource m => HTTP.Request -> Client -> m (HTTP.Response (ResumableSource m ByteString))
fetch req client = HTTP.http req $ getManager client

-- Sends a request, returning the response as a parsed JSON value.
fetchJSON :: (MonadResource m, FromJSON a) => HTTP.Request -> Client -> m (HTTP.Response a)
fetchJSON req client = do
    response <- fetch req client
    value <- HTTP.responseBody response $$+- sinkParser json

    case fromJSON value of
        Success a -> return $ fmap (const a) response
        Error str -> fail str

data Link = Link
    { linkRelation :: Text
    , linkUrl :: String
    } deriving (Eq, Show)

link :: Parser Link
link = do
    skipSpace

    _ <- char '<'
    url <- decodeUtf8 <$> takeTill ((==) '>')
    _ <- char '>'

    _ <- char ';'
    skipSpace

    _ <- string "rel=\""
    rel <- decodeUtf8 <$> takeTill ((==) '"')
    _ <- char '"'

    return $ Link { linkRelation = rel, linkUrl = unpack url }

links :: Parser [Link]
links = link `sepBy` (char ',')

nextPageUrl :: HTTP.Response body -> Maybe String
nextPageUrl response = do
    linkStrings <- lookup "Link" $ HTTP.responseHeaders response
    lnks <- either (const Nothing) Just $ parseOnly links linkStrings

    let isMatch = (==) "next" . linkRelation

    map linkUrl $ find isMatch lnks

-- Sends a request and returns the result as a single JSON value.
fetchSingle :: (MonadResource m, FromJSON a) => Request -> m a
fetchSingle req = HTTP.responseBody <$> fetchJSON (httpRequest req) (client req)

-- Fetches a page of results and all remaining pages thereafter.
fetchRemaining :: (MonadResource m, FromJSON a) => HTTP.Request -> Client -> m [a]
fetchRemaining req client = do
    response <- fetchJSON req client

    let values = HTTP.responseBody response
        nextUrl = nextPageUrl response >>= HTTP.parseUrl
        nextRequest = map (decorateHttpRequest client) nextUrl

    nextValues <- case nextRequest of
                    Just nextRequest' -> fetchRemaining nextRequest' client
                    Nothing -> return []

    return $ values ++ nextValues

-- Sends a request and returns all pages of results.
fetchAll :: (MonadResource m, FromJSON a) => Request -> m [a]
fetchAll req = fetchRemaining (httpRequest req) (client req)

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
    manager <- liftIO $ HTTP.newManager HTTP.conduitManagerSettings
    return $ Client { getToken = token, getManager = manager }

-- Creates a path relative to the repos/ namespace.
repoRelativePath :: Repository -> Text -> Text
repoRelativePath repo subpath = "repos/" ++ repoNWO repo ++ "/" ++ subpath

-- Fetches repositories of the current user.
fetchRepos :: MonadResource m => Client -> m [Repository]
fetchRepos client = fetchAll $ request client "user/repos"

-- Fetches a repository by NWO.
fetchRepo :: MonadResource m => Client -> Text -> Text -> m Repository
fetchRepo client ownerLogin name = fetchSingle $ request client $ "repos/" ++ ownerLogin ++ "/" ++ name

-- Fetches orgs that the current user is a member of.
fetchOrgs :: MonadResource m => Client -> m [Organization]
fetchOrgs client = fetchAll $ request client "user/orgs"

-- Fetches repositories in the given org.
fetchOrgRepos :: MonadResource m => Client -> Organization -> m [Repository]
fetchOrgRepos client org = fetchAll $ request client $ "orgs/" ++ orgLogin org ++ "/repos"

data MilestoneFilter
    = AllMilestones
    | OnlyMilestone Milestone
    | NoMilestones
    deriving Eq

instance Show MilestoneFilter where
    show AllMilestones = "milestone=*"
    show (OnlyMilestone m) = "milestone=" ++ (show $ milestoneId m)
    show NoMilestones = "milestone=none"

data StateFilter
    = OnlyOpen
    | OnlyClosed
    | AllStates
    deriving Eq

instance Show StateFilter where
    show OnlyOpen = "state=open"
    show OnlyClosed = "state=closed"
    show AllStates = "state=all"

-- Fetches a single issue from a repository.
fetchIssue :: MonadResource m => Client -> Repository -> Integer -> m Issue
fetchIssue client repo issueNumber = fetchSingle $ request client $ repoRelativePath repo $ "issues/" ++ pack (show issueNumber)

-- Fetches issues in the given repository.
fetchIssues :: MonadResource m => Client -> Repository -> StateFilter -> MilestoneFilter -> m [Issue]
fetchIssues client repo state milestone =
    let req = request client $ repoRelativePath repo "issues"
    in fetchAll $ req { parameters = [ pack $ show milestone, pack $ show state ] }

-- Fetches a single milestone from a repository.
fetchMilestone :: MonadResource m => Client -> Repository -> Integer -> m Milestone
fetchMilestone client repo milestoneId = fetchSingle $ request client $ repoRelativePath repo $ "milestones/" ++ pack (show milestoneId)

-- Fetches milestones in the given repository.
fetchMilestones :: MonadResource m => Client -> Repository -> StateFilter -> m [Milestone]
fetchMilestones client repo state =
    let req = request client $ repoRelativePath repo "milestones"
    in fetchAll $ req { parameters = [ pack $ show state ] }
