module Handler.Issue where

import Data.Maybe (fromJust)
import Import
import qualified GitHub as GH

data EstimateSubmission = EstimateSubmission
    { points :: Maybe Int
    } deriving Show

getIssueR :: Text -> Text -> Integer -> Handler Html
getIssueR ownerLogin name issueNumber = do
    userId <- requireAuthId
    user <- runDB $ get userId

    let token = userToken $ fromJust user
    client <- GH.newClient token

    repo <- GH.fetchRepo client ownerLogin name
    issue <- GH.fetchIssue client repo issueNumber

    currentEstimate <- runDB $ getEstimate (GH.issueId issue) userId

    (formWidget, enctype) <- generateFormPost $ estimateForm $ fmap entityVal currentEstimate
    defaultLayout $ do
        setTitle "ScrumBut | Issue"
        $(widgetFile "issue")

getEstimate :: (MonadIO m, backend ~ PersistEntityBackend Estimate) => Integer -> UserId -> ReaderT backend m (Maybe (Entity Estimate))
getEstimate githubIssueId userId = do
    maybeIssue <- getBy $ UniqueIssue $ show githubIssueId
    maybe (return Nothing) (\dbIssue -> getBy $ UniqueEstimate (entityKey dbIssue) userId) maybeIssue

estimateAForm :: Maybe Estimate -> AForm Handler EstimateSubmission
estimateAForm current =
    let estimateField = selectFieldList
                            [ ("" :: Text, Nothing)
                            , ("1", Just 1)
                            , ("2", Just 2)
                            , ("3", Just 3)
                            , ("5", Just 5)
                            , ("8", Just 8)
                            , ("13", Just 13)
                            ]
    in EstimateSubmission
        <$> areq estimateField "Your estimate: " (return $ fmap estimatePoints current)

estimateForm :: Maybe Estimate -> Html -> MForm Handler (FormResult EstimateSubmission, Widget)
estimateForm = renderDivs . estimateAForm

-- TODO: This should really be PUT.
postIssueR :: Text -> Text -> Integer -> Handler Html
postIssueR ownerLogin name issueNumber = do
    -- TODO: Reduce this duplication.
    userId <- requireAuthId
    user <- runDB $ get userId

    let token = userToken $ fromJust user
    client <- GH.newClient token

    repo <- GH.fetchRepo client ownerLogin name
    issue <- GH.fetchIssue client repo issueNumber

    originalEstimate <- runDB $ getEstimate (GH.issueId issue) userId
    ((result, formWidget), enctype) <- runFormPost $ estimateForm $ fmap entityVal originalEstimate

    case result of
        FormSuccess submission -> runDB $ do
            dbRepo <- upsert (Repository
                { repositoryGithubId = show $ GH.repoId repo
                , repositoryName = GH.repoName repo
                , repositoryOwnerLogin = GH.userLogin $ GH.repoOwner repo
                }) []

            dbIssue <- upsert (Issue
                { issueGithubId = show $ GH.issueId issue
                , issueNumber = show $ GH.issueNumber issue
                , issueRepositoryId = entityKey dbRepo
                }) []

            let issueId = entityKey dbIssue

            case points submission of
                Just p -> void $ upsert (Estimate
                    { estimateIssueId = issueId
                    , estimateUserId = userId
                    , estimatePoints = p
                    }) []

                Nothing -> deleteBy $ UniqueEstimate issueId userId
        _ -> setMessage "Error in form submission"

    defaultLayout $ do
        setTitle "ScrumBut | Issue"
        $(widgetFile "issue")
