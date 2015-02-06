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

    currentEstimate <- runDB $ do
        issueId <- generateIssueId repo issue
        getBy $ UniqueEstimate issueId userId

    (formWidget, enctype) <- generateFormPost $ estimateForm $ fmap entityVal currentEstimate
    defaultLayout $ do
        setTitle "ScrumBut | Issue"
        $(widgetFile "issue")

generateIssueId repo issue = do
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

    return $ entityKey dbIssue

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

estimateForm = renderDivs . estimateAForm

-- TODO: This should really be PUT.
postIssueR :: Text -> Text -> Integer -> Handler Html
postIssueR ownerLogin name issueNumber = do
    ((result, formWidget), enctype) <- runFormPost $ estimateForm Nothing

    -- TODO: Reduce this duplication.
    userId <- requireAuthId
    user <- runDB $ get userId

    let token = userToken $ fromJust user
    client <- GH.newClient token

    repo <- GH.fetchRepo client ownerLogin name
    issue <- GH.fetchIssue client repo issueNumber

    currentEstimate <- case result of
        FormSuccess submission -> runDB $ do
            issueId <- generateIssueId repo issue

            case points submission of
                Just p -> Just <$> upsert (Estimate
                    { estimateIssueId = issueId
                    , estimateUserId = userId
                    , estimatePoints = p
                    }) []

                Nothing -> do
                    deleteBy $ UniqueEstimate issueId userId
                    return Nothing
        _ -> do
            setMessage "Error in form submission"
            runDB $ do
                issueId <- generateIssueId repo issue
                getBy $ UniqueEstimate issueId userId

    defaultLayout $ do
        setTitle "ScrumBut | Issue"
        $(widgetFile "issue")
