module Handler.Issue where

import Data.Maybe (fromJust)
import Import
import qualified GitHub as GH

data Points = One | Two | Three | Five | Eight | Thirteen
    deriving (Eq, Show, Ord)

pointsToInt :: Points -> Int
pointsToInt One = 1
pointsToInt Two = 2
pointsToInt Three = 3
pointsToInt Five = 5
pointsToInt Eight = 8
pointsToInt Thirteen = 13

data EstimateSubmission = EstimateSubmission
    { points :: Points
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

    (formWidget, enctype) <- generateFormPost estimateForm
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

estimateAForm :: AForm Handler EstimateSubmission
estimateAForm =
    let estimateField = selectFieldList
                            [ ("1" :: Text, One)
                            , ("2", Two)
                            , ("3", Three)
                            , ("5", Five)
                            , ("8", Eight)
                            , ("13", Thirteen)
                            ]
    in EstimateSubmission
        <$> areq estimateField "Estimate: " Nothing

estimateForm :: Html -> MForm Handler (FormResult EstimateSubmission, Widget)
estimateForm = renderDivs estimateAForm

-- TODO: This should really be PUT.
postIssueR :: Text -> Text -> Integer -> Handler Html
postIssueR ownerLogin name issueNumber = do
    ((result, formWidget), enctype) <- runFormPost estimateForm

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
            Just <$> upsert (Estimate
                { estimateIssueId = issueId
                , estimateUserId = userId
                , estimatePoints = pointsToInt $ points submission
                }) []
        _ -> do
            setMessage "Error in form submission"
            runDB $ do
                issueId <- generateIssueId repo issue
                getBy $ UniqueEstimate issueId userId

    defaultLayout $ do
        setTitle "ScrumBut | Issue"
        $(widgetFile "issue")
