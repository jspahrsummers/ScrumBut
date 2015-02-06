module Handler.Issue where

import Data.Maybe (fromJust)
import Import
import qualified GitHub as GH

data Points = One | Two | Three | Five | Eight | Thirteen
    deriving (Eq, Show, Ord)

-- TODO: There's probably a prebuilt thing to do this.
pointsToInt :: Points -> Int
pointsToInt One = 1
pointsToInt Two = 2
pointsToInt Three = 3
pointsToInt Five = 5
pointsToInt Eight = 8
pointsToInt Thirteen = 13

intToPoints :: Int -> Maybe Points
intToPoints 1 = Just One
intToPoints 2 = Just Two
intToPoints 3 = Just Three
intToPoints 5 = Just Five
intToPoints 8 = Just Eight
intToPoints 13 = Just Thirteen
intToPoints _ = Nothing

data EstimateSubmission = EstimateSubmission
    { points :: Maybe Points
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
                            , ("1", Just One)
                            , ("2", Just Two)
                            , ("3", Just Three)
                            , ("5", Just Five)
                            , ("8", Just Eight)
                            , ("13", Just Thirteen)
                            ]
    in EstimateSubmission
        <$> areq estimateField "Estimate: " (return (fmap estimatePoints current >>= intToPoints))

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
                    , estimatePoints = pointsToInt p
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
