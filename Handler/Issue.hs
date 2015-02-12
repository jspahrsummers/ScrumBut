module Handler.Issue where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Import
import qualified GitHub as GH

data EstimateSubmission = EstimateSubmission
    { points :: Maybe Int
    } deriving Show

resolveRequestArguments :: Text -> Text -> Integer -> Handler (UserId, GH.Repository, GH.Issue)
resolveRequestArguments ownerLogin name issueNumber = do
    userId <- requireAuthId
    user <- runDB $ get userId

    let token = userToken $ fromJust user
    client <- GH.newClient token

    repo <- GH.fetchRepo client ownerLogin name
    issue <- GH.fetchIssue client repo issueNumber
    return (userId, repo, issue)

getIssueR :: Text -> Text -> Integer -> Handler Html
getIssueR ownerLogin name issueNumber = do
    (userId, _, issue) <- resolveRequestArguments ownerLogin name issueNumber

    (myEstimate, otherEstimates) <- runDB $ myEstimateAndOthers (GH.issueId issue) userId
    (formWidget, enctype) <- generateFormPost $ estimateForm myEstimate

    defaultLayout $ do
        setTitle "ScrumBut | Issue"
        $(widgetFile "issue")

estimatesForIssue :: (MonadIO m, backend ~ PersistEntityBackend Estimate) => Integer -> ReaderT backend m [Estimate]
estimatesForIssue githubIssueId = do
    maybeIssue <- getBy $ UniqueIssue $ show githubIssueId
    entities <- maybe (return []) (\dbIssue -> selectList [ EstimateIssueId ==. entityKey dbIssue ] []) maybeIssue
    return $ fmap entityVal entities

estimatesByUser :: [Estimate] -> Map.Map UserId Estimate
estimatesByUser estimates =
    let insertEstimate m estimate = Map.insert (estimateUserId estimate) estimate m
    in foldl' insertEstimate Map.empty estimates

lookupUser :: (MonadIO m, backend ~ PersistEntityBackend User) => UserId -> Estimate -> ReaderT backend m (Maybe (User, Estimate))
lookupUser userId estimate = do
    maybeUser <- get userId
    return $ fmap (, estimate) maybeUser

-- TODO: Don't fetch others' estimates/users if we don't have our own estimate.
myEstimateAndOthers :: (MonadIO m, backend ~ PersistEntityBackend Estimate) => Integer -> UserId -> ReaderT backend m (Maybe Estimate, [(User, Estimate)])
myEstimateAndOthers githubIssueId userId = do
    estimates <- liftM estimatesByUser $ estimatesForIssue githubIssueId
    maybeUsersAndEstimates <- mapM (uncurry lookupUser) $ Map.toList $ Map.delete userId estimates

    return (Map.lookup userId estimates, catMaybes maybeUsersAndEstimates)

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
    (userId, repo, issue) <- resolveRequestArguments ownerLogin name issueNumber

    (myEstimate, otherEstimates) <- runDB $ myEstimateAndOthers (GH.issueId issue) userId
    ((result, formWidget), enctype) <- runFormPost $ estimateForm myEstimate

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
