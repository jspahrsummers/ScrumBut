module Handler.Milestone where

import Data.Maybe (fromJust)
import Import
import qualified GitHub as GH

getMilestoneR :: Text -> Text -> Integer -> Handler Html
getMilestoneR ownerLogin name milestoneId = do
    userId <- requireAuthId
    user <- runDB $ get userId

    let token = userToken $ fromJust user
    client <- GH.newClient token

    repo <- GH.fetchRepo client ownerLogin name
    milestone <- GH.fetchMilestone client repo milestoneId
    issues <- GH.fetchIssues client repo GH.AllStates $ GH.OnlyMilestone milestone

    defaultLayout $ do
        setTitle "ScrumBut | Milestone"
        $(widgetFile "milestone")
