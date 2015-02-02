module Handler.Repo where

import Import
import Data.Maybe (fromJust)
import qualified GitHub as GH

getRepoR :: Text -> Text -> Handler Html
getRepoR ownerLogin name = do
    -- TODO: Factor all this out.
    userId <- requireAuthId
    user <- runDB $ get userId

    let token = userToken $ fromJust user
    client <- GH.newClient token

    repo <- GH.fetchRepo client ownerLogin name
    milestones <- GH.fetchMilestones client repo GH.AllStates

    defaultLayout $ do
        setTitle "Milestones"
        $(widgetFile "repository")
