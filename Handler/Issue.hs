module Handler.Issue where

import Data.Maybe (fromJust)
import Import
import qualified GitHub as GH

getIssueR :: Text -> Text -> Integer -> Handler Html
getIssueR ownerLogin name issueNumber = do
    userId <- requireAuthId
    user <- runDB $ get userId

    let token = userToken $ fromJust user
    client <- GH.newClient token

    repo <- GH.fetchRepo client ownerLogin name
    issue <- GH.fetchIssue client repo issueNumber

    defaultLayout $ do
        setTitle "ScrumBut | Issue"
        $(widgetFile "issue")

putIssueR :: Text -> Text -> Integer -> Handler Html
putIssueR ownerLogin name issueId = error "Not yet implemented: putIssueR"
