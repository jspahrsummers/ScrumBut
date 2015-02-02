module Handler.Repositories where

import Import
import Data.Maybe (fromJust)
import GitHub

_repository :: Repository -> Widget
_repository repo = $(widgetFile "_repository")

getRepositoriesR :: Handler Html
getRepositoriesR = do
    userId <- requireAuthId
    user <- runDB $ get userId

    let token = userToken $ fromJust user
    client <- newClient token

    userRepos <- fetchRepos client

    orgs <- fetchOrgs client
    orgRepos <- concat <$> mapM (fetchOrgRepos client) orgs

    defaultLayout $ do
        setTitle "Welcome To Yesod!"
        $(widgetFile "repositories")
