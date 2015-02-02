module Handler.Repositories where

import Import
import Data.Maybe (fromJust)
import qualified GitHub as GH

_repository :: GH.Repository -> Widget
_repository repo = $(widgetFile "_repository")

getRepositoriesR :: Handler Html
getRepositoriesR = do
    userId <- requireAuthId
    user <- runDB $ get userId

    let token = userToken $ fromJust user
    client <- GH.newClient token

    userRepos <- GH.fetchRepos client

    orgs <- GH.fetchOrgs client
    orgRepos <- concat <$> mapM (GH.fetchOrgRepos client) orgs

    defaultLayout $ do
        setTitle "Repositories"
        $(widgetFile "repositories")
