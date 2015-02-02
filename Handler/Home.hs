module Handler.Home where

import Data.Maybe (fromJust)
import Import
import qualified GitHub as GH

getHomeR :: Handler Html
getHomeR = maybeAuthId >>= homeHandler

_repository :: GH.Repository -> Widget
_repository repo = $(widgetFile "_repository")

homeHandler :: Maybe UserId -> Handler Html
homeHandler Nothing =
    defaultLayout $ do
        setTitle "ScrumBut | Sign in"
        $(widgetFile "sign_in")

homeHandler (Just userId) = do
    user <- runDB $ get userId

    let token = userToken $ fromJust user
    client <- GH.newClient token

    userRepos <- GH.fetchRepos client

    orgs <- GH.fetchOrgs client
    orgRepos <- concat <$> mapM (GH.fetchOrgRepos client) orgs

    defaultLayout $ do
        setTitle "ScrumBut | Repositories"
        $(widgetFile "repositories")
