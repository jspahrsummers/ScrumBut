module Handler.Repositories where

import Import
import Data.Maybe (fromJust)
import GitHub

getRepositoriesR :: Handler Html
getRepositoriesR = do
    userId <- requireAuthId
    user <- runDB $ get userId

    let token = userToken $ fromJust user
    client <- newClient token
    repos <- fetchRepos client

    defaultLayout $ do
        setTitle "Welcome To Yesod!"
        $(widgetFile "repositories")
