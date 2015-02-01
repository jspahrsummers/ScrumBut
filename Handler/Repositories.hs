module Handler.Repositories where

import Import
import Data.Aeson
import Data.Maybe (fromJust)
import GitHub

getRepositoriesR :: Handler Html
getRepositoriesR = do
    userId <- requireAuthId
    user <- runDB $ get userId

    let token = userToken $ fromJust user
    client <- newClient token

    response <- responseBody <$> fetchPath client "user/repos"

    let repos = (decode' response :: Maybe Value)

    defaultLayout $ do
        setTitle "Welcome To Yesod!"
        $(widgetFile "repositories")
