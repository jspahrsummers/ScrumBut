module Handler.Repositories where

import Import
import Data.Aeson
import Data.Conduit.Attoparsec
import Data.Maybe (fromJust)
import GitHub

getRepositoriesR :: Handler Html
getRepositoriesR = do
    userId <- requireAuthId
    user <- runDB $ get userId

    let token = userToken $ fromJust user
    client <- newClient token

    response <- fetchPath client "user/repos"
    repos <- responseBody response $$+- sinkParser json

    defaultLayout $ do
        setTitle "Welcome To Yesod!"
        $(widgetFile "repositories")
