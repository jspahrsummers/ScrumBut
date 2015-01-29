{-# LANGUAGE OverloadedStrings #-}

module Handler.Repositories where

import Import
import Data.Aeson
import Data.Maybe (fromJust)
import GitHub
import Network.HTTP.Conduit

getRepositoriesR :: Handler Html
getRepositoriesR = do
    userId <- requireAuthId
    user <- runDB $ get userId

    let token = userToken $ fromJust user
    client <- newClient token

    response <- responseBody <$> fetchPath client "repos"

    let repos = (decode' response :: Maybe Value)

    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "repositories")
