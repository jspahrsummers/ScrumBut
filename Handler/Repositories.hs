{-# LANGUAGE OverloadedStrings #-}

module Handler.Repositories where

import Import
import Data.Aeson
import Data.Maybe (fromJust)
import Network.HTTP.Conduit

getRepositoriesR :: Handler Html
getRepositoriesR = do
    userId <- requireAuthId
    user <- runDB $ get userId

    let token = unpack $ userToken $ fromJust user

    response <- simpleHttp ("https://api.github.com/repos?access_token=" ++ token)

    let repos = (decode' response :: Maybe Value)

    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "repositories")
