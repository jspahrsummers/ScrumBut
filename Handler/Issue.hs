module Handler.Issue where

import Data.Maybe (fromJust)
import Import
import qualified GitHub as GH

data Points = One | Two | Three | Five | Eight | Thirteen
    deriving (Eq, Show, Ord)

data EstimateSubmission = EstimateSubmission
    { points :: Points
    } deriving Show

getIssueR :: Text -> Text -> Integer -> Handler Html
getIssueR ownerLogin name issueNumber = do
    userId <- requireAuthId
    user <- runDB $ get userId

    let token = userToken $ fromJust user
    client <- GH.newClient token

    repo <- GH.fetchRepo client ownerLogin name
    issue <- GH.fetchIssue client repo issueNumber

    (formWidget, enctype) <- generateFormPost estimateForm

    defaultLayout $ do
        setTitle "ScrumBut | Issue"
        $(widgetFile "issue")

estimateAForm :: AForm Handler EstimateSubmission
estimateAForm =
    let estimateField = selectFieldList
                            [ ("1" :: Text, One)
                            , ("2", Two)
                            , ("3", Three)
                            , ("5", Five)
                            , ("8", Eight)
                            , ("13", Thirteen)
                            ]
    in EstimateSubmission
        <$> areq estimateField "Estimate: " Nothing

estimateForm :: Html -> MForm Handler (FormResult EstimateSubmission, Widget)
estimateForm = renderDivs estimateAForm

-- TODO: This should really be PUT.
postIssueR :: Text -> Text -> Integer -> Handler Html
postIssueR ownerLogin name issueId = do
    ((result, widget), enctype) <- runFormPost estimateForm

    case result of
        FormSuccess submission -> return ()
        _ -> setMessage "Error in form submission"

    getIssueR ownerLogin name issueId
