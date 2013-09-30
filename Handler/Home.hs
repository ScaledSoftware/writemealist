{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Yesod.Auth

getHomeR :: Handler Html
getHomeR = do
    mAuth <- maybeAuth
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getHomeR" :: Text
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To WriteMeAList.com!"
        toWidgetHead
            [hamlet|
                <meta name="google-site-verification"
                   content="hTMPzastDoFcR4-DLMM2jHogIhG5oWDRAZ4L-BVTWiU">
            |]

        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
    mAuth <- maybeAuth
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postHomeR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing

    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To WriteMeAList.com!"
        $(widgetFile "homepage")

