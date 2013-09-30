{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Yesod.Auth

getHomeR :: Handler Html
getHomeR = do
    mAuth <- maybeAuth
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To WriteMeAList.com!"
        toWidgetHead
            [hamlet|
                <meta name="google-site-verification"
                   content="hTMPzastDoFcR4-DLMM2jHogIhG5oWDRAZ4L-BVTWiU">
            |]

        $(widgetFile "homepage")

