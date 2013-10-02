module Handler.Lists where

import Import
import Yesod.Auth
import Data.Maybe (isJust, isNothing)
import System.Random (getStdRandom, randomR)

getSharedListShow :: ListId -> Int -> Handler Html
getSharedListShow listId randKey = do
    shareUrl <- runDB $ selectList [ListViewerListId  ==. listId,
                                    ListViewerRandKey  ==. randKey] []

    if null shareUrl
        then do
            setMessageI MsgListAccessDenied
            redirect ListsR
        else do-- show the user the list.
            mAuth <- maybeAuth
            case mAuth of
                Nothing -> do
                    aDomId <- newIdent
                    (entrywidget, enctype) <- generateFormPost (listItemCreateForm aDomId listId Nothing)
                    list <- runDB $ get404 listId
                    listItems <- runDB $ selectList [ListItemList ==. listId]
                                                    [Asc ListItemCompletedAt, 
                                                     Asc ListItemCreatedAt]
                    defaultLayout $ do
                        setTitleI $ MsgListTitle (listName list)
                        $(widgetFile "topNav")
                        $(widgetFile "listItemCreate")
                        $(widgetFile "list")
                Just (Entity userId _) -> do
                    maybeListEditor <- runDB $ getBy $ UniqueListEditor listId userId
                    case maybeListEditor of
                        Nothing -> do
                            _ <- runDB $ insert (ListEditor listId userId)
                            redirect $ ListR listId
                        _ -> do
                            redirect $ ListR listId


postSharedListCreate :: ListId -> Handler Html
postSharedListCreate listId = do
    Entity uId _ <- requireAuth
    randKey <- lift $ getStdRandom (randomR (100000, 536870910 :: Int)) -- the :: Int keeps the compiler from crashing
    _ <- runDB $ insert $ ListViewer listId uId randKey
    redirect $ ListR listId

getListsR :: Handler Html
getListsR = do
    Entity uId user <- requireAuth
    let mAuth = Just user
    listsIView <- runDB $ selectList ([ListEditorViewer ==. uId]) []
    let myListIds = map (\(Entity _ listEd) -> listEditorList listEd) listsIView
    myLists <- runDB $ selectList [ListId <-. myListIds] [Asc ListName]

    let listIdNamePairs = map (\(Entity listId list) -> (listId, listName list)) myLists

    (entrywidget, enctype) <- generateFormPost (listCreateForm uId)
    defaultLayout $ do
        setTitleI $ MsgAllListsTitle
        $(widgetFile "topNav")
        $(widgetFile "listLists")
        $(widgetFile "createList")


getListR :: ListId -> Handler Html
getListR listId = do
    Entity uId _ <- requireAuth

    allowedLists <- runDB $ selectList [ListEditorViewer ==. uId, 
                                        ListEditorList ==. listId] []

    shareUrl <- runDB $ selectList [ListViewerListId  ==. listId,
                                    ListViewerSharingUserId  ==. uId] []
    
    if null allowedLists
        then do -- user can't view this list.
            setMessageI MsgListAccessDenied
            redirect ListsR
        else do-- show the user the list.
            mAuth <- maybeAuth
            list <- runDB $ get404 listId
            listItems <- runDB $ selectList [ListItemList ==. listId]
                                            [Asc ListItemCompletedAt, 
                                             Asc ListItemCreatedAt]
            let createItemAllowed = True
            aDomId <- newIdent
            (entrywidget, enctype) <- generateFormPost (listItemCreateForm aDomId listId Nothing)
            defaultLayout $ do
                setTitleI $ MsgListTitle (listName list)
                $(widgetFile "topNav")
                $(widgetFile "listItemCreate")
                $(widgetFile "list")

postListItemCreateR :: ListId -> Handler Html
postListItemCreateR listId = do
    Entity uId _ <- requireAuth
    let mAuth = Just uId
    aDomId <- newIdent
    ((res, entrywidget), enctype) <- runFormPost (listItemCreateForm aDomId listId Nothing)
    -- TODO: Add check that this user owns this list

    case res of 
        FormSuccess (_, listItemText) -> do
            currTime <- lift getCurrentTime
            _ <- runDB $ insert (ListItem listItemText listId uId currTime Nothing Nothing)
            redirect $ ListR listId
        _ -> do 
            list <- runDB $ get404 listId
            defaultLayout $ do
                setTitleI MsgEnterListItemName
                $(widgetFile "topNav")
                $(widgetFile "listItemCreate")


getListItemCreateR :: ListId -> Handler Html
getListItemCreateR listId = do
    aDomId <- newIdent
    (entrywidget, enctype) <- generateFormPost (listItemCreateForm aDomId listId Nothing)
    list <- runDB $ get404 listId
    defaultLayout $ do
        setTitleI $ MsgAllListsTitle
        $(widgetFile "listItemCreate")

postListItemEditR :: ListId -> ListItemId -> Handler Html
postListItemEditR listId listItemId = do
    Entity uId _ <- requireAuth
    aDomId <- newIdent
    ((res, entrywidget), enctype) <- runFormPost (listItemEditForm aDomId listId Nothing)
    -- TODO: Add check that this user owns this list

    case res of 
        FormSuccess (_, listItemText) -> do
            runDB $ update listItemId [ListItemName =. listItemText] 
            redirect $ ListR listId
        _ -> do 
            list <- runDB $ get404 listId
            mAuth <- maybeAuth

            defaultLayout $ do
                setTitleI MsgEnterListItemName
                $(widgetFile "topNav")
                $(widgetFile "listItemEdit")

getListItemEditR :: ListId -> ListItemId -> Handler Html
getListItemEditR listId listItemId = do
    listItem <- runDB $ get404 listItemId
    aDomId <- newIdent
    (entrywidget, enctype) <- generateFormPost (listItemEditForm aDomId listId (Just $ listItemName listItem))
    list <- runDB $ get404 listId
    mAuth <- maybeAuth

    defaultLayout $ do
        setTitleI MsgEnterListItemName
        $(widgetFile "topNav")
        $(widgetFile "listItemEdit")


postListItemDeleteR :: ListId -> ListItemId -> Handler Html
postListItemDeleteR listId listItemId = do
    runDB $ delete listItemId
    redirect $ ListR listId

postListItemCompleteR :: ListId -> ListItemId -> Handler Html
postListItemCompleteR listId listItemId = do
    Entity uId _ <- requireAuth
    currTime <- lift getCurrentTime

    runDB $ do
        listItem <- get404 listItemId
        if isNothing (listItemCompletedAt listItem)
           then do
               update listItemId [ListItemCompletedBy =. (Just uId)] 
               update listItemId [ListItemCompletedAt =. (Just currTime)] 
           else do
               update listItemId [ListItemCompletedBy =. Nothing] 
               update listItemId [ListItemCompletedAt =. Nothing] 

    redirect $ ListR listId


listItemEditForm :: Text -> ListId -> Maybe Text -> Form (ListId, Text)
listItemEditForm aDomId listId entryName = renderTable $ (,)
    <$> pure listId
    <*> areq textField ((fieldSettingsLabel MsgListItemEditLabel) {fsId = Just aDomId}) entryName

listItemCreateForm :: Text -> ListId -> Maybe Text -> Form (ListId, Text)
listItemCreateForm aDomId listId entryName = renderTable $ (,)
    <$> pure listId
    <*> areq textField ((fieldSettingsLabel MsgListItemCreateLabel) {fsId = Just aDomId}) entryName
--  <*> areq textField "" entryName


listCreateForm :: UserId -> Form List
listCreateForm uId = renderTable $ List
    <$> areq textField (fieldSettingsLabel MsgListNameLabel) Nothing
    <*> pure uId


postCreateListR :: Handler Html
postCreateListR = do
    Entity uId _ <- requireAuth
    ((res, entrywidget), enctype) <- runFormPost (listCreateForm uId)
    case res of 
        FormSuccess list -> do
            listId <- runDB $ insert (list {listCreatedBy = uId})
            _ <- runDB $ insert (ListEditor { listEditorList = listId
                                            , listEditorViewer = uId
                                            })
            redirect $ ListR listId
        _ -> defaultLayout $ do
            setTitleI MsgEnterListName
            $(widgetFile "createList")

    
