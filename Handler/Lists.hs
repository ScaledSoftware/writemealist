module Handler.Lists where

import Import
import Yesod.Auth
import Data.Maybe (isJust, isNothing)

getListsR :: Handler Html
getListsR = do
    Entity uId user <- requireAuth
    listsIView <- runDB $ selectList ([ListEditorViewer ==. uId]) []
    let myListIds = map (\(Entity _ listEd) -> listEditorList listEd) listsIView
    myLists <- runDB $ selectList [ListId <-. myListIds] [Asc ListName]

    let listIdNamePairs = map (\(Entity listId list) -> (listId, listName list)) myLists

    (entrywidget, enctype) <- generateFormPost (listCreateForm uId)
    defaultLayout $ do
        setTitleI $ MsgAllListsTitle
        $(widgetFile "listLists")
        $(widgetFile "createList")


getListR :: ListId -> Handler Html
getListR listId = do
    Entity uId user <- requireAuth

    allowedLists <- runDB $ selectList ([ListEditorViewer ==. uId, 
                                         ListEditorList ==. listId]) []
    if null allowedLists
        then do -- user can't view this list.
            setMessageI MsgListAccessDenied
            redirect ListsR
        else do-- show the user the list.
            list <- runDB $ get404 listId
            listItems <- runDB $ selectList [ListItemList ==. listId]
                                            [Asc ListItemCompletedAt, 
                                             Asc ListItemCreatedAt]
            (entrywidget, enctype) <- generateFormPost (listItemEditForm listId Nothing)
            defaultLayout $ do
                setTitleI $ MsgAllListsTitle
                $(widgetFile "list")
                $(widgetFile "listItemCreate")

postListItemCreateR :: ListId -> Handler Html
postListItemCreateR listId = do
    Entity uId _ <- requireAuth
    ((res, entrywidget), enctype) <- runFormPost (listItemEditForm listId Nothing)
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
                $(widgetFile "listItemCreate")


getListItemCreateR :: ListId -> Handler Html
getListItemCreateR listId = do
    (entrywidget, enctype) <- generateFormPost (listItemEditForm listId Nothing)
    list <- runDB $ get404 listId
    defaultLayout $ do
        setTitleI $ MsgAllListsTitle
        $(widgetFile "listItemCreate")

postListItemEditR :: ListId -> ListItemId -> Handler Html
postListItemEditR listId listItemId = do
    Entity uId _ <- requireAuth
    ((res, entrywidget), enctype) <- runFormPost (listItemEditForm listId Nothing)
    -- TODO: Add check that this user owns this list

    case res of 
        FormSuccess (_, listItemText) -> do
            runDB $ update listItemId [ListItemName =. listItemText] 
            redirect $ ListR listId
        _ -> do 
            list <- runDB $ get404 listId

            defaultLayout $ do
                setTitleI MsgEnterListItemName
                $(widgetFile "listItemEdit")

getListItemEditR :: ListId -> ListItemId -> Handler Html
getListItemEditR listId listItemId = do
    listItem <- runDB $ get404 listItemId
    (entrywidget, enctype) <- generateFormPost (listItemEditForm listId (Just $ listItemName listItem))
    list <- runDB $ get404 listId

    defaultLayout $ do
        setTitleI MsgEnterListItemName
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


listItemEditForm :: ListId -> Maybe Text -> Form (ListId, Text)
listItemEditForm listId entryName = renderTable $ (,)
    <$> pure listId
    <*> areq textField (fieldSettingsLabel MsgListItemLabel) entryName


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

    
