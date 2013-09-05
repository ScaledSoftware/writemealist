module Handler.Lists where

import Import
import Yesod.Auth

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
            listItems <- runDB $ selectList [ListEntryList ==. listId]
                                            [Asc ListEntryCompletedAt, 
                                             Asc ListEntryCreatedAt]
            (entrywidget, enctype) <- generateFormPost (listEntryEditForm listId Nothing)
            defaultLayout $ do
                setTitleI $ MsgAllListsTitle
                $(widgetFile "list")
                $(widgetFile "listEntryCreate")

postListEntryCreateR :: ListId -> Handler Html
postListEntryCreateR listId = do
    Entity uId _ <- requireAuth
    ((res, entrywidget), enctype) <- runFormPost (listEntryEditForm listId Nothing)
    -- TODO: Add check that this user owns this list

    case res of 
        FormSuccess (_, listEntryText) -> do
            currTime <- lift getCurrentTime
            _ <- runDB $ insert (ListEntry listEntryText listId uId currTime Nothing Nothing)
            redirect $ ListR listId
        _ -> do 
            list <- runDB $ get404 listId
            defaultLayout $ do
                setTitleI MsgEnterListEntryName
                $(widgetFile "listEntryCreate")


getListEntryCreateR :: ListId -> Handler Html
getListEntryCreateR listId = do
    (entrywidget, enctype) <- generateFormPost (listEntryEditForm listId Nothing)
    list <- runDB $ get404 listId
    defaultLayout $ do
        setTitleI $ MsgAllListsTitle
        $(widgetFile "listEntryCreate")

postListEntryEditR :: ListId -> ListEntryId -> Handler Html
postListEntryEditR listId listEntryId = do
    Entity uId _ <- requireAuth
    ((res, entrywidget), enctype) <- runFormPost (listEntryEditForm listId Nothing)
    -- TODO: Add check that this user owns this list

    case res of 
        FormSuccess (_, listEntryText) -> do
            runDB $ update listEntryId [ListEntryItem =. listEntryText] 
            redirect $ ListR listId
        _ -> do 
            list <- runDB $ get404 listId

            defaultLayout $ do
                setTitleI MsgEnterListEntryName
                $(widgetFile "listEntryEdit")

getListEntryEditR :: ListId -> ListEntryId -> Handler Html
getListEntryEditR listId listEntryId = do
    listEntry <- runDB $ get404 listEntryId
    (entrywidget, enctype) <- generateFormPost (listEntryEditForm listId (Just $ listEntryItem listEntry))
    list <- runDB $ get404 listId

    defaultLayout $ do
        setTitleI MsgEnterListEntryName
        $(widgetFile "listEntryEdit")


postListEntryDeleteR :: ListId -> ListEntryId -> Handler Html
postListEntryDeleteR listId listEntryId = do
    runDB $ delete listEntryId
    redirect $ ListR listId


listEntryEditForm :: ListId -> Maybe Text -> Form (ListId, Text)
listEntryEditForm listId entryName = renderDivs $ (,)
    <$> pure listId
    <*> areq textField (fieldSettingsLabel MsgListEntryLabel) entryName


listCreateForm :: UserId -> Form List
listCreateForm uId = renderDivs $ List
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

    
