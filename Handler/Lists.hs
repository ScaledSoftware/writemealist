module Handler.Lists where

import Import
import Yesod.Auth
import Data.Maybe
import System.Random (getStdRandom, randomR)
import Data.List (sort, nub)

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
            (widget, enctype) <- generateFormPost (listItemCreateForm aDomId "" "")
            list <- runDB $ get404 listId
            listItems <- runDB $ selectList [ListItemList ==. listId]
                                            [Asc ListItemCategory, Asc ListItemName, Asc ListItemCreatedAt]
            let itemsByCategory = groupItemsByCategory listItems

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
                 _ -> redirect $ ListR listId

getListsR :: Handler Html
getListsR = do
    Entity uId user <- requireAuth
    let mAuth = Just user
    listsIView <- runDB $ selectList ([ListEditorViewer ==. uId]) []
    let myListIds = map (\(Entity _ listEd) -> listEditorList listEd) listsIView
    myLists <- runDB $ selectList [ListId <-. myListIds] [Asc ListName]

    let listIdNamePairs = map (\(Entity listId list) -> (listId, listName list)) myLists

    txtBoxDomId <- newIdent
    (widget, enctype) <- generateFormPost (listCreateForm uId txtBoxDomId)
    defaultLayout $ do
        setTitleI $ MsgAllListsTitle
        $(widgetFile "topNav")
        $(widgetFile "listLists")
        $(widgetFile "createList")


getListR :: ListId -> Handler Html
getListR listId = do
    Entity uId uAuth <- requireAuth

    allowedLists <- runDB $ selectList [ListEditorViewer ==. uId, 
                                        ListEditorList ==. listId] []

    existingUrl <- runDB $ selectList [ListViewerListId  ==. listId,
                                    ListViewerSharingUserId  ==. uId] []

    shareUrl <- if not $ null existingUrl
                 then return existingUrl
                 else do
                    randKey <- lift $ getStdRandom (randomR (100000, 1999999999 :: Int))
                    _ <- runDB $ insert $ ListViewer listId uId randKey
                    runDB $ selectList [ListViewerListId  ==. listId,
                                                    ListViewerSharingUserId  ==. uId] []
    
    if null allowedLists
     then do -- user can't view this list.
        setMessageI MsgListAccessDenied
        redirect ListsR
     else do-- show the user the list.
        let mAuth = Just uAuth
        list <- runDB $ get404 listId
        listItems <- runDB $ selectList [ListItemList ==. listId]
                                        [Asc ListItemCategory,
                                         Asc ListItemName, 
                                         Asc ListItemCreatedAt]

        let itemsByCategory = groupItemsByCategory listItems

        let createItemAllowed = True
        aDomId <- newIdent
        (widget, enctype) <- generateFormPost (listItemCreateForm aDomId "" "")
        defaultLayout $ do
            setTitleI $ MsgListTitle (listName list)
            $(widgetFile "topNav")
            $(widgetFile "list")

getCategoryFromEntity :: Entity ListItem -> Text
getCategoryFromEntity (Entity _ item) = listItemCategory item

getCategoriesFromList :: [Entity ListItem] -> [Text]
getCategoriesFromList list = nub $ sort $ map getCategoryFromEntity list

groupItemsByCategory :: [Entity ListItem] -> [(Text, [Entity ListItem])]
groupItemsByCategory itemList = groupItemsByCategory' (getCategoriesFromList itemList) itemList
  where
    groupItemsByCategory' :: [Text] -> [Entity ListItem] -> [(Text, [Entity ListItem])]
    groupItemsByCategory' [] list = [("", list)]
    groupItemsByCategory' (cat:cats) list = (cat,thisCatList) : groupItemsByCategory' cats otherCatsList
      where
        (thisCatList, otherCatsList) = span (\li -> getCategoryFromEntity li == cat) list

postListItemCreateR :: ListId -> Handler Html
postListItemCreateR listId = do
    -- TODO: Add check that this user owns this list
    Entity uId _ <- requireAuth
    let mAuth = Just uId
    aDomId <- newIdent
    ((res, widget), enctype) <- runFormPost (listItemCreateForm aDomId "" "")

    case res of 
        FormSuccess (listItemText, category) -> do
            currTime <- lift getCurrentTime
            _ <- runDB $ insert (ListItem listItemText listId uId currTime Nothing Nothing category)
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
    (widget, enctype) <- generateFormPost (listItemCreateForm aDomId "" "")
    list <- runDB $ get404 listId
    defaultLayout $ do
        setTitleI $ MsgAllListsTitle
        $(widgetFile "listItemCreate")

postListItemEditR :: ListId -> ListItemId -> Handler Html
postListItemEditR listId listItemId = do
    -- TODO: Add check that this user owns this list
    Entity uId _ <- requireAuth
    aDomId <- newIdent
    ((res, widget), enctype) <- runFormPost (listItemEditForm aDomId "" "")

    case res of 
        FormSuccess (itemName, category) -> do
            _ <- runDB $ update listItemId [ListItemName =. itemName, ListItemCategory =. category] 
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

    let itemName = listItemName listItem
    let category = listItemCategory listItem

    aDomId <- newIdent

    (widget, enctype) <- generateFormPost (listItemEditForm aDomId itemName category)
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
               update listItemId [ListItemCompletedBy =. (Just uId), 
                                  ListItemCompletedAt =. (Just currTime)] 
           else do
               update listItemId [ListItemCompletedBy =. Nothing, 
                                  ListItemCompletedAt =. Nothing] 

    redirect $ ListR listId


listItemEditForm :: Text -> Text -> Text -> Form (Text, Text)
listItemEditForm aDomId entryName catName = renderTable $ (,)
    <$> areq textField ((fieldSettingsLabel MsgListItemEditLabel) {fsId = Just aDomId}) (Just entryName)
    <*> areq textField (fieldSettingsLabel MsgListItemCategoryLabel) (Just catName)

listItemCreateForm :: Text -> Text -> Text -> Form (Text, Text)
listItemCreateForm aDomId entryName catName = renderTable $ (,)
    <$> areq textField ((fieldSettingsLabel MsgListItemCreateLabel) {fsId = Just aDomId}) (Just entryName)
    -- make next line optional
    <*> areq textField (fieldSettingsLabel MsgListItemCategoryLabel) (Just catName)

listCreateForm :: UserId -> Text -> Form List
listCreateForm uId txtBoxDomId = renderTable $ List
    <$> areq textField ((fieldSettingsLabel MsgListNameLabel) {fsId = Just txtBoxDomId}) Nothing
    <*> pure uId


postCreateListR :: Handler Html
postCreateListR = do
    Entity uId _ <- requireAuth
    txtBoxDomId <- newIdent
    ((res, widget), enctype) <- runFormPost (listCreateForm uId txtBoxDomId)
    case res of 
        FormSuccess list -> do
            listId <- runDB $ insert (list {listCreatedBy = uId})
            _ <- runDB $ insert (ListEditor { listEditorList = listId
                                            , listEditorViewer = uId
                                            })
            randKey <- lift $ getStdRandom (randomR (100000, 1999999999 :: Int))
            _ <- runDB $ insert $ ListViewer listId uId randKey
            redirect $ ListR listId
        _ -> defaultLayout $ do
            setTitleI MsgEnterListName
            $(widgetFile "createList")

    
