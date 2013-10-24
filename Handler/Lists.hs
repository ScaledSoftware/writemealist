module Handler.Lists where

import Import
import Yesod.Auth
import Data.Maybe
import System.Random (getStdRandom, randomR)
import Data.List (sortBy)

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
            categories <- runDB $ selectList [CategoryListId ==. listId] []
            (widget, enctype) <- generateFormPost (\x -> listItemCreateForm x categories aDomId "" Nothing)
            (catwidget, catenctype) <- generateFormPost listCategoryCreateForm
            list <- runDB $ get404 listId
            listItems <- runDB $ selectList [ListItemList ==. listId]
                                            [Asc ListItemName, Asc ListItemCreatedAt]
            let itemsByCategory = groupItemsByCategory categories listItems

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
                                        [Asc ListItemName, 
                                         Asc ListItemCreatedAt]

        categories <- runDB $ selectList [CategoryListId ==. listId] []
        let itemsByCategory = groupItemsByCategory categories listItems

        let createItemAllowed = True
        aDomId <- newIdent
        (widget, enctype) <- generateFormPost (\x -> listItemCreateForm x categories aDomId "" Nothing)
        (catwidget, catenctype) <- generateFormPost listCategoryCreateForm
        defaultLayout $ do
            setTitleI $ MsgListTitle (listName list)
            $(widgetFile "topNav")
            $(widgetFile "list")

catIdToNameLookup :: [Entity Category] -> [(CategoryId, Text)]
catIdToNameLookup cats = zip (map entityKey cats) (map (categoryName . entityVal) cats)

catIdToName :: [Entity Category] -> CategoryId -> Maybe Text
catIdToName [] _ = Nothing
catIdToName ((Entity catId cat):cats) cId =
    if catId == cId
        then Just $ categoryName cat
        else catIdToName cats cId

listItemCategoryName :: [(CategoryId, Text)] -> ListItem -> Text
listItemCategoryName cat2Name li =
        if isNothing lookupId 
            then ""
            else fromMaybe "" $ (flip lookup) cat2Name (fromJust lookupId)
    where
        lookupId = listItemCatId li

groupItemsByCategory :: [Entity Category] -> [Entity ListItem] -> [(Text, [Entity ListItem])]
groupItemsByCategory cats itemList = groupItemsByCategory' catItem
  where
    catIdNames = catIdToNameLookup cats
    catItem = sortBy (\(c1, _) (c2, _) -> compare c1 c2) $ 
                map (\li -> (listItemCategoryName catIdNames $ entityVal li, li)) itemList

    groupItemsByCategory' :: [(Text, Entity ListItem)] -> [(Text, [Entity ListItem])]
    groupItemsByCategory' [] = []
    groupItemsByCategory' list@((catName,_):_) =
                (catName, map snd thisCatList) : groupItemsByCategory' otherCatsList
      where
        (thisCatList, otherCatsList) = span (\(name, _) -> name == catName) list

postListItemCreateR :: ListId -> Handler Html
postListItemCreateR listId = do
    -- TODO: Add check that this user owns this list
    Entity uId _ <- requireAuth
    let mAuth = Just uId
    aDomId <- newIdent
    categories <- runDB $ selectList [CategoryListId ==. listId] []
    ((res, widget), enctype) <- runFormPost (\x -> listItemCreateForm x categories aDomId "" Nothing)

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
    categories <- runDB $ selectList [CategoryListId ==. listId] []
    (widget, enctype) <- generateFormPost (\x -> listItemCreateForm x categories aDomId "" Nothing)
    list <- runDB $ get404 listId
    defaultLayout $ do
        setTitleI $ MsgAllListsTitle
        $(widgetFile "listItemCreate")

postListItemEditR :: ListId -> ListItemId -> Handler Html
postListItemEditR listId listItemId = do
    -- TODO: Add check that this user owns this list
    Entity uId _ <- requireAuth
    aDomId <- newIdent
    categories <- runDB $ selectList [CategoryListId ==. listId] []
    ((res, widget), enctype) <- runFormPost (\x -> listItemCreateForm x categories aDomId "" Nothing)

    case res of 
        FormSuccess (itemName, category) -> do
            _ <- runDB $ update listItemId [ListItemName =. itemName, ListItemCatId =. category] 
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
    categories <- runDB $ selectList [CategoryListId ==. listId] []
    let category = listItemCatId listItem

    aDomId <- newIdent

    (widget, enctype) <- generateFormPost (\x -> listItemCreateForm x categories aDomId itemName category)
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

listItemCreateForm :: Html -> [Category] -> Text -> Text -> Maybe CategoryId 
                           -> MForm Handler (FormResult (Text, Maybe CategoryId), Widget)
listItemCreateForm extra categories aDomId entryName catName = do
    let cats = zip (map (categoryName . entityVal) categories) (map entityKey categories)
    (itemRes, itemView) <- mreq textField ((fieldSettingsLabel MsgListItemCreateLabel) {fsId = Just aDomId}) (Just entryName)
    (catRes, catView) <- mopt (selectFieldList cats) (fieldSettingsLabel MsgListItemCategoryLabel) (Just catName)

    let result = (,) <$> itemRes <*> catRes
    let widget = do
        toWidget 
              [lucius|
                    ##{fvId itemView} { width: 8em; }
                    ##{fvId catView} { width: 10em; }
              |]
        [whamlet|
                    #{extra}^{fvInput itemView} &nbsp ^{fvInput catView}
        |]

    return (result, widget)

listCategoryCreateForm :: Html -> MForm Handler (FormResult (Maybe Text), Widget)
listCategoryCreateForm extra = do
    (catName, catView) <- mreq textField "" Nothing

    let result = Just <$> catName
    let widget = do
        toWidget 
              [lucius|
                    ##{fvId catView} { width: 10em; }
              |]
        [whamlet|
                    #{extra} ^{fvInput catView}
        |]

    return (result, widget)

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

postListCategoryCreateR :: ListId -> Handler Html
postListCategoryCreateR listId = do
    Entity _ _ <- requireAuth
    ((res, _), _) <- runFormPost listCategoryCreateForm

    case res of 
        FormSuccess (Just catName) -> do
            categories <- runDB (selectList [CategoryListId ==. listId] []) >>= return . map (categoryName . entityVal)
            if catName `elem` categories
                then redirect $ ListR listId
                else do
                    _ <- runDB $ insert $ Category catName 0 listId
                    redirect $ ListR listId

        _ -> do
            -- oh well
            redirect $ ListR listId
