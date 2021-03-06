module Handler.Lists where

import Import
import Yesod.Auth
import Data.Maybe
import System.Random (getStdRandom, randomR)
import qualified Data.Text as T

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
            catId2NameLookup <- runDB $ selectList [CategoryListId ==. listId] [Asc CategoryName] >>= (return . mapCatIdToName)
            (catwidget, catenctype) <- generateFormPost listCategoryCreateForm

            list <- runDB $ get404 listId
            listItems <- runDB $ selectList [ListItemList ==. listId]
                                            [Asc ListItemName, Asc ListItemCreatedAt]

            let itemsByCategory = groupItemsByCategory catId2NameLookup listItems
            itemsByCatWAdd <- mapM (\(catName, lis) -> generateFormPost (\x -> listItemCreateForm x Nothing "" (Just catName)) >>=
                                (\formRes -> return (catName, lis, formRes))) itemsByCategory

            listItemCDomId <- newIdent >>= (return . Just)
            (nocatWidg, nocatEnc) <- generateFormPost (\x -> listItemCreateForm x listItemCDomId "" (Just ""))

            defaultLayout $ do
                setTitleI $ MsgListTitle (listName list)
                $(widgetFile "topNav")
                $(widgetFile "list")
         Just (Entity userId _) -> do
             maybeListEditor <- runDB $ getBy $ UniqueListEditor listId userId
             case maybeListEditor of
                 Nothing -> do
                     _ <- runDB $ insert (ListEditor listId userId True True)
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

        catId2NameLookup <- runDB $ selectList [CategoryListId ==. listId] [] >>= (return . mapCatIdToName)
        let itemsByCategory = groupItemsByCategory catId2NameLookup listItems
        itemsByCatWAdd <- mapM (\(catName, lis) -> generateFormPost (\x -> listItemCreateForm x Nothing "" (Just catName)) >>=
                             (\formRes -> return (catName, lis, formRes))) itemsByCategory

        listItemCDomId <- newIdent >>= (return . Just)
        (nocatWidg, nocatEnc) <- generateFormPost (\x -> listItemCreateForm x listItemCDomId "" (Just ""))

        let createItemAllowed = True
        aDomId <- newIdent
        (catwidget, catenctype) <- generateFormPost listCategoryCreateForm
        defaultLayout $ do
            setTitleI $ MsgListTitle (listName list)
            $(widgetFile "topNav")
            $(widgetFile "list")

mapCatIdToName :: [Entity Category] -> [(CategoryId, Text)]
mapCatIdToName cats = zip (map entityKey cats) (map (categoryName . entityVal) cats)

mapCatNameToId :: [Entity Category] -> [(Text, CategoryId)]
mapCatNameToId cats = zip (map (categoryName . entityVal) cats) (map entityKey cats)

maybeCatNameToId :: [(Text, CategoryId)] -> Maybe Text -> Maybe CategoryId
maybeCatNameToId eCats mCatName = mCatName >>= flip lookup eCats

catNameToId :: [(Text, CategoryId)] -> Text -> Maybe CategoryId
catNameToId eCats catName = lookup catName eCats

listItemCategoryName :: [(CategoryId, Text)] -> ListItem -> Text
listItemCategoryName catToName li = fromMaybe "" (catIdToName catToName (listItemCatId li))
    where
        catIdToName :: [(CategoryId, Text)] -> Maybe CategoryId -> Maybe Text
        catIdToName eCats mCatId = mCatId >>= flip lookup eCats

groupItemsByCategory :: [(CategoryId, Text)] -> [Entity ListItem] -> [(Text, [Entity ListItem])]
groupItemsByCategory cats items = (T.pack "", filter (\(Entity _ li) -> Nothing == listItemCatId li) items) : categoryItems
    where
        categoryItems = map (\(catId, catName) -> (catName, filter (\(Entity _ li) -> Just catId == listItemCatId li) items)) cats

postListItemCreateR :: ListId -> Handler Html
postListItemCreateR listId = do
    -- TODO: Add check that this user owns this list
    Entity uId _ <- requireAuth
    let mAuth = Just uId
    aDomId <- newIdent
    categories <- runDB $ selectList [CategoryListId ==. listId] []
    let catId2NameLookup = mapCatIdToName categories
    ((res, widget), enctype) <- runFormPost (\x -> listItemEditForm x catId2NameLookup aDomId "" Nothing)

    case res of 
        FormSuccess (listItemText, mCatName) -> do
            let mCatId = maybeCatNameToId (mapCatNameToId categories) mCatName
            currTime <- lift getCurrentTime
            _ <- runDB $ insert (ListItem listItemText listId uId currTime Nothing Nothing mCatId currTime)
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
    categories <- runDB $ selectList [CategoryListId ==. listId] [] >>= (return .  mapCatIdToName)
    (widget, enctype) <- generateFormPost (\x -> listItemEditForm x categories aDomId "" Nothing)
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
    let catIdToNames = mapCatIdToName categories
    ((res, widget), enctype) <- runFormPost (\x -> listItemEditForm x catIdToNames aDomId "" Nothing)

    case res of 
        FormSuccess (itemName, mCatName) -> do
            currTime <- lift getCurrentTime
            let lookupIdFromName = mapCatNameToId categories
            _ <- runDB $ update listItemId [ListItemName =. itemName,
                                            ListItemCatId =. (maybeCatNameToId lookupIdFromName mCatName),
                                            ListItemModified =. currTime] 
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
    let catIdToNames = mapCatIdToName categories
    let catName = Just $ listItemCategoryName catIdToNames listItem

    aDomId <- newIdent

    (widget, enctype) <- generateFormPost (\x -> listItemEditForm x catIdToNames aDomId itemName catName)
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
                                  ListItemCompletedAt =. (Just currTime), 
                                  ListItemModified =. currTime] 
           else do
               update listItemId [ListItemCompletedBy =. Nothing,
                                  ListItemCompletedAt =. Nothing,
                                  ListItemModified =. currTime] 

    redirect $ ListR listId

listItemEditForm :: Html -> [(CategoryId, Text)] -> Text -> Text -> Maybe Text
                           -> MForm Handler (FormResult (Text, Maybe Text), Widget)
listItemEditForm extra catIdToNames aDomId entryName catName = do
    let cats = (\x -> zip x x) $ map snd catIdToNames 
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

listItemCreateForm :: Html -> Maybe Text -> Text -> Maybe Text
                           -> MForm Handler (FormResult (Text, Maybe Text), Widget)
listItemCreateForm extra mADomId entryName catName = do
    (itemRes, itemView) <- do
                                let label = (fieldSettingsLabel MsgListItemCreateLabel) {fsId = mADomId}
                                mreq textField label (Just entryName)
    (catRes, catView) <- mopt hiddenField "" (Just catName)

    let result = (,) <$> itemRes <*> catRes
    let widget = do
        toWidget 
              [lucius|
                    ##{fvId itemView} { width: 8em; }
              |]
        [whamlet|
                    #{extra}^{fvInput itemView}^{fvInput catView}
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
                                            , listEditorShowCategories = True
                                            , listEditorShowCompletedAfter = True
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

fixNoCategoryName :: Text -> Text
fixNoCategoryName t = if T.null t
                        then "Un-categorized items"
                        else t

postListItemCreateKnownCatR :: ListId -> Handler Html
postListItemCreateKnownCatR listId = do
    -- TODO: Add check that this user owns this list
    Entity uId _ <- requireAuth
    let mAuth = Just uId
    aDomId <- newIdent
    categories <- runDB $ selectList [CategoryListId ==. listId] []
    let catId2NameLookup = mapCatIdToName categories
    ((res, widget), enctype) <- runFormPost (\x -> listItemCreateForm x Nothing "" (Just ""))

    case res of 
        FormSuccess (listItemText, mCatName) -> do
            let mCatId = maybeCatNameToId (mapCatNameToId categories) mCatName
            currTime <- lift getCurrentTime
            _ <- runDB $ insert (ListItem listItemText listId uId currTime Nothing Nothing mCatId currTime)
            redirect $ ListR listId
        _ -> do 
            list <- runDB $ get404 listId
            defaultLayout $ do
                setTitleI MsgEnterListItemName
                $(widgetFile "topNav")
                $(widgetFile "listItemCreate")
