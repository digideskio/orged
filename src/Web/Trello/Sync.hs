{-# LANGUAGE OverloadedStrings #-}

module Web.Trello.Sync
       ( CardId(..)
       , Card(..)
       , ListId(..)
       , List(..)
       , BoardId(..)
       , Board(..)
       , getUserBoardNames
       , getUserBoards
       , getOrgBoardNames
       , getOrgBoards
       , updateBoard
       )
       where

import           Prelude             hiding (concatMap, elem, find, mapM, mapM_)

import           Control.Applicative
import           Control.Lens
import           Control.Monad       (when)
import           Data.Aeson          (toJSON)
import           Data.Aeson.Lens
import           Data.Foldable
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Maybe          (catMaybes)
import           Data.Monoid
import           Data.Set            (Set)
import qualified Data.Set            as S
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as TE
import qualified Data.Text.IO        as T
import           Data.Traversable
import           Network.Wreq
import           System.Directory    (doesFileExist)
import           System.Environment


-- Core Data structures

newtype Creds = Creds { unCreds :: (Text, Text) } deriving (Eq, Show, Ord)

readCreds :: IO Creds
readCreds = do key <- T.pack <$> getEnv "TRELLO_DEVELOPER_PUBLIC_KEY"
               token <- T.pack <$> getEnv "TRELLO_MEMBER_TOKEN"
               return (Creds (key, token))

newtype CardId = CardId { unCardId :: Text } deriving (Eq, Show, Ord)
data Card = Card { cardId          :: CardId
                 , cardName        :: Text
                 , cardDescription :: Text
                 , cardCheckList   :: Maybe (Set Text)
                 , cardSubscribed  :: Bool
                 , cardLabels      :: [Text]
                 , cardUrl         :: Text
                 } deriving (Eq, Show, Ord)

newtype ListId = ListId { unListId :: Text } deriving (Eq, Show, Ord)
data List = List { listId    :: ListId
                 , listName  :: Text
                 , listCards :: Set Card
                 } deriving (Eq, Show, Ord)

newtype BoardId = BoardId { unBoardId :: Text } deriving (Eq, Show, Ord)
data Board = Board { boardId    :: BoardId
                   , boardName  :: Text
                   , boardLists :: Set List
                   } deriving (Eq, Show, Ord)


-- Public API functions

getUserBoardNames :: Text -> IO [Text]
getUserBoardNames userId =
  do creds <- readCreds
     r <- get' creds (userBoardsEndpoint userId)
     return $ map (\v -> (v ^?! key "name" . _String))
                  (filter notClosed (r ^.. responseBody . values))

getUserBoards :: Text -> IO [Board]
getUserBoards userId =
  do creds <- readCreds
     r <- get' creds (userBoardsEndpoint userId)
     mapM (\v ->
               do let boardId = BoardId (v ^?! key "id" . _String)
                  lists <- S.fromList <$> getLists creds boardId
                  return $ Board boardId
                                 (v ^?! key "name" . _String)
                                 lists
               )
               (filter notClosed (r ^.. responseBody . values))

getOrgBoardNames :: Text -> IO [Text]
getOrgBoardNames orgName =
  do creds <- readCreds
     r <- get' creds (groupEndpoint orgName)
     return $ map (\v -> (v ^?! key "name" . _String))
                  (filter notClosed (r ^.. responseBody . values))

getOrgBoards :: Text -> IO [Board]
getOrgBoards orgName =
  do creds <- readCreds
     r <- get' creds (groupEndpoint orgName)
     mapM (\v ->
               do let boardId = BoardId (v ^?! key "id" . _String)
                  lists <- S.fromList <$> getLists creds boardId
                  return $ Board boardId
                                 (v ^?! key "name" . _String)
                                 lists
               )
               (filter notClosed (r ^.. responseBody . values))

-- Diffing API

updateBoard :: Board -> Board -> IO ()
updateBoard old new =
  do creds <- readCreds
     let changes = diffLists (boardId old) (boardLists old) (boardLists new)
     mapM_ (effectChange creds) changes

-- HTTP Helpers

get' (Creds (k,t)) u =
  getWith (defaults & param "key" .~ [k] & param "token" .~ [t])
          (T.unpack u)
post' (Creds (k,t)) u args =
  postWith (defaults & params .~ (("key", k) : ("token", t) : args))
           (T.unpack u)
           (TE.encodeUtf8 "")
put' (Creds (k,t)) u args =
  putWith (defaults & params .~ (("key", k) : ("token", t) : args))
          (T.unpack u)
          (TE.encodeUtf8 "")
delete' (Creds (k,t)) u =
  deleteWith (defaults & params .~ (("key", k) : ("token", t) : []))
             (T.unpack u)


-- Endpoints
groupEndpoint :: Text -> Text
groupEndpoint orgName = "https://api.trello.com/1/organizations/" <> orgName <> "/boards"
boardListEndpoint :: BoardId -> Text
boardListEndpoint (BoardId i) = "https://api.trello.com/1/boards/" <> i <> "/lists"
listEndpoint :: ListId -> Text
listEndpoint (ListId i) = "https://api.trello.com/1/lists/" <> i
listCardsEndpoint :: ListId -> Text
listCardsEndpoint l = listEndpoint l  <> "/cards"
listClosedEndpoint :: ListId -> Text
listClosedEndpoint l = listEndpoint l  <> "/closed"
checklistEndpoint :: Text -> Text
checklistEndpoint i = "https://api.trello.com/1/checklists/" <> i
cardEndpoint :: CardId -> Text
cardEndpoint (CardId i) = "https://api.trello.com/1/cards/" <> i
cardSubscribedEndpoint :: CardId -> Text
cardSubscribedEndpoint i = cardEndpoint i <> "/subscribed"
cardLabelsEndpoint :: CardId -> Text
cardLabelsEndpoint i = cardEndpoint i <> "/labels"
cardDescEndpoint :: CardId -> Text
cardDescEndpoint i = cardEndpoint i <> "/desc"
userBoardsEndpoint :: Text -> Text
userBoardsEndpoint userId = "https://api.trello.com/1/members/" <> userId <> "/boards"



-- Private API functions

getChecklist :: Creds -> Text -> IO [Text]
getChecklist creds checklistId =
  do r <- get' creds (checklistEndpoint checklistId)
     let items = r ^.. responseBody . key "checkItems" . values
     return (map (\i -> i ^?! key "name" . _String) items)

getCards :: Creds -> ListId -> IO [Card]
getCards creds listId =
  do r <- get' creds (listCardsEndpoint listId)
     mapM (\v ->
             do let checklistId = v ^? key "idChecklists" . nth 0 . _String
                checklist <- case checklistId of
                               Nothing -> return Nothing
                               Just i -> Just . S.fromList <$> getChecklist creds i
                return $ Card (CardId (v ^?! key "id" . _String))
                              (v ^?! key "name" . _String)
                              (v ^?! key "desc" . _String)
                              checklist
                              (v ^?! key "subscribed" . _Bool)
                              (v ^.. key "labels" . values . key "color" . _String)
                              (v ^?! key "shortUrl" . _String))
          (r ^.. responseBody . values)

getLists :: Creds -> BoardId -> IO [List]
getLists creds boardId =
  do r <- get' creds (boardListEndpoint boardId)
     mapM (\v -> do let listId = ListId (v ^?! key "id" . _String)
                    cards <- getCards creds listId
                    return $ List listId
                                  (v ^?! key "name" . _String)
                                  (S.fromList cards)
                                  )
                  (r ^.. responseBody . values)

notClosed v = not $ v ^?! key "closed" . _Bool


addList :: Creds -> BoardId -> Text -> IO ListId
addList creds boardId name =
  do r <- post' creds (boardListEndpoint boardId) [("name", name)]
     return $ ListId $ r ^?! responseBody . key "id" . _String

archiveList :: Creds -> ListId -> IO ()
archiveList creds list = do put' creds (listClosedEndpoint list) [("value", "true")]
                            return ()

setCardSubscribed :: Creds -> CardId -> Bool -> IO ()
setCardSubscribed creds card subscribe =
  do put' creds (cardSubscribedEndpoint card) [("value", case subscribe of
                                                           True -> "true"
                                                           False -> "false")]
     return ()

setCardLabels :: Creds -> CardId -> [Text] -> IO ()
setCardLabels creds card labels =
  do put' creds (cardLabelsEndpoint card) [("value", T.intercalate "," labels)]
     return ()

setCardDesc :: Creds -> CardId -> Text -> IO ()
setCardDesc creds card desc =
  do put' creds (cardDescEndpoint card) [("value", desc)]
     return ()

addCard :: Creds -> ListId -> Text -> Text -> Bool -> [Text] -> IO CardId
addCard creds listId name desc subscribe labels =
  do r <- post' creds (listCardsEndpoint listId)
                      (addLabels [("name", name),("due", "null"),("desc", desc)])
     let cardId = CardId $ r ^?! responseBody . key "id" . _String
     setCardSubscribed creds cardId subscribe
     return cardId
  where addLabels rest = if null labels
                            then rest
                            else ("labels", T.intercalate "," labels) : rest

deleteCard :: Creds -> CardId -> IO ()
deleteCard creds card = do delete' creds (cardEndpoint card)
                           return ()

notColonPrefixed :: Text -> Bool
notColonPrefixed s = not $ T.isPrefixOf ":" s


-- Diffing implementation
data Change = AddList BoardId List
            | RemoveList BoardId ListId
            | AddCard ListId Card
            | RemoveCard ListId CardId
            | SetCardDesc CardId Text
            | SetCardLabels CardId [Text]
            | SetCardSubscribed CardId Bool
     deriving (Eq, Show, Ord)

diffCards :: ListId -> Set Card -> Set Card -> [Change]
diffCards l old new =
  concatMap (\o -> case find (\n -> cardName o == cardName n) new of
                     Nothing -> [RemoveCard l (cardId o)]
                     Just n ->
                       catMaybes
                          [if cardLabels o /= cardLabels n
                            then Just $ SetCardLabels (cardId o) (cardLabels n)
                            else Nothing,
                          if cardDescription o /= cardDescription n
                           then Just $ SetCardDesc (cardId o) (cardDescription n)
                           else Nothing,
                          if cardSubscribed o /= cardSubscribed n
                           then Just $ SetCardSubscribed (cardId o) (cardSubscribed n)
                           else Nothing
                          ]
                       )
            old
  <> concatMap (\n -> case find (\o -> cardName o == cardName n) old of
                        Nothing -> [AddCard l n]
                        Just _ -> []) new

diffLists :: BoardId -> Set List -> Set List -> [Change]
diffLists b old new = concatMap (\o -> case find (\n -> listName o == listName n) new of
                                         Nothing -> [RemoveList b (listId o)]
                                         Just n -> diffCards (listId o)
                                                             (listCards o)
                                                             (listCards n)) old
                   <> concatMap (\n -> case find (\o -> listName o == listName n) old of
                                         Nothing -> [AddList b n]
                                         Just _ -> []) new

effectChange :: Creds -> Change -> IO ()
effectChange creds (AddList board list) =
  do lid <- addList creds board (listName list)
     mapM_ (effectChange creds . (AddCard (listId list)))
           (S.toList (listCards list))
effectChange creds (RemoveList board list) = archiveList creds list
effectChange creds (AddCard list card) =
  do addCard creds list (cardName card) (cardDescription card) (cardSubscribed card) (cardLabels card)
     return ()
effectChange creds (RemoveCard list card) = deleteCard creds card
effectChange creds (SetCardDesc card desc) = setCardDesc creds card desc
effectChange creds (SetCardLabels card labels) = setCardLabels creds card labels
effectChange creds (SetCardSubscribed card subscribed) = setCardSubscribed creds card subscribed
