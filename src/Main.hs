{-# LANGUAGE OverloadedStrings #-}

import           Prelude             hiding (concatMap, find, mapM, mapM_)

import           Control.Applicative
import           Control.Lens
import           Data.Aeson          (toJSON)
import           Data.Aeson.Lens
import           Data.Foldable
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Monoid
import           Data.Set            (Set)
import qualified Data.Set            as S
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as TE
import qualified Data.Text.IO        as T
import           Data.Traversable
import           Network.Wreq
import           System.Environment

readDotEnv :: IO ()
readDotEnv = do envs <- T.lines <$> T.readFile ".env"
                mapM_ (\s -> let [name, val] = T.splitOn "=" s in
                                 setEnv (T.unpack name) (T.unpack val))
                      envs

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


get' (Creds (k,t)) u = getWith (defaults & param "key" .~ [k] & param "token" .~ [t])
                               (T.unpack u)

post' (Creds (k,t)) u args = postWith (defaults & params .~ (("key", k) : ("token", t) : args)) (T.unpack u) (TE.encodeUtf8 "")

put' (Creds (k,t)) u args = putWith (defaults & params .~ (("key", k) : ("token", t) : args)) (T.unpack u) (TE.encodeUtf8 "")

delete' (Creds (k,t)) u = deleteWith (defaults & params .~ (("key", k) : ("token", t) : [])) (T.unpack u)

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
                              checklist)
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

getBoards :: Creds -> Text -> IO [Board]
getBoards creds orgName =
  do r <- get' creds (groupEndpoint orgName)
     mapM (\v ->
               do let boardId = BoardId (v ^?! key "id" . _String)
                  lists <- S.fromList <$> getLists creds boardId
                  return $ Board boardId
                                 (v ^?! key "name" . _String)
                                 lists
               )
               (r ^.. responseBody . values)


addList :: Creds -> BoardId -> Text -> IO ListId
addList creds boardId name =
  do r <- post' creds (boardListEndpoint boardId) [("name", name)]
     return $ ListId $ r ^?! responseBody . key "id" . _String

archiveList :: Creds -> ListId -> IO ()
archiveList creds list = do put' creds (listClosedEndpoint list) [("value", "true")]
                            return ()

addCard :: Creds -> ListId -> Text -> Text -> IO CardId
addCard creds listId name desc =
  do r <- post' creds (listCardsEndpoint listId)
                      [("name", name),("due", "null"),("desc", desc)]
     return $ CardId $ r ^?! responseBody . key "id" . _String

deleteCard :: Creds -> CardId -> IO ()
deleteCard creds card = do delete' creds (cardEndpoint card)
                           return ()


mainBoard = BoardId "55528aa166fe462917fc9dc0"

projects =
  [(BoardId "5506e72021961efe09fa5ca4", "Haymarket", "https://trello.com/b/DhK1G0jg/haymarket")
  ,(BoardId "54d3f234c4aa2e743cb57534", "Verso", "https://trello.com/b/ooVAX4gj/verso")
  ,(BoardId "54c99a52dd268d7dc428c586", "TNI", "https://trello.com/b/2DEBPclN/tni")]

notColonPrefixed :: Text -> Bool
notColonPrefixed s = not $ T.isPrefixOf ":" s

data Change = AddList BoardId List
            | RemoveList BoardId ListId
            | AddCard ListId Card
            | RemoveCard ListId CardId

diffCards :: ListId -> Set Card -> Set Card -> [Change]
diffCards l old new = concatMap (\o -> case find (\n -> cardName o == cardName n) new of
                                         Nothing -> [RemoveCard l (cardId o)]
                                         Just n ->
                                           if o == n
                                             then []
                                             else error $ "Don't support diffing cards yet: "
                                                        <> show o <> " to " <> show n)
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
     mapM_ (\c -> addCard creds lid (cardName c) (cardDescription c))
           (S.toList (listCards list))
effectChange creds (RemoveList board list) = archiveList creds list
effectChange creds (AddCard list card) =
  do addCard creds list (cardName card) (cardDescription card)
     return ()
effectChange creds (RemoveCard list card) = deleteCard creds card

main :: IO ()
main = do readDotEnv
          creds <- readCreds
          projectBoards <- mapM (\(i, name, _) -> do lists <- S.fromList <$> getLists creds i
                                                     return (Board i name lists))
                                               projects
          projectLists <- getLists creds mainBoard

          let addPrefix p c = c { cardName = p <> cardName c }
          let cards = concatMap (\b -> case find (\l -> listName l == "Top 3")
                                                 (boardLists b) of
                                  Nothing -> []
                                  Just l -> map (addPrefix (boardName b <> ": "))
                                               (S.toList (listCards l)))
                          projectBoards
          let allExisting = S.fromList $
                              concatMap (\l -> map cardName $ S.toList $ listCards l)
                                        projectLists
          let newCards = S.fromList $ filter (\c -> not $ S.member (cardName c) allExisting) cards
          let newLists = S.fromList $ map (\l -> if listName l == "Later"
                                                    then l { listCards = S.union newCards (listCards l)}
                                                    else l) projectLists
          let changes = diffLists mainBoard (S.fromList projectLists) newLists
          mapM_ (effectChange creds) changes
