{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Aeson          (toJSON)
import           Data.Aeson.Lens
import           Data.List
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Monoid
import           Data.Set            (Set)
import qualified Data.Set            as S
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as TE
import qualified Data.Text.IO        as T
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

groupEndpoint :: Text -> Text
groupEndpoint orgName = "https://api.trello.com/1/organizations/" <> orgName <> "/boards"
boardListEndpoint :: BoardId -> Text
boardListEndpoint (BoardId i) = "https://api.trello.com/1/boards/" <> i <> "/lists"
listCardsEndpoint :: ListId -> Text
listCardsEndpoint (ListId i) = "https://api.trello.com/1/lists/" <> i <> "/cards"
checklistEndpoint :: Text -> Text
checklistEndpoint i = "https://api.trello.com/1/checklists/" <> i

getChecklist :: Creds -> Text -> IO [Text]
getChecklist creds checklistId =
  do r <- get' creds (checklistEndpoint checklistId)
     print (r ^? responseBody)
     let items = r ^.. responseBody . key "checkItems" . values
     return (map (\i -> i ^?! key "name" . _String) items)

getCards :: Creds -> ListId -> IO [Card]
getCards creds listId =
  do r <- get' creds (listCardsEndpoint listId)
     print (r ^? responseBody)
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

addCard :: Creds -> ListId -> Text -> Text -> IO CardId
addCard creds listId name desc =
  do r <- post' creds (listCardsEndpoint listId)
                      [("name", name),("due", "null"),("desc", desc)]
     return $ CardId $ r ^?! responseBody . key "id" . _String

mainBoard = BoardId "55528aa166fe462917fc9dc0"

projects =
  [(BoardId "5506e72021961efe09fa5ca4", "Haymarket", "https://trello.com/b/DhK1G0jg/haymarket")
  ,(BoardId "54d3f234c4aa2e743cb57534", "Verso", "https://trello.com/b/ooVAX4gj/verso")
  ,(BoardId "54c99a52dd268d7dc428c586", "TNI", "https://trello.com/b/2DEBPclN/tni")]

notColonPrefixed :: Text -> Bool
notColonPrefixed s = not $ T.isPrefixOf ":" s

main :: IO ()
main = do readDotEnv
          creds <- readCreds
          projectLists <- getLists creds mainBoard
          forM_ projects $ \(projectBoardId, projectName, projectUrl) ->
            do projectListId <- case find ((== projectName) . listName) projectLists of
                                  Nothing -> addList creds mainBoard projectName
                                  Just list -> return (listId list)
               tasks <- filter (notColonPrefixed.listName) <$> getLists creds projectBoardId
               existing <- getCards creds projectListId
               forM_ tasks $ \task ->
                  do let prefixedName = projectName <> ": " <> (listName task)
                     case find ((== prefixedName) . cardName) existing of
                       Nothing -> addCard creds projectListId prefixedName projectUrl
                       Just card -> return (cardId card)
                     return ()
