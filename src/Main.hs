{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Aeson          (toJSON)
import           Data.Aeson.Lens
import           Data.List
import           Data.Monoid
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

get' (k,t) u = getWith (defaults & param "key" .~ [k] & param "token" .~ [t])
                       (T.unpack u)

post' (k,t) u args = postWith (defaults & params .~ (("key", k) : ("token", t) : args)) (T.unpack u) (TE.encodeUtf8 "")

groupEndpoint :: Text -> Text
groupEndpoint orgName = "https://api.trello.com/1/organizations/" <> orgName <> "/boards"

getGroups :: (Text, Text) -> Text -> IO [(Text, Text)]
getGroups creds orgName =
  do r <- get' creds (groupEndpoint orgName)
     return $ map (\v -> (v ^?! key "id" . _String
                         ,v ^?! key "name" . _String))
                  (r ^.. responseBody . values)

boardListEndpoint :: Text -> Text
boardListEndpoint boardId = "https://api.trello.com/1/boards/" <> boardId <> "/lists"

getLists :: (Text, Text) -> Text -> IO [(Text, Text)]
getLists creds boardId =
  do r <- get' creds (boardListEndpoint boardId)
     return $ map (\v -> (v ^?! key "id" . _String
                         ,v ^?! key "name" . _String))
                  (r ^.. responseBody . values)

addList :: (Text, Text) -> Text -> Text -> IO Text
addList creds boardId name =
  do r <- post' creds (boardListEndpoint boardId) [("name", name)]
     return $ r ^?! responseBody . key "id" . _String

listCardsEndpoint :: Text -> Text
listCardsEndpoint boardId = "https://api.trello.com/1/lists/" <> boardId <> "/cards"

getCards :: (Text, Text) -> Text -> IO [(Text, Text)]
getCards creds listId =
  do r <- get' creds (listCardsEndpoint listId)
     return $ map (\v -> (v ^?! key "id" . _String
                         ,v ^?! key "name" . _String))
                  (r ^.. responseBody . values)

addCard :: (Text, Text) -> Text -> Text -> Text -> IO Text
addCard creds listId name desc =
  do r <- post' creds (listCardsEndpoint listId)
                      [("name", name),("due", "null"),("desc", desc)]
     return $ r ^?! responseBody . key "id" . _String

mainBoard = "55528aa166fe462917fc9dc0"

projects = [("5506e72021961efe09fa5ca4", "Haymarket", "https://trello.com/b/DhK1G0jg/haymarket")
           ,("54d3f234c4aa2e743cb57534", "Verso", "https://trello.com/b/ooVAX4gj/verso")
           ,("54c99a52dd268d7dc428c586", "TNI", "https://trello.com/b/2DEBPclN/tni")]

notColonPrefixed :: (Text,Text) -> Bool
notColonPrefixed (_, name) = not $ T.isPrefixOf ":" name

main :: IO ()
main = do readDotEnv
          key <- T.pack <$> getEnv "TRELLO_DEVELOPER_PUBLIC_KEY"
          token <- T.pack <$> getEnv "TRELLO_MEMBER_TOKEN"
          let creds = (key, token)
          projectLists <- getLists creds mainBoard
          forM_ projects $ \(projectBoardId, projectName, projectUrl) ->
            do projectListId <- case find ((== projectName) . snd) projectLists of
                                  Nothing -> addList creds mainBoard projectName
                                  Just (id,_) -> return id
               tasks <- filter notColonPrefixed <$> getLists creds projectBoardId
               existing <- getCards creds projectListId
               forM_ tasks $ \(listId, taskName) -> do
                  do let prefixedName = projectName <> ": " <> taskName
                     case find ((== prefixedName) . snd) existing of
                       Nothing -> addCard creds projectListId prefixedName projectUrl
                       Just (id,_) -> return id
                     return ()
