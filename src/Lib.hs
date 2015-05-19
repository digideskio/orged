{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Prelude              hiding (concatMap, elem, find, mapM,
                                       mapM_)

import qualified Configuration.Dotenv
import           Control.Applicative
import           Control.Lens
import           Control.Monad        (when)
import           Data.Aeson           (toJSON)
import           Data.Aeson.Lens
import           Data.Foldable
import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.Maybe           (catMaybes)
import           Data.Monoid
import           Data.Set             (Set)
import qualified Data.Set             as S
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE
import qualified Data.Text.IO         as T
import           Data.Traversable
import           Network.Wreq
import           System.Directory     (doesFileExist)
import           System.Environment
import           Web.Trello.Sync

readSummaryName :: IO Text
readSummaryName = T.pack <$> getEnv "ORGED_SUMMARY_BOARD"

getOrgedUserBoards :: IO [Board]
getOrgedUserBoards = do userId <- T.pack <$> getEnv "ORGED_USER_ID"
                        getUserBoards userId

getOrgedUserBoardNames :: IO [Text]
getOrgedUserBoardNames = do userId <- T.pack <$> getEnv "ORGED_USER_ID"
                            getUserBoardNames userId

top3DoneStrategy :: Board -> [Board] -> Board
top3DoneStrategy summaryBoard projectBoards =
  do let addPrefix p c = c { cardName = p <> cardName c }
     let setLabel l c = c { cardLabels = [l] }
     let setDoneLabel = setLabel "green"
     let setTodoLabel = setLabel "yellow"
     let customize c = c { cardSubscribed = True, cardDescription = cardUrl c, cardLabels = [] }
     let findAll name = concatMap (\b -> case find (\l -> listName l == name)
                                                   (boardLists b) of
                                    Nothing -> []
                                    Just l -> map (addPrefix (boardName b <> ": "))
                                                  (S.toList (listCards l)))
                            projectBoards
     let top3 = findAll "Top 3"
     let done = findAll "Done"
     let allOurs = top3 <> done
     let allExisting = S.fromList $
                         concatMap (\l -> map cardName $ S.toList $ listCards l)
                                   (boardLists summaryBoard)
     let unscheduled =
           S.fromList $ filter (\c -> not $ S.member (cardName c) allExisting)
                               (map customize allOurs)
     let updateCard c = case find ((== cardName c) . cardName) done of
                          Nothing -> case find ((== cardName c) . cardName) top3 of
                                       Nothing -> c
                                       Just _ -> setTodoLabel c
                          Just _ -> setDoneLabel c
     let permittedCard c = case cardSubscribed c of
                             True -> case find ((== cardName c) . cardName) allOurs of
                                       Nothing -> False
                                       Just _ -> True
                             False -> True
     let newLists = S.map (\l ->
           let newList = if listName l == "Unscheduled"
                            then l { listCards = S.union unscheduled (listCards l)}
                            else l in
           newList { listCards = S.filter permittedCard $
                                 S.map updateCard (listCards newList)})
                                                  (boardLists summaryBoard)
     summaryBoard { boardLists = newLists }

run :: (Text -> IO ()) -> IO ()
run log = do e <- doesFileExist ".env"
             when e $ Configuration.Dotenv.loadFile True ".env"
             summaryName <- readSummaryName
             allBoards <- getOrgedUserBoards
             let [summaryBoard] = filter ((== summaryName) . boardName) allBoards
             let projectBoards = filter ((/= summaryName) . boardName) allBoards

             Web.Trello.Sync.updateBoard log
                                         summaryBoard
                                         (top3DoneStrategy summaryBoard projectBoards)
