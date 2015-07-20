{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Prelude              hiding (any, concatMap, elem, elem, find,
                                       mapM, mapM_)

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

topDoneStrategy :: Board -> [Board] -> Board
topDoneStrategy summaryBoard projectBoards =
  do let addPrefix p c = c { cardName = p <> cardName c }
     let setLabel l c = c { cardLabels = [l] }
     let setDoneLabel = setLabel "green"
     let setTodoLabel = setLabel "blue"
     let setInbetweenLabel = setLabel "orange"
     let customize c = c { cardSubscribed = True, cardDescription = cardUrl c, cardLabels = [] }
     let findAll name = concatMap (\b -> case find (\l -> name `T.isPrefixOf` listName l)
                                                   (boardLists b) of
                                    Nothing -> []
                                    Just l -> map (addPrefix (boardName b <> ": "))
                                                  (listCards l))
                            projectBoards
     let exists name lists = any ((T.isPrefixOf name) . listName) lists
     let top = findAll "Top"
     let done = findAll "Done"
     let inbetween =
           concatMap (\b ->
             let lists = boardLists b in
             if exists "Top" lists &&
                exists "Done" (dropWhile (not . (T.isPrefixOf "Top") . listName) lists)
                then let middle =
                           takeWhile (not . (T.isPrefixOf "Done") . listName)
                           . tail
                           . dropWhile (not . (T.isPrefixOf "Top") . listName)
                           $ lists in
                         concatMap (map (addPrefix (boardName b <> ": ")) . listCards)
                                   middle

                else []) projectBoards
     let allOurs = top <> inbetween <> done
     let allExisting = concatMap (\l -> map cardName $ listCards l)
                                 (boardLists summaryBoard)
     let unscheduled =
           filter (\c -> not $ elem (cardName c) allExisting)
                  (map customize allOurs)
     let updateCard c =
           case find ((== cardName c) . cardName) done of
             Nothing ->
               case find ((== cardName c) . cardName) top of
                 Nothing ->
                   case find ((== cardName c) . cardName) inbetween of
                     Nothing -> c
                     Just _ -> setInbetweenLabel c
                 Just _ -> setTodoLabel c
             Just _ -> setDoneLabel c
     let permittedCard c = case cardSubscribed c of
                             True -> case find ((== cardName c) . cardName) allOurs of
                                       Nothing -> False
                                       Just _ -> True
                             False -> True
     let newLists = map (\l ->
           let newList = if listName l == "Unscheduled"
                            then l { listCards = unscheduled <> (listCards l)}
                            else l in
           newList { listCards = filter permittedCard $
                                 map updateCard (listCards newList)})
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
                                         (topDoneStrategy summaryBoard projectBoards)
