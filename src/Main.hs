{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Configuration.Dotenv
import           Control.Applicative
import           Control.Concurrent             (MVar, forkIO, modifyMVar,
                                                 modifyMVar_, newMVar, readMVar,
                                                 threadDelay)
import           Control.Exception              (SomeException, catch)
import           Control.Monad                  (forM_, forever, when)
import           Control.Monad.Trans            (liftIO)
import           Data.ByteString                (ByteString)
import           Data.Monoid
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import qualified Data.Text.Lazy                 as LT
import           Data.UUID                      (toASCIIBytes)
import           Data.UUID.V4                   (nextRandom)
import           Network.HTTP.Types.Method
import           Network.Wai                    (Response)
import           Network.Wai.Handler.Warp       (run)
import           Network.Wai.Handler.WebSockets
import           Network.WebSockets             hiding (Response)
import           System.Directory
import           System.Environment
import           Web.Fn

import qualified Lib

page = "<html>\
       \<style type=\"text/css\">\
       \div { width: 200px; margin: 40px auto; }\
       \#button { width: 200px; text-align: center; font-size: 40px; background-color: blue; color: white; border-radius: 10px; border: 10px solid black; }\
       \#button:hover { cursor: pointer; color: red; }\
       \#button:disabled { color: black; background-color: gray; }\
       \#button:hover:disabled { cursor: auto; }\
       \pre { width: 150px; margin: 40px auto; text-align: center; border: 10px solid black; padding: 10px; background-color: gray; }\
       \.clear { width: 50px; margin: 10px 0 0 100px; text-align: center; color: white; background-color: blue; }\
       \.clear:hover { cursor: pointer; }\
       \</style>\
       \<body></body>\
       \<script type=\"text/javascript\" src=\"/m.js\"></script>\
       \<script type=\"text/javascript\" src=\"/app.js\"></script>\
       \</html>"

type State = [(ByteString, Connection)]

removeClient :: ByteString -> MVar State -> IO ()
removeClient uuid state = modifyMVar_ state $ \s -> do
  return (filter ((/= uuid) . fst) s)

socketHandler :: MVar State -> ServerApp
socketHandler state pending =
  do conn <- acceptRequest pending
     forkPingThread conn 10
     uuid' <- nextRandom
     let uuid = toASCIIBytes uuid'
     modifyMVar_ state $ \s ->
       return ((uuid, conn) : s)
     catch (forever $ do
       (_ ) <- receive conn
       return ())
           (\(e :: SomeException) -> removeClient uuid state)

broadcast :: MVar State -> Text -> IO ()
broadcast state msg =
  do clients <- readMVar state
     forM_ clients (\(uuid, conn) ->
                      catch (sendTextData conn msg)
                            (\(e :: SomeException) -> removeClient uuid state))

workerThread :: MVar State -> MVar Bool -> IO ()
workerThread clients request = forever $
  do v <- readMVar request
     when v $ do modifyMVar_ request (const (return False))
                 broadcast clients "Starting sync..."
                 Lib.run (broadcast clients)
                 broadcast clients "Finished sync..."
     threadDelay 100000

data Ctxt = Ctxt { _req          :: FnRequest
                 , _clients      :: MVar State
                 , _sync_request :: MVar Bool }
instance RequestContext Ctxt where
  getRequest = _req
  setRequest c r = c { _req = r }

site :: Ctxt -> IO Response
site ctxt = route ctxt [end // method GET  ==> homeH
                       ,end // method POST ==> queueH
                       ,path "go"          ==> goH
                       ,path "summary"     ==> summaryH
                       ,anything           ==> staticServe "static"
                       ]
                  `fallthrough` notFoundText "Page not found."

homeH :: Ctxt -> IO (Maybe Response)
homeH _ = okHtml page

queueH :: Ctxt -> IO (Maybe Response)
queueH ctxt = do broadcast (_clients ctxt) "Queuing request..."
                 modifyMVar_ (_sync_request ctxt) (const (return True))
                 okText ""

goH :: Ctxt -> IO (Maybe Response)
goH ctxt = do liftIO $ broadcast (_clients ctxt) "Queuing request..."
              liftIO $ modifyMVar_ (_sync_request ctxt) (const (return True))
              redirect "/"

summaryH :: Ctxt -> IO (Maybe Response)
summaryH _ = do s <- Lib.readSummaryName
                names <- filter (/= s) <$> Lib.getOrgedUserBoardNames
                okText (T.intercalate "\n" (s <> "\n-----":names))

main :: IO ()
main = do
  port <- read <$> getEnv "PORT"
  e <- doesFileExist ".env"
  when e $ Configuration.Dotenv.loadFile True ".env"
  clients <- newMVar []
  request <- newMVar False
  forkIO (workerThread clients request)
  let ctxt = (Ctxt defaultFnRequest clients request)
  run port (websocketsOr defaultConnectionOptions
                         (socketHandler clients)
                         (toWAI ctxt site))
