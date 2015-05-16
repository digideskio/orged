{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Control.Monad.Trans (liftIO)
import           Data.Monoid
import qualified Data.Text           as T
import qualified Data.Text.Lazy      as LT
import           System.Environment
import           Web.Scotty

import qualified Lib

page c = "<html><body><style type=\"text/css\">\
         \form { width: 200px; margin: 40px auto; }\
         \button { width: 200px; text-align: center; font-size: 40px; background-color: blue; color: white; border-radius: 10px; border: 10px solid black; }\
         \button:hover { cursor: pointer; color: red; }\
         \button:disabled { color: black; background-color: gray; }\
         \button:hover:disabled { cursor: auto; }\
         \pre { width: 110px; margin: 40px auto; text-align: center; border: 10px solid black; padding: 10px; background-color: gray; }\
         \</style>\
         \<form method=post action=/><button id=\"button\" onclick=\"document.getElementById(\'button').setAttribute('disabled', true)\">SYNC</button></form>\
         \<pre>" <> LT.fromStrict c <>"</pre>\
         \</body></html>"

main :: IO ()
main = do
  port <- read <$> getEnv "PORT"
  scotty port $ do get "/" $ do liftIO Lib.readDotEnv
                                creds <- liftIO Lib.readCreds
                                s <- liftIO Lib.readSummaryName
                                names <- filter (/= s) <$>
                                         liftIO (Lib.getOrgedUserBoardNames creds)
                                html (page (T.intercalate "\n" (s <> ":\n-----":names)))
                   post "/" $ do liftIO Lib.run
                                 redirect "/"
