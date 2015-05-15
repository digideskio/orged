{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Control.Monad.Trans (liftIO)
import           Data.Monoid
import           System.Environment
import           Web.Scotty

import qualified Lib

page = "<html><body><style type=\"text/css\">\
       \form { width: 200px; margin: 40px auto; }\
       \button { font-size: 40px; background-color: blue; color: white; border-radius: 10px; border: 10px solid black; }\
       \button:hover { cursor: pointer; color: red; }\
       \button:disabled { color: black; background-color: gray; }\
       \button:hover:disabled { cursor: auto; }\
       \</style>\
       \<form method=post action=/><button id=\"button\" onclick=\"document.getElementById(\'button').setAttribute('disabled', true)\">MAGIC SYNC BUTTON</button></form></body></html>"

main :: IO ()
main = do
  port <- read <$> getEnv "PORT"
  scotty port $ do get "/" $ html page
                   post "/" $ do liftIO Lib.run
                                 redirect "/"
