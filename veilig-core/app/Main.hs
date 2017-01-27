module Main where

import WebSocketServer
import Types
import Utils
import qualified Network.WebSockets as WS
import System.Environment

main = do
  filepath : _ <- getArgs
  state <- initializeState filepath
  WS.runServer "0.0.0.0" 3000 (application state)
