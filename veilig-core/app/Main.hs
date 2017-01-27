module Main where

import WebSocketServer
import Types
import Utils
import qualified Network.WebSockets as WS
import System.Environment

address :: String
address = "0.0.0.0"

port :: Int
port = 3000

main = do
  filepath : _ <- getArgs
  state <- initializeState filepath
  WS.runServer address port (application state)
