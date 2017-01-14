module Main where

import Types
import WebSocketServer
import qualified Network.WebSockets as WS
import Control.Concurrent
import System.Environment

address :: String
address = "0.0.0.0"

port :: Int
port = 3000

main :: IO ()
main = do
  filepath : _ <- getArgs
  state <- initializeState filepath
  WS.runServer address port (application state)
