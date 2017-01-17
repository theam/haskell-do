module Main where

import Types
import WebSocketServer
import qualified Network.WebSockets as WS
import Control.Concurrent
import System.Environment
import System.Process

address :: String
address = "0.0.0.0"

port :: Int
port = 3000

main :: IO ()
main = do
  projectname : _ <- getArgs
  createProcess $ proc "stack" ["new", projectname]
  state <- initializeState projectname
  WS.runServer address port (application state)
