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
  state <- initializeState projectname
  -- ^ can be replaced when frontend ability is in place to call
  -- functions like createNewNotebook
  WS.runServer address port (application state)
