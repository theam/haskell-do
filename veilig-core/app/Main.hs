module Main where

import Types
import WebSocketServer
import Language.Haskell.GhcMod
import qualified Network.WebSockets as WS
import Control.Concurrent
import System.Environment
import System.Command (runInteractiveCommand)

address :: String
address = "0.0.0.0"

port :: Int
port = 3000

main :: IO ()
main = do
  filepath : _ <- getArgs
  cradle <- findCradle filepath
  state <- initializeState filepath cradle
  WS.runServer address port (application state)
