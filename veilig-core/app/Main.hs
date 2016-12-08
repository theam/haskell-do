module Main where

import Types
import WebSocketServer
import qualified Network.WebSockets as WS
import Control.Concurrent

address :: String
address = "0.0.0.0"

port :: Int
port = 3000

main :: IO ()
main = do
  server <- newServer 
  WS.runServer address port (application server)
