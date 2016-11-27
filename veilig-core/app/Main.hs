module Main where

import Types
import WebSocketServer

address :: String
address = "0.0.0.0"

port :: Int
port = 3000

main :: IO ()
main = do
    state <- newMVar newServerState
    WS.runServer address port $ application state
