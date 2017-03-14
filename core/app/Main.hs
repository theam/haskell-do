{-# LANGUAGE DataKinds, TypeOperators, ScopedTypeVariables #-}

module Main where

import WebSocketServer
import Types
import Utils
import qualified Network.WebSockets as WS
import Language.Haskell.GhcMod
import System.Environment
import Data.Proxy

address :: String
address = "0.0.0.0"

port :: Int 
port = 3000

main = do
  filepath : _ <- getArgs
  state <- initializeState filepath
  nb <- loadNotebookFromFile filepath
  WS.runServer address port (application filepath nb state)

