{-# LANGUAGE DataKinds, TypeOperators, ScopedTypeVariables #-}

module Main where

import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WebSockets
import Servant
import WebSocketServer
import Types
import Utils
import qualified Network.WebSockets as WS
import System.Environment
import Data.Proxy

address :: String
address = "0.0.0.0"

main = do
  filepath : _ <- getArgs
  nb <- loadNotebookFromFile filepath
  state <- initializeState filepath
  let server :: Server API = return filepath
  run 3000 (websocketsOr WS.defaultConnectionOptions (application nb state) (serve api server))

type API = "filepath" :> Get '[JSON] FilePath
api :: Proxy API
api = Proxy
