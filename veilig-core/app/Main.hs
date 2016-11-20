{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Concurrent
import Control.Monad (forM_, forever)
import Control.Exception (finally)

import qualified Network.WebSockets as WS

type Client = (Text, WS.Connection)
type ServerState = [Client]

newServerState :: ServerState
newServerState = []

numClients :: ServerState -> Int
numClients = length

clientExists :: Client -> ServerState -> Bool
clientExists c = any ((== fst c) . fst)

addClient :: Client -> ServerState -> ServerState
addClient c s = c : s

removeClient :: Client -> ServerState -> ServerState
removeClient c = filter ((/= fst c) . fst)

broadcast :: Text -> ServerState -> IO ()
broadcast msg state = do
    T.putStrLn msg
    forM_ state $ \(_, conn) -> WS.sendTextData conn msg

application :: MVar ServerState -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  msg <- WS.receiveData conn
  clients <- readMVar state
  case msg of
      _   | not (prefix `T.isPrefixOf` msg) ->
              WS.sendTextData conn ("Wrong announcement" :: Text)
          | any ($ fst client)
              [T.null, T.any isPunctuation, T.any isSpace] ->
                  WS.sendTextData conn ("Name cannot " `mappend`
                      "contain punctuation or whitespace, and " `mappend`
                      "cannot be empty" :: Text)
          | clientExists client clients ->
              WS.sendTextData conn ("User already exists" :: Text)
          | otherwise -> flip finally disconnect $ do
              modifyMVar_ state $ \s -> do
                let s' = addClient client s
                return s'
              talk conn state client
        where
          prefix     = "Hi! I am "
          client     = (T.drop (T.length prefix) msg, conn)
          disconnect = do
            -- Remove client and return new state
            s <- modifyMVar state $ \s ->
              let s' = removeClient client s in return (s', s')
            broadcast (fst client `mappend` " disconnected") s

talk :: WS.Connection -> MVar ServerState -> Client -> IO ()
talk conn state (user, _) = forever $ do
  msg <- WS.receiveData conn
  readMVar state >>= broadcast msg

main :: IO ()
main = do
  state <- newMVar newServerState
  WS.runServer "0.0.0.0" 3000 $ application state
