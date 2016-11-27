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

import Types
import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Language.Haskell.Interpreter
import Data.String.Conversions

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

addClient' :: Client -> MVar ServerState -> IO ()
addClient' client state =
    modifyMVar_ state $ \s -> do
        return $ addClient client s

removeClient :: Client -> ServerState -> ServerState
removeClient c = filter ((/= fst c) . fst)

removeClient' :: Client -> MVar ServerState -> IO ServerState
removeClient' client state =
    modifyMVar state $ \s ->
        let s' = removeClient client s in return (s', s')

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
                notifyWrongPrefix conn
            | isWrongName ->
                notifyWrongName conn
            | clientExists client clients ->
                notifyUserExists conn
            | otherwise -> afterEverything disconnect $ do
                addClient' client state
                talk conn state client
                    where
                        notifyWrongPrefix conn = WS.sendTextData conn ("Wrong announcement" :: Text)
                        isWrongName = any ($ fst client) [T.null, T.any isPunctuation, T.any isSpace]
                        notifyWrongName conn = WS.sendTextData conn ("Name cannot contain punctuation or whitespace, and cannot be empty" :: Text)
                        notifyUserExists conn = WS.sendTextData conn ("User already exists" :: Text)
                        prefix     = "Hi! I am "
                        client     = (T.drop (T.length prefix) msg, conn)
                        afterEverything = flip finally
                        disconnect = do
                            s <- removeClient' client state
                            broadcast (fst client <> " disconnected") s

intNotebook :: Notebook -> Text
intNotebook n = T.intercalate "\n"
              . map cellContent
              . filter (\c -> cellType c == CodeCell )
              . cells
              $ n

notebookInterpreter :: Text -> Interpreter Text
notebookInterpreter code = do
    setImportsQ[("Prelude",Nothing)]
    a <- eval $ cs code
    return $ cs a

talk :: WS.Connection -> MVar ServerState -> Client -> IO ()
talk conn state (user, _) = forever $ do
    msg <- WS.receiveData conn
    let x = decode msg :: Maybe Notebook
    case x of
        Just n -> do
            r <- runInterpreter $ notebookInterpreter (intNotebook n)
            case r of
                Left err -> readMVar state >>= broadcast (cs (show err))
                Right res -> readMVar state >>= broadcast (cs res)
        Nothing ->
            readMVar state >>= broadcast "Could not decode"

main :: IO ()
main = do
    state <- newMVar newServerState
    WS.runServer "0.0.0.0" 3000 $ application state
