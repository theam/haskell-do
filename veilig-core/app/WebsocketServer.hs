{-# LANGUAGE OverloadedStrings #-}
module WebSocketServer where

import Types

import Language.Haskell.Interpreter
import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend, (<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Concurrent
import Control.Monad (forM_, forever)
import Control.Exception (finally)
import qualified Network.WebSockets as WS
import Data.String.Conversions
import Data.Aeson
import Interpreter

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
    modifyMVar_ state $ \s -> return $ addClient client s

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
    dispatchMessage conn state (client conn msg) clients msg

dispatchMessage :: WS.Connection -> MVar ServerState -> Client -> ServerState -> Text -> IO ()
dispatchMessage conn state client clients msg =
    case msg of
        _   | not $ hasCorrectPrefix msg ->
                notifyWrongPrefix conn
            | isWrongName client ->
                notifyWrongName conn
            | clientExists client clients ->
                notifyUserExists conn
            | otherwise -> afterEverything (disconnect client state) $ do
                addClient' client state
                talk conn state client

client :: WS.Connection -> Text -> (Text, WS.Connection)
client conn msg = (T.drop (T.length connectionPrefix) msg, conn)

hasCorrectPrefix :: Text -> Bool
hasCorrectPrefix msg = connectionPrefix `T.isPrefixOf` msg

disconnect :: Client -> MVar ServerState -> IO ()
disconnect client state = do
    s <- removeClient' client state
    broadcast (fst client <> " disconnected") s

connectionPrefix :: Text
connectionPrefix = "Hi! I am "

afterEverything :: IO b -> IO a -> IO a
afterEverything = flip finally

notifyWrongPrefix :: WS.Connection -> IO ()
notifyWrongPrefix conn = WS.sendTextData conn ("Wrong announcement" :: Text)

notifyWrongName :: WS.Connection -> IO ()
notifyWrongName conn = WS.sendTextData conn ("Name cannot contain punctuation or whitespace, and cannot be empty" :: Text)

notifyUserExists :: WS.Connection -> IO ()
notifyUserExists conn = WS.sendTextData conn ("User already exists" :: Text)

isWrongName :: Client -> Bool
isWrongName c = any ($ fst c) [T.null, T.any isPunctuation, T.any isSpace]

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
