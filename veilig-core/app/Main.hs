module Main where

import Types
import WebSocketServer
import Language.Haskell.GhcMod
import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Monad.Out
import Language.Haskell.GhcMod.Monad.Log
import qualified Network.WebSockets as WS
import Control.Concurrent
import System.Environment

-- needs these instances for findCradle to work
-- findCradle :: (GmLog m, IOish m, GmOut m) => Programs -> Cradle
instance GmOut IO where
  gmoAsk = liftIO gmoAsk

instance GmLog IO where
  gmlJournal = liftIO . gmlJournal
  gmlHistory = liftIO gmlHistory
  gmlClear = liftIO gmlClear

address :: String
address = "0.0.0.0"

port :: Int
port = 3000

main :: IO ()
main = do
  projecname : _ <- getArgs
  cradle <- findCradle $ Programs { stackProgram = projectname }
  state <- initializeState cradle
  WS.runServer address port (application state)
