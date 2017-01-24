module App.State where

import Prelude
import Data.Traversable (traverse)

import Cells.State   as Cells
import Cells.Types   as Cells
import Columns.State as Columns
import Columns.Types as Columns
import Console.State as Console
import Console.Types as Console
import BackendConnection.State as BackendConnection
import BackendConnection.Types as BackendConnection
import App.Types
import Notebook.Types
import Data.Array
import Data.Lens
import Data.Lens.Index
import Data.Lens.Setter
import Data.Array
import Data.Maybe
import Data.String as Str
import Control.Monad.Aff
import Control.Monad.Eff
import WebSocket
import Data.Either
import Data.Tuple
import Data.String as S
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Var (($=))
import Data.Lens.Traversal (traversed)
import Pux (noEffects)
import Signal.Channel (CHANNEL, send, Channel)
import Data.Argonaut hiding ((:=))
import DOM
import Pux

initialNotebook :: Notebook
initialNotebook = Notebook
  { title: ""
  , subtitle: ""
  , author: ""
  , date: ""
  , cells: [] :: Array Cells.Cell
  , console: ">"
  }

update :: Action -> State -> EffModel State Action (ws :: WEBSOCKET, dom :: DOM)
update (CellsAction action) as = updateCells as $ Cells.update action (view cellsState as)
update (ColumnsAction action) as           = mapEffects ColumnsAction $ Columns.update action (view columnsState as)
update (ConsoleAction action) as           = mapEffects ConsoleAction $ Console.update action (view consoleState as)
update (BackendConnectionAction action) as = mapEffects BackendConnectionAction $ BackendConnection.update action (view backendConnectionState as)
update NoOp appState = noEffects $ appState

updateCells :: ∀ action substate eff . State -> EffModel Cells.State action eff -> EffModel State Action eff
updateCells st subState =
    { state : (cellsState .~ subst) st
    , effects  : mapEffAction CellsAction ?what
    }
  where
    subst = subState.state
    subeffs = subState.effects

mapEffAction :: ∀ eff subaction superaction .
              (subaction -> superaction) ->
              Array (Aff eff subaction) ->
              Array (Aff eff superaction)
mapEffAction superActionConstructor = map convertToSuperAction
  where
    convertToSuperAction subAction = subAction >>= (\sa -> pure $ superActionConstructor sa)
