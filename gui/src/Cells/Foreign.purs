module Cells.Foreign where

import Prelude
import Data.Tuple
import Data.Foreign.Callback
import Cells.Types
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import DOM (DOM)
import Data.Function.Uncurried (Fn1)
import Signal.Channel (send, Channel, CHANNEL)

-- | The reference to a CodeMirror editor
foreign import data CodeEditor :: *

-- | The reference to a SimpleMDE editor
type MarkdownEditor = { codemirror :: CodeEditor }

type TextAreaId = String
type Configuration =
    { mode :: String
    }

-- | Hooks the function `f` into the Channel `chan` if `editor` content changes
onChange ::
    ∀ a e .
    CodeEditor ->
    Channel a ->
    (String -> a) ->
    Eff ( dom :: DOM, channel :: CHANNEL | e ) Unit
onChange editor chan f = do
    let cb = callback1 (\x -> send chan (f x))
    _onChange editor cb

-- | Hooks the function `f` into the Channel `chan` if `editor` content changes
onClick ::
    ∀ a e .
    CodeEditor ->
    Channel a ->
    a ->
    Eff ( dom :: DOM, channel :: CHANNEL | e ) Unit
onClick editor chan f = do
    let cb = callback0 $ send chan f
    _onClick editor cb


-- | Converts a text area into a CodeMirror code editor
foreign import fromTextAreaCodeEditor ::
    forall e .
    TextAreaId ->
    Configuration ->
    Eff ( dom :: DOM | e ) CodeEditor

-- | Converts a text area into a SimpleMDE markdown editor
foreign import fromTextAreaMarkdownEditor ::
    forall e .
    TextAreaId ->
    Eff (dom :: DOM | e) MarkdownEditor

foreign import _onChange ::
    ∀ a e .
    CodeEditor ->
    Callback1 a Unit ->
    Eff ( dom :: DOM | e) Unit

foreign import _onClick ::
    ∀ a e .
    CodeEditor ->
    Callback0 Unit ->
    Eff ( dom :: DOM | e) Unit

foreign import toggleEditor :: forall e . MarkdownEditor -> Eff ( dom :: DOM | e) Unit
 
makeCodeEditor :: ∀ eff . Channel Action -> CellId -> Eff ( channel :: CHANNEL, dom :: DOM | eff ) Action
makeCodeEditor chan i = do
    editor <- liftEff $ fromTextAreaCodeEditor (show i) { mode : "haskell" }
    onChange editor chan (\code -> SaveContent i code)
    onClick editor chan $ SetCurrentCell i
    pure NoOp

makeTextEditor :: ∀ eff . Channel Action -> CellId -> Eff (channel :: CHANNEL, dom :: DOM | eff ) Action
makeTextEditor chan i = do
    editor <- fromTextAreaMarkdownEditor (show i)
    onChange editor.codemirror chan (\txt -> SaveContent i txt)
    onClick editor.codemirror chan $ SetCurrentCell i
    pure NoOp

loadTextEditor :: ∀ eff . Channel Action -> CellId -> Eff (channel :: CHANNEL, dom :: DOM | eff ) Action
loadTextEditor chan i = do
    editor <- fromTextAreaMarkdownEditor (show i)
    toggleEditor editor
    onChange editor.codemirror chan (\txt -> SaveContent i txt)
    onClick editor.codemirror chan $ SetCurrentCell i
    pure NoOp


