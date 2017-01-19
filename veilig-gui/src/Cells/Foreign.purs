module Cells.Foreign
  ( CodeEditor(..)
  , MarkdownEditor
  , Configuration
  , onChange
  , fromTextAreaCodeEditor
  , fromTextAreaMarkdownEditor
  )
where

import Prelude
import Data.Tuple
import Data.Foreign.Callback
import Control.Monad.Eff (Eff)
import Data.Function.Uncurried (Fn1)
import Signal.Channel (send, Channel, CHANNEL)
import DOM (DOM)

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
    ∀ a .
    CodeEditor ->
    Callback1 a Unit ->
    CodeMirrorEff Unit
