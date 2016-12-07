module Editor.CodeMirror where

import Prelude
import Data.Tuple
import Data.Foreign.Callback
import Control.Monad.Eff (Eff)
import Data.Function.Uncurried (Fn1)
import Signal.Channel (send, Channel, CHANNEL)

foreign import data CODEMIRROR :: !
foreign import data CodeEditor :: *
type MarkdownEditor = { codemirror :: CodeEditor }
type CodeMirrorEff a = ∀ a e . Eff ( codemirror :: CODEMIRROR | e ) a
type TextAreaId = String
type Configuration =
    { mode :: String
    }

infixr 0 Tuple as :=

onChange ::
    ∀ a e .
    CodeEditor ->
    Channel a ->
    (String -> a) ->
    Eff ( codemirror :: CODEMIRROR, channel :: CHANNEL | e ) Unit
onChange editor chan f = do
    let cb = callback1 (\x -> send chan (f x))
    _onChange editor cb

foreign import fromTextArea :: TextAreaId -> Configuration -> CodeMirrorEff CodeEditor
foreign import fromTextAreaMarkdownEditor :: TextAreaId -> CodeMirrorEff MarkdownEditor
foreign import _onChange ::
    ∀ a .
    CodeEditor ->
    Callback1 a Unit ->
    CodeMirrorEff Unit
