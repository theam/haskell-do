module Editor.CodeMirror ( fromTextArea
                         , onChange
                         , (:=)
                         , CODEMIRROR
                         , CodeMirrorEff
                         , CodeEditor
                         ) where

import Prelude
import Data.Tuple
import Control.Monad.Eff (Eff)
import Data.Function.Uncurried (Fn1)

foreign import data CODEMIRROR :: !
foreign import data CodeEditor :: *
type CodeMirrorEff a = ∀ a e . Eff ( codemirror :: CODEMIRROR | e ) a
type TextAreaId = String
type Configuration a =
    ∀ a .
    { mode :: String
    , onChange :: Fn1 String a
    }

infixr 0 Tuple as :=

fromTextArea ::
    ∀ a .
    TextAreaId ->
    Configuration a ->
    CodeMirrorEff CodeEditor
fromTextArea id' config =
    fromTextAreaImpl id' config

foreign import fromTextAreaImpl :: ∀ a . TextAreaId -> Configuration a -> CodeMirrorEff CodeEditor
foreign import onChange :: ∀ a . CodeEditor -> (String -> a) -> CodeMirrorEff a
