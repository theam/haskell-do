module Editor.CodeMirror ( fromTextArea
                         , (:=)
                         , CODEMIRROR
                         ) where

import Prelude
import Data.Tuple
import Control.Monad.Eff (Eff)

foreign import data CODEMIRROR :: !
type CodeMirrorEff a = ∀ a e . Eff ( codemirror :: CODEMIRROR | e ) a
type TextAreaId = String
type Configuration = Array (Tuple String String)

infixr 0 Tuple as :=

fromTextArea ::
    TextAreaId ->
    Configuration ->
    CodeMirrorEff Unit
fromTextArea id' config =
    fromTextAreaImpl id' ar
  where ar = map (\x -> [fst x, snd x]) config

foreign import fromTextAreaImpl :: TextAreaId -> Array (Array String) -> CodeMirrorEff Unit
