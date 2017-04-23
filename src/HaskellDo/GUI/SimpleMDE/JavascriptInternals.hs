{-# LANGUAGE NoMonomorphismRestriction #-}
module HaskellDo.GUI.SimpleMDE.JavascriptInternals
  ( simpleMDE
  , initializeSimpleMDE
  )
where

import BasicPrelude hiding (id, div, empty)
import GHCJS.Types
import Data.JSString

import GHCJS.HPlay.View hiding (map, option,input)

import HaskellDo.GUI.SimpleMDE.Common

foreign import javascript unsafe "simpleMDE.value()"
    js_getMDEContent :: IO JSString

initializeSimpleMDE :: EditorConfig -> IO ()
initializeSimpleMDE (EditorConfig eid) =
    addHeader $ do
        link ! atr (fromString "rel") (fromString "stylesheet")
             ! href (fromString "https://cdn.jsdelivr.net/simplemde/latest/simplemde.min.css")
        script (pack "var simpleMDE;" :: JSString)
        script ! src (fromString "https://cdn.jsdelivr.net/simplemde/latest/simplemde.min.js")
               $ noHtml
        script (pack "function initMDE() {if (typeof SimpleMDE !== 'undefined') {simpleMDE=new SimpleMDE();}else{window.setTimeout(initMDE, 10);}};initMDE()" :: JSString)

simpleMDE :: Widget String
simpleMDE = do
    textArea "" `fire` OnKeyUp `fire` OnClick
    content <- liftIO js_getMDEContent
    return $ unpack content
