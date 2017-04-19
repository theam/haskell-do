module HaskellDo.GUI.SimpleMDE.JavascriptInternals 
  ( SimpleMDE(..)
  , simpleMDE
  )
where

import BasicPrelude
import GHCJS.Types
import Data.JSString

import HaskellDo.GUI.SimpleMDE.Common

newtype SimpleMDE = SimpleMDE JSVal

foreign import javascript "$r = new SimpleMDE({ element: document.getElementById($1) });" 
  js_simpleMDE :: JSString -> IO JSVal


simpleMDE :: EditorConfig -> IO SimpleMDE
simpleMDE (EditorConfig eid) = 
  SimpleMDE <$> js_simpleMDE (pack eid)

