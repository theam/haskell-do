module HaskellDo.GUI.SimpleMDE.JavascriptInternals
  ( SimpleMDE(..)
  , simpleMDE
  )
where

import BasicPrelude hiding (id, div, empty)
import GHCJS.Types
import Data.JSString

import GHCJS.HPlay.View hiding (map, option,input)

import HaskellDo.GUI.SimpleMDE.Common

newtype SimpleMDE = SimpleMDE JSVal

foreign import javascript unsafe "simpleMDE = new SimpleMDE(/*{ element: document.getElementById($1) }*/);"
  js_simpleMDE :: JSString -> IO ()

simpleMDE :: EditorConfig -> Widget String
simpleMDE (EditorConfig eid) = do
  rawHtml $ textarea ! id "mde" $ (pack "")
  _ <- rawHtml $ liftIO $ js_simpleMDE (pack "mde")
  return "LOLASO" -- TODO: Capture and return content
