module HaskellDo.GUI.SimpleMDE.ServerInternals
  ( SimpleMDE(..)
  , simpleMDE
  )
where

import BasicPrelude
import HaskellDo.GUI.SimpleMDE.Common

import GHCJS.HPlay.View hiding (map, option,input)

newtype SimpleMDE = SimpleMDE () deriving (Read, Show)

simpleMDE :: EditorConfig -> Widget String
simpleMDE = throwBrowserError

throwBrowserError :: a
throwBrowserError = error "This function is supposed to run on the browser"
