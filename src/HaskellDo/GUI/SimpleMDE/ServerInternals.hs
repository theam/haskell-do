module HaskellDo.GUI.SimpleMDE.ServerInternals
  ( SimpleMDE(..)
  , simpleMDE
  )
where

import BasicPrelude
import HaskellDo.GUI.SimpleMDE.Common

newtype SimpleMDE = SimpleMDE () deriving (Read, Show)

simpleMDE :: EditorConfig -> IO SimpleMDE
simpleMDE = throwBrowserError

throwBrowserError = error "This function is supposed to run on the browser" 
