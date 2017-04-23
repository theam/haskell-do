module HaskellDo.GUI.SimpleMDE.ServerInternals
  ( initializeSimpleMDE
  , simpleMDE
  )
where

import BasicPrelude
import HaskellDo.GUI.SimpleMDE.Common

import GHCJS.HPlay.View hiding (map, option,input)

newtype SimpleMDE = SimpleMDE () deriving (Read, Show)

simpleMDE :: Widget String
simpleMDE = throwBrowserError "simpleMDE"

initializeSimpleMDE :: EditorConfig -> IO ()
initializeSimpleMDE _ = return ()

throwBrowserError :: String -> a
throwBrowserError fName = error $ fName ++ ": This function is supposed to run on the browser"
