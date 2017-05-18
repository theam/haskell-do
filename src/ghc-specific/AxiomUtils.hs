module AxiomUtils where

import GHCJS.HPlay.View hiding (id, atr)

setContents :: Perch -> String -> Perch
setContents element _ = element

addHeader :: Perch -> IO ()
addHeader _ = return ()

atr :: String -> String -> (String, String)
atr = (,)

id :: String -> (String, String)
id = atr "id"

at :: String -> UpdateMethod -> Widget a -> Widget a
at _ _ w = w

wlink :: (Show a) => a -> Perch -> Widget a
wlink _ _= empty
