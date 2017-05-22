module AxiomUtils where

import GHCJS.HPlay.View (setHtml, Attribute, Perch, Widget, BrowserEvent(..), a, (!), href, pass)
import qualified GHCJS.HPlay.View
import GHCJS.Types
import Data.JSString

setContents :: Perch -> String -> Perch
setContents element content = element `setHtml` (pack content :: JSString)

addHeader :: Perch -> IO ()
addHeader = GHCJS.HPlay.View.addHeader

atr :: String -> String -> Attribute
atr propName propValue = GHCJS.HPlay.View.atr propName (pack propValue :: JSString)

id :: String -> Attribute
id = atr "id"

at :: String -> GHCJS.HPlay.View.UpdateMethod -> GHCJS.HPlay.View.Widget a -> GHCJS.HPlay.View.Widget a
at s = GHCJS.HPlay.View.at (pack s :: JSString)

wlink :: a -> Perch -> Widget a
wlink x v =  do
    _ <- (a ! href "#"   $ v)  `pass` OnClick
    return x
