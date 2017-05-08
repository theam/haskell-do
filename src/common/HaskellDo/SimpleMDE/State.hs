module HaskellDo.SimpleMDE.State where

import Transient.Move
import HaskellDo.SimpleMDE.Types

initialState :: State
initialState = State
    { content = ""
    }

update :: Action -> State -> Cloud State
update (NewContent newContent) state = do
    return (state { content = newContent } )
