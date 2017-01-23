module Notebook.Types where

import Cells.Types as Cells
import Data.Generic
import Data.Argonaut

data Notebook = Notebook
    { title :: String
    , subtitle :: String
    , date :: String
    , author :: String
    , cells :: Array Cells.Cell
    , console :: String
    }

derive instance genericNotebook :: Generic Notebook

instance encodeJsonNotebook :: EncodeJson Notebook where
    encodeJson = gEncodeJson

instance decodeJsonNotebook :: DecodeJson Notebook where
    decodeJson = gDecodeJson