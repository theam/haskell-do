module Notebook.Types where

import Prelude

import Cells.Types as Cells
import Data.Generic
import Data.Argonaut

newtype Notebook = Notebook
    { title :: String
    , subtitle :: String
    , date :: String
    , author :: String
    , cells :: Array Cells.Cell
    , console :: String
    }

instance encodeJsonNotebook :: EncodeJson Notebook where
    encodeJson (Notebook n)
        = "title" := n.title
       ~> "subtitle" := n.subtitle
       ~> "date" := n.date
       ~> "author" := n.author
       ~> "cells" := n.cells
       ~> "console" := n.console
       ~> jsonEmptyObject

instance decodeJsonNotebook :: DecodeJson Notebook where
    decodeJson json = do
        o <- decodeJson json
        title <- o .? "title"
        subtitle <- o .? "subtitle"
        date <- o .? "date"
        author <- o .? "author"
        cells <- o.? "cells"
        console <- o.? "console"
        pure $ Notebook {title, subtitle, date, author, cells, console}