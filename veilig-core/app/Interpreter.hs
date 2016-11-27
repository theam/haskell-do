{-# LANGUAGE OverloadedStrings #-}
module Interpreter where

import Types
import Language.Haskell.Interpreter
import Data.Text (Text)
import qualified Data.Text as T
import Data.String.Conversions

intNotebook :: Notebook -> Text
intNotebook= T.intercalate "\n"
              . map cellContent
              . filter (\c -> cellType c == CodeCell )
              . cells

notebookInterpreter :: Text -> Interpreter Text
notebookInterpreter code = do
    setImportsQ[("Prelude",Nothing)]
    a <- eval $ cs code
    return $ cs a
