{-# LANGUAGE OverloadedStrings #-}
module Interpreter
  ( runInterpreter
  , notebookInterpreter
  ) where

import Types
import Language.Haskell.Interpreter
import Data.Text (Text)
import qualified Data.Text as T
import Data.String.Conversions

notebookInterpreter :: Notebook -> Interpreter Notebook
notebookInterpreter code = do
    setImportsQ[("Prelude",Nothing)]
    let newIds = [maximum (cellId <$> cells code) + 1 ..]
    newCells <- sequence $ do
      (c, newId) <- cells code `zip` newIds
      case cellType c of
        DisplayCell -> []
        TextCell -> pure $ pure c
        CodeCell -> do
          let str = cs <$> eval (cs $ cellContent c)
          [pure c, Cell DisplayCell newId <$> str]
    return $ code { cells = newCells}
