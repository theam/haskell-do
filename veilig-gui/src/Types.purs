module Types where

data Action
  = ToggleEdit

type AppState =
  { editing :: Boolean
  , rawText :: String
  , renderedText :: String
  }
