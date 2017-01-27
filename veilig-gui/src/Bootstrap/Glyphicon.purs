module Bootstrap.Glyphicon where

import Data.Show

data Glyphicon
    = GlyphiconPlus

instance showGlyphicon :: Show Glyphicon where
    show GlyphiconPlus = "glyphicon-plus"


