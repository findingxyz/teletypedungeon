module Dungeon.World.Description where

import Data.Text (Text)

data Description = Description { dName        :: Text
                               , dDescription :: Text
                               }
                               deriving Show
