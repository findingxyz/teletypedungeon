module Dungeon.World.Player where

import Data.Graph.Inductive
import Dungeon.World.Creature
import Dungeon.World.Description

data Player = Player { pAt       :: Context (Description, [Creature]) Int
                     , pCreature :: Creature
                     }
                     deriving (Show)
