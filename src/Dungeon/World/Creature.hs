module Dungeon.World.Creature where

import Dungeon.World.Description

data Creature = Creature { cDescription :: Description
                         , cHp          :: Int
                         , cMaxHp       :: Int
                         }
                         deriving Show


