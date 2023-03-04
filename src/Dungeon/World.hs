{-# LANGUAGE OverloadedStrings #-}

module Dungeon.World where


import Data.Graph.Inductive
import System.Random (StdGen)

import Dungeon.World.Player
import Dungeon.World.Description
import Dungeon.World.Creature

data World   = World { player  :: Player
                     , dungeon :: Dungeon
                     , seed    :: StdGen
                     }
                     deriving Show

type Dungeon = Gr Area Int

type Area    = (Description, [Creature])

type Room    = Context Area Int
