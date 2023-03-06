{-# LANGUAGE OverloadedStrings #-}

module DungeonWorld where

import Data.Graph.Inductive
import System.Random (getStdGen)

import Dungeon.Prompt
import Dungeon.World
import Dungeon.World.Description
import Dungeon.World.Creature
import Dungeon.World.Player


test :: IO ()
test = do
    sed <- getStdGen
    let room = case match 1 aLittleDungeon of
                  (Just spawn, _) -> spawn
                  (Nothing, _)  -> ([], 0, ((Description ":(" "It failed."), []), [])
        newPlayer     = Player room (Creature (Description "Player" "The player.") 15 15)
        world         = World newPlayer aLittleDungeon sed
    prompt world

aLittleDungeon :: Dungeon
aLittleDungeon = let gobbo    = Creature (Description "Goblin" "Small and green.") 6 6
                     dragon   = Creature (Description "Dragon" "Big and maybe green.") 60 60
                     bob      = Creature (Description "Bob" "This is Bob. She's got my back. She can cut all of you in half with one sword stroke, just like mowing the lawn. I would advise not getting killed by her. Her sword traps the souls of its victims.") 1 1
                     litNodes = [ (1, ((Description "Entrance" "It has one door next to Bob."), [bob]))
                                , (2, ((Description "Corridor" "It's a bit dank."), []))
                                , (3, ((Description "Corridor" "It's a bit humid."), []))
                                , (4, ((Description "Intersection" "You hear some goblins on the left and absolutely nothing on the right."), []))
                                , (5, ((Description "Goblin Hideout" "There's a red stain where you're standing."), replicate 5 gobbo))
                                , (6, ((Description "Absolutely nothing" "Nothing here, you're trapped."), []))
                                , (7, ((Description "Intersection to Dragon?" "It's getting hotter on the left."), [gobbo]))
                                , (8, ((Description "Prelude to Dragon" "It's a bit hot."), [gobbo, gobbo]))
                                , (9, ((Description "No dragons here" "Turn back."), []))
                                , (10, ((Description "Dragon" "DRAGON!"), [dragon]))
                                , (11, ((Description "Exit" "Behind you is a bit hot, but at least the way forwards is nice and chilly"), []))
                                ]
                     litEdges = [ (1, 2, 1)
                              , (2, 3, 1)
                              , (3, 4, 1)
                              , (4, 5, 1)
                              , (4, 6, 1)
                              , (5, 7, 1)
                              , (7, 8, 1)
                              , (7, 9, 1)
                              , (8, 10, 1)
                              , (9, 7, 1)
                              , (10, 11, 1)
                              ]
                     in insEdges litEdges $ insNodes litNodes empty
                     
