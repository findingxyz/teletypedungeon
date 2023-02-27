{-# LANGUAGE OverloadedStrings #-}

module DungeonWorld where

import System.IO
import Data.Graph.Inductive
import Control.Monad.State.Strict
import qualified Dungeon.Dice as D
import System.Random (StdGen, split, getStdGen)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO


data World = World { player  :: Player
                   , dungeon :: Dungeon
                   , seed    :: StdGen
                   }
                   deriving Show

data Player = Player { pAt       :: Room
                     , pCreature :: Creature
                     }
                     deriving Show

data Creature = Creature { cDescription :: Description
                         , cHp          :: Int
                         , cMaxHp       :: Int
                         }
                         deriving Show

data Description = Description { dName        :: Name
                               , dDescription :: String
                               }
                               deriving Show

data Expression = Number Int
                | Move   Expression
                | Roll   T.Text
                deriving Show

data Value = NumberVal  Int
           | NumbersVal [Int]
           | RollVal    T.Text
           | PlayerVal  Player
           | RoomVal    Room
           | DungeonVal Dungeon
           | FailVal    String
           deriving Show

type M a     = State World a

type Name    = String

type Area    = (Description, [Creature])

type Dungeon = Gr Area Int

type Room    = Context Area Int

eval :: Expression -> M Value
eval (Number n) = return $ NumberVal n
eval (Move e)   = do
    evaled <- eval e
    world <- get
    let (_, _, _, s) = (pAt $ player world)
        rooms        = map snd s
        n            = case evaled of
                           (NumberVal num) -> num
                           _               -> 0
    case n `elem` rooms of
        True -> do
            let at = case match n (dungeon world) of
                         (Just mAt, _) -> mAt
                         (Nothing, _)  -> pAt $ player world -- something went very wrong if this happens
            put $ world { player = (player world) { pAt = at } }
            return $ RoomVal at
        False -> return $ FailVal ("not found: " ++ show n
                                ++ " in rooms: " ++ show rooms)
eval (Roll e)   = do
    world <- get
    put $ world { seed = snd $ split (seed world) }
    return $ NumbersVal $ D.roll e (seed world)

exposit :: World -> IO ()
exposit world = do
    let (_, _, (area, _), _) = pAt $ player world
    putStrLn $ dName area
    putStrLn $ '\n' : dDescription area
    gogogo world

elaborate :: World -> IO ()
elaborate world = do
    prompt "elaborate what? (here/from/to/creature/creatures)"
    hFlush stdout
    what <- getLine
    case what of
        "here"       -> do
            let (ps, _, area, ss) = pAt $ player world
            putStrLn $ "contains " ++ (show $ length $ snd area) ++ " creatures"
            putStrLn $ "from "
                    ++ (show $ length ps)
                    ++ " rooms. goes to "
                    ++ (show $ length ss)
                    ++ " rooms."
        "from"       -> do
            putStr "this room came from one of "
            let (from, _, _, _) = pAt $ player world
            putStrLn $ show $ map snd from
        "to"         -> do
            putStr "this room can go to one of "
            let (_, _, _, to) = pAt $ player world
            putStrLn $ show $ map snd to
        "creature"   -> do
            putStrLn "ok"
        "creatures"  -> do
            let (_, _, area, _) = pAt $ player world
            mapM_ (putStrLn . show) $ snd area
        _            -> do
            putStrLn "bad input, try again"
            elaborate world

gogogo :: World -> IO ()
gogogo world = do
    prompt "command (exposit/elaborate/move)?"
    hFlush stdout
    input <- getLine
    case input of 
        "exposit"   -> do
            exposit world
            gogogo world
        "elaborate" -> do
            elaborate world
            gogogo world
        "move"      -> do
            putStr "move to? "
            n <- getLine
            gogogo $ execState (eval (Move (Number $ read n))) world
        "roll"      -> do
            putStr "roll what? "
            r <- TIO.getLine
            let expr   = Roll r
                e      = eval expr
                world' = snd $ runState e world
            --gogogo $ execState (eval (Roll r)) world
            putStrLn $ show $ runState e world
            gogogo world'
        _           -> do
            putStrLn "bad input, try again"
            gogogo world

test :: IO ()
test = do
    --hSetBuffering stdin NoBuffering
    sed <- getStdGen
    let room = case match 1 aLittleDungeon of
                  (Just spawn, _) -> spawn
                  (Nothing, _)  -> ([], 0, ((Description ":(" "It failed."), []), [])
        newPlayer     = Player room (Creature (Description "Player" "The player.") 15 15)
        world         = World newPlayer aLittleDungeon sed
    gogogo world

prompt :: String -> IO ()
prompt p = do
    putStrLn $ "┌─ " ++ p
    putStr   "└─› "

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
                     
