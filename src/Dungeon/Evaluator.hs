{-# LANGUAGE OverloadedStrings #-}

module Dungeon.Evaluator where

import Data.Graph.Inductive
import Control.Monad.State.Strict
import qualified Dungeon.Dice as D
import System.Random (split)
import qualified Data.Text as T

import Dungeon.World
import Dungeon.World.Player


data Expression = Number Int
                | Move   Expression
                | Roll   T.Text
                deriving Show

data Value = NumberVal  Int
           | NumbersVal [Int]
           | PlayerVal  Player
           | RoomVal    Room
           | DungeonVal Dungeon
           | FailVal    String
           deriving Show

--type M a     = State World a

eval :: Expression -> State World Value
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

