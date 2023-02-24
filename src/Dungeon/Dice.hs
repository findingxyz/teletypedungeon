{-# LANGUAGE OverloadedStrings #-}

module Dungeon.Dice where

-- |
-- Module: Dungeon.Dice
--
-- Some functions to simulate rolling dice.

import System.Random (StdGen, uniformR, split)
import Data.List (unfoldr)
import Control.Applicative ((<|>), many)
import Data.Attoparsec.Text (Parser, char, digit, many1, skipSpace, parseOnly)
import Control.Monad.State.Strict


type M a = State StdGen a

ss :: Parser ()
ss = skipSpace

number :: Parser Int
number = read <$> many1 digit <|> (pure 100 <$> char '%')

expr :: M (Parser Int)
expr = do
    world <- get
    let dice'      = evalState dice world
        eval' x xs = evalState (eval x xs) world
    return $ pure eval' <*> dice' <*> many (pure (,) <*> (ss *> (char '+' <|> char '-') <* ss) <*> dice')

dice :: M (Parser Int)
dice = do
    world <- get
    let eval' x xs  = evalState (eval x xs) world
        factor'     = evalState factor world
    return $ pure eval' <*> factor' <*> many ((pure (,) <*> (ss *> (char 'd') <* ss) <*> factor'))

factor :: M (Parser Int)
factor = do
    world <- get
    let expr' = evalState expr world
    return $ number <|> ss *> char '(' *> ss *> expr' <* ss <* char ')' <* ss

eval :: Int -> [(Char,Int)] -> M Int
eval x [] = return x
eval x (('+', x'):xs) = do
    e <- eval (x + x') xs
    return e
eval x (('-', x'):xs) = do
    e <- eval (x - x') xs
    return e
eval x (('d', x'):xs) = do
    world <- get
    put $ (snd $ split world)
    e <- eval (fst $ roll' world (x, x')) xs
    return e
eval _ _ = error "ok"

roll' :: StdGen -> (Int, Int) -> (Int, StdGen)
roll' s (x, y) = let roll'' = uniformR (1, y)
                     rolls  = unfoldr (Just . roll'')
                 in (sum $ take x (rolls s), snd $ split s)

roll e seed = case parseOnly (evalState expr seed) e of
           (Left r)  -> 0
           (Right r) -> r
