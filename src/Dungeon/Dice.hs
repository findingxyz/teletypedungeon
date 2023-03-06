{-# LANGUAGE OverloadedStrings #-}

module Dungeon.Dice
    ( roll
    )
    where

-- |
-- Module: Dungeon.Dice
--
-- Some functions to simulate rolling dice.

import System.Random (StdGen, mkStdGen, uniformR, split)
import Data.List (unfoldr)
import Control.Applicative ((<|>), many, optional)
import Data.Attoparsec.Text (Parser, char, decimal, skipSpace, parseOnly)
import Control.Monad.State.Strict (State, evalState, put, get)
import qualified Data.Text as T(Text)


type M a = State StdGen a

ss :: Parser ()
ss = skipSpace

number :: Parser [M Int]
number = (\x -> pure x : []) <$> decimal <|> (pure [return 100] <$> char '%')

expr :: M (Parser [M Int])
expr = do
    world <- get
    let evals' x xs = evalState (evals x xs) world
        rolls'      = evalState rolls world
    return $ evals' <$> rolls' <*> many (pure (,) <*> (ss *> (char '+' <|> char '-') <* ss) <*> rolls')

rolls :: M (Parser [M Int])
rolls = do
    world <- get
    let evals' x xs = evalState (evals x xs) world
        exprs'      = evalState exprs world
    return $ evals' <$> exprs' <*> many ((pure (,) <*> (ss *> (char 'd') <* ss)) <*> exprs')

exprs :: M (Parser [M Int])
exprs = do
    world <- get
    let evals' x xs = evalState (evals x xs) world
        factor'     = evalState factor world
    return $ evals' <$> factor' <*> many ((pure (,) <*> (ss *> (char '*') <* ss)) <*> factor')

factor :: M (Parser [M Int])
factor = do
    world <- get
    let expr' = evalState expr world
    return $ (pure f <*> optional (char 'd') <*> number) <|> (ss *> char '(' *> ss *> expr' <* ss <* char ')' <* ss)
        where f Nothing x = x
              --f _       x = evalState (evals [pure 1] [('d', x)]) world
              f _       x = evalState (evals [pure 1] [('d', x)]) (mkStdGen 1)

evals :: [M Int] -> [(Char, [M Int])] -> M [M Int]
evals x [] = return x
evals x (('+', x'):xs) = evals (map add [ (a, a') | a <- x, a' <- x' ]) xs
evals x (('-', x'):xs) = evals (map sub [ (a, a') | a <- x, a' <- x' ]) xs
evals x (('d', x'):xs) = do
    xs' <- mapM droll [ (a, a') | a <- x, a' <- x' ]
    evals (fmap pure xs') xs
evals x (('*', x'):xs) = do
    evals (concat $ map dupe [ (a, a') | a <- x, a' <- x' ]) xs
evals _ _ = error "wrong operator?"

droll :: (M Int, M Int) -> M Int
droll (x, y) = do
    x' <- x
    y' <- y
    seed <- get
    put $ (snd $ split seed)
    let roll'  = uniformR (1, y')
        rollin = unfoldr (Just . roll')
    return $ sum $ take x' (rollin seed)

add :: (M Int, M Int) -> M Int
add (x, y) = do
    seed <- get
    put $ (snd $ split seed)
    x' <- x
    y' <- y
    return $ x' + y'

sub :: (M Int, M Int) -> M Int
sub (x, y) = do
    seed <- get
    put $ (snd $ split seed)
    x' <- x
    y' <- y
    return $ x' - y'

dupe :: (M Int, M Int) -> [M Int]
dupe (x, y) = do
    let x' = evalState x (mkStdGen 42)
    replicate x' y


roll :: T.Text -> StdGen -> [Int]
roll e seed = case parseOnly (evalState expr seed) e of
                  (Left _)  -> [0]
                  (Right r) -> map (\x -> evalState x seed) r
