module Dungeon.Prompt where

import System.IO
import Control.Monad.State.Strict
import qualified Data.Text.IO as TIO

import Dungeon.World
import Dungeon.Commands
import Dungeon.Evaluator

prompt :: World -> IO ()
prompt world = do
    pprompt "command (exposit/elaborate/move)?"
    hFlush stdout
    input <- getLine
    case input of
        "exposit"   -> do
            exposit world
            prompt world
        "elaborate" -> do
            elaborate world
            prompt world
        "move"      -> do
            putStr "move to? "
            n <- getLine
            prompt $ execState (eval (Move (Number $ read n))) world
        "roll"      -> do
            putStr "roll what? "
            r <- TIO.getLine
            let expr   = Roll r
                e      = eval expr
                world' = snd $ runState e world
            --prompt $ execState (eval (Roll r)) world
            putStrLn $ show $ runState e world
            prompt world'
        _           -> do
            putStrLn "bad input, try again"
            prompt world

pprompt :: String -> IO ()
pprompt p = do
    putStrLn $ "┌─ " ++ p
    putStr   "└─› "
