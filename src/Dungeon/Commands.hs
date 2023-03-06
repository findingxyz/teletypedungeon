module Dungeon.Commands where

import System.IO (hFlush, stdout)
import qualified Data.Text.IO as TIO(putStrLn)

import Dungeon.World
import Dungeon.World.Player
import Dungeon.World.Description

exposit :: World -> IO ()
exposit world = do
    let (_, _, (area, _), _) = pAt $ player world
    TIO.putStrLn $ dName area
    TIO.putStrLn $ dDescription area

elaborate :: World -> IO ()
elaborate world = do
    --prompt "elaborate what? (here/from/to/creature/creatures)"
    putStrLn "elaborate what? (here/from/to/creature/creatures)"
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
