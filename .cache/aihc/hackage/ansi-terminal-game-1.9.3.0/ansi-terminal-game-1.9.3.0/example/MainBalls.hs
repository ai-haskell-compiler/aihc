module Main where

import Balls

import Terminal.Game

-- Balls Main module. The meat of the game is in `examples/Balls.hs`

main :: IO ()
main = do
        g <- getStdGen
        r <- playGame (fireworks g)
            -- We use game result `r` (how many balls were on
            -- screen) and feed it to another function.
            -- This could be useful to upload high scores to
            -- a site, or for a game embedded in a larger pro-
            -- gram, etc.
        putStrLn (bye r)
    where
          bye wi = "See you later!\nYou left the game with " ++
                   show wi ++ " balls on screen."

