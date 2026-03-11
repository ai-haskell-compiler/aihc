module Main (main) where

import Bindings.Reliable.IO ( c'RELIABLE_OK
                            , c'reliable_init, c'reliable_test, c'reliable_term
                            )

main :: IO ()
main = do
    initResult <- c'reliable_init
    if initResult == c'RELIABLE_OK
      then return ()
      else fail "Failed to initialize reliable.io"
    c'reliable_test
    c'reliable_term
    putStrLn "Success!"