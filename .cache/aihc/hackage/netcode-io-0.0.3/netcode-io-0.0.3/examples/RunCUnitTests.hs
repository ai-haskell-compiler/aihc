module Main (main) where

import Bindings.Netcode.IO ( c'NETCODE_OK
                           , c'netcode_init, c'netcode_test, c'netcode_term
                           )

main :: IO ()
main = do
    initResult <- c'netcode_init
    if initResult == c'NETCODE_OK
      then return ()
      else fail "Failed to initialize netcode.io"
    c'netcode_test
    c'netcode_term
    putStrLn "Success!"
