{-# LANGUAGE QuasiQuotes #-}

import Text.Nowdoc

main :: IO ()
main = putStr [nowdoc|
main :: IO ()
main = putStr [nowdoc|
Hello, world!
| ]
|]
