{-# LANGUAGE QuasiQuotes #-}

import Text.Nowdoc

main :: IO ()
main = putStr [nowdoc|
hello | ]
world |  ]
|]
