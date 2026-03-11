{-# LANGUAGE QuasiQuotes #-}

import Text.Nowdoc

main :: IO ()
main = putStr src

src :: String
src = [nowdoc|
$hello = "hello"
$world = "world"

puts $hello, $world
|]
