{-# LANGUAGE QuasiQuotes #-}

import Text.Nowdoc

main :: IO ()
main = putStr [txtfile|
	samples/sample.txt |]
