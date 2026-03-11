{-# LANGUAGE QuasiQuotes #-}

import Text.Nowdoc

main :: IO ()
main = print [binfile| samples/sample.dat |]
