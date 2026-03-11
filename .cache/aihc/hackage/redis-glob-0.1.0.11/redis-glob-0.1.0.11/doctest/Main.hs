-- file doctests.hs
import Test.DocTest


main :: IO ()
main = doctest ["src", "ascii-char"]
