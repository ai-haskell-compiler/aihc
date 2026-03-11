-- file doctests.hs
import Test.DocTest


main :: IO ()
main = doctest ["-isrc", "src/Test/Hspec/Benri.hs"]
