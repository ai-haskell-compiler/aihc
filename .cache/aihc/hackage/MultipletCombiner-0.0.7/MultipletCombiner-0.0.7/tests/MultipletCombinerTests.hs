module Main (main) where
import System.Exit

import Test.HUnit
import Physics.MultipletCombiner

main :: IO ()
main = do
    counts <- runTestTT tests
    if (errors counts + failures counts == 0)
        then exitSuccess
        else exitFailure


comb1 = TestCase (assertEqual "[1] >< [1], " [[2],[0]] ([1] >< [1]))
comb2 = TestCase (assertEqual "[1,0] >< [0,1], " [[1,1],[0,0]] ([1,0] >< [0,1]))

multi1 = TestCase (assertEqual "n in [2], " 3 (multi [2]))
multi2 = TestCase (assertEqual "n in octet [1,1], " 8 (multi [1,1]))
multi3 = TestCase (assertEqual "n in decuplet [3,0], " 10 (multi [3,0]))
multi4 = TestCase (assertEqual "empty" 0 (multi []))

yt1 = TestCase (assertEqual "yt [0],   " "# \n# \n"
        (show $ ytSymbols [0]))
yt2 = TestCase (assertEqual "yt [1],   " "# # \n# \n"
        (show $ ytSymbols [1]))
yt3 = TestCase (assertEqual "yt [0,0], " "# \n# \n# \n"
        (show $ ytSymbols [0,0]))
yt4 = TestCase (assertEqual "yt [1,0], " "# # \n# \n# \n"
        (show $ ytSymbols [1,0]))
yt5 = TestCase (assertEqual "yt [2,1], " "# # # # \n# # \n# \n"
        (show $ ytSymbols [2,1]))

pot1 = TestCase (assertEqual "]1] ><^ 0" [] ([1] ><^ 0))
pot2 = TestCase (assertEqual "[] ><^ 3" [] ([] ><^ 3))
pot3 = TestCase (assertEqual "[1] ><^ 3" ([1] >< [1] >>< [1]) ([1] ><^ 3))
pot4 = TestCase (assertEqual "[1,0] ><^ 3" ([1,0] >< [1,0] >>< [1,0])
                                                            ([1,0] ><^ 3))

tests = TestList [TestLabel "comb1" comb1,
                TestLabel "comb2" comb2,
                TestLabel "multi1" multi1,
                TestLabel "multi2" multi2,
                TestLabel "multi3" multi3,
                TestLabel "multi4" multi4,
                TestLabel "yt1" yt1,
                TestLabel "yt2" yt2,
                TestLabel "yt3" yt3,
                TestLabel "yt4" yt4,
                TestLabel "yt5" yt5,
                TestLabel "pot1" pot1,
                TestLabel "pot2" pot2,
                TestLabel "pot3" pot3,
                TestLabel "pot4" pot4]



