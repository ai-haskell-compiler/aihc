import Test.DocTest (doctest)

main :: IO ()
main = doctest
   [ "-isrc"
   , "src/Data/Time/Clock/Duration.hs"
   , "src/Data/Time/Clock/Duration/QQ.hs"
   , "src/Data/Time/Clock/Duration/Types.hs"
   ]
