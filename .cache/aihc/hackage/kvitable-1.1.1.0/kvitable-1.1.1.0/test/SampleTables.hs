{-# LANGUAGE OverloadedStrings #-}

module SampleTables where

import           Data.KVITable
import           Data.String ( fromString )
import           Data.Text ( Text )
import qualified Data.Text as T
import           Lens.Micro ( (^.), (.~), (%~), (&) )


listing1 :: [ ( [(Key, KeyVal)], Text ) ]
listing1 =
  [ ( [("foo", "bar"), ("moo", "cow")], "one" )
  , ( [("moo", "cow"), ("foo", "bar"), ("goose", "honk")], "two" )
  , ( [("moo", "cow")], "three" )
  , ( [("foo", "baz"), ("moo", "cow")], "four")
  ]
kvi1 = fromList listing1
kvi1_1 = fromList $ take 2 listing1
kvi1_2 = fromList $ drop 2 listing1

mediumKVI = foldl foldlInsert
            (mempty
              & keyVals .~ [ ("compiler", [ "gcc7", "gcc8", "clang6", "clang10", "clang7" ])
                           , ("debug", [ "yes", "no" ])
                           , ("optimization", [ "0", "1", "3" ])
                           ])
            [ ([("compiler", "gcc7"), ("debug", "yes"), ("optimization", "0")], "good")
            , ([("compiler", "gcc7"), ("debug", "no" ), ("optimization", "0")], "bad")
            , ([("compiler", "gcc7"), ("debug", "yes"), ("optimization", "3")], "ugly")
            , ([("compiler", "gcc8"), ("debug", "yes"), ("optimization", "0")], "good")
            , ([("compiler", "clang6"), ("debug", "yes"), ("optimization", "0")], "ok")
            , ([("compiler", "clang7"), ("debug", "no"), ("optimization", "1")], "good")
            , ([("compiler", "clang7"), ("debug", "no"), ("optimization", "3")], "good")
            , ([("compiler", "clang7"), ("debug", "yes"), ("optimization", "3")], "good")
            , ([("compiler", "clang10"), ("debug", "no"), ("optimization", "3")], "good")
            , ([("compiler", "clang10"), ("debug", "yes"), ("optimization", "3")], "good")
            , ([("compiler", "gcc8"), ("debug", "yes"), ("optimization", "3")], "true")
            , ([("compiler", "gcc8"), ("debug", "yes"), ("optimization", "1")], "bad")
            , ([("compiler", "clang7"), ("debug", "no"), ("optimization", "0")], "good")
            , ([("compiler", "gcc7"), ("debug", "no"), ("optimization", "1")], "good")
            ]

-- this table has floating values and keys with spaces
ptable = foldl foldlInsert
         (mempty & valueColName .~ "Annual Rainfall")
         [ ([("City name", "Adelaide"), ("Area", "1295"), ("Population", "1158259")], 600.5)
         , ([("City name", "Brisbane"), ("Area", "5905"), ("Population", "1857594")], 1146.4)
         , ([("City name", "Darwin"), ("Area", "112"), ("Population", "120900")], 1714.7)
         , ([("City name", "Hobart"), ("Area", "1357"), ("Population", "205556")], 619.5)
         , ([("City name", "Melbourne"), ("Area", "1566"), ("Population", "3806092")], 646.9)
         , ([("City name", "Perty"), ("Area", "5386"), ("Population", "1554769")], 869.4)
         , ([("City name", "Sydney"), ("Area", "2058"), ("Population", "4336374")], 1214.8)
         ]

-- big and complicated
zooTable = foldl foldlInsert
           (mempty & valueColName .~ "Count"
             & keyVals .~ [ ("Location", ["San Diego", "LA", "Miami", "New York"])
                          , ("Biome",    ["Savannah", "Jungle", "Polar"])
                          , ("Category", ["Animal", "Reptile", "Bird"])
                          , ("Diet",     ["Herbivore", "Carnivore"])
                          , ("Name",     [])
                          ])
           [ ([ ("Diet", "Carnivore")
              , ("Category", "Animal")
              , ("Biome", "Savannah")
              , ("Name", "Lion")
              , ("Location", "New York")], 3)
           , ([ ("Diet", "Carnivore")
              , ("Category", "Animal")
              , ("Biome", "Savannah")
              , ("Name", "Lion")
              , ("Location", "Miami")], 2)
           , ([ ("Diet", "Carnivore")
              , ("Category", "Animal")
              , ("Biome", "Savannah")
              , ("Name", "Lion")
              , ("Location", "LA")], 4)
           , ([ ("Diet", "Carnivore")
              , ("Category", "Animal")
              , ("Biome", "Savannah")
              , ("Name", "Lion")
              , ("Location", "San Diego")], 8)
           , ([ ("Location", "LA")
              , ("Biome", "Savannah")
              , ("Category", "Animal")
              , ("Name", "Giraffe")
              , ("Diet", "Herbivore")], 2)
           , ([ ("Location", "LA")
              , ("Biome", "Jungle")
              , ("Category", "Animal")
              , ("Name", "Hippo")
              , ("Diet", "Herbivore")], 1)
           , ([ ("Location", "LA")
              , ("Biome", "Savannah")
              , ("Category", "Animal")
              , ("Diet", "Herbivore")
              , ("Name", "Rhino")], 3)
           , ([ ("Location", "Miami")
              , ("Biome", "Polar")
              , ("Category", "Bird")
              , ("Diet", "Carnivore")
              , ("Subtype", "Gentoo")  -- new key
              , ("Name", "Penguin")], 20)
           , ([ ("Location", "San Diego")
              , ("Biome", "Polar")
              , ("Category", "Bird")
              , ("Diet", "Carnivore")
              , ("Subtype", "Emperor")
              , ("Name", "Penguin")], 8)
           , ([ ("Location", "San Diego")
              , ("Biome", "Polar")
              , ("Category", "Bird")
              , ("Diet", "Carnivore")
              , ("Subtype", "Gentoo")
              , ("Name", "Penguin")], 2)
           , ([ ("Location", "Miami")
              , ("Biome", "Savannah")
              , ("Category", "Animal")
              , ("Diet", "Herbivore")
              , ("Name", "Giraffe")
              , ("Subtype", "Reticulated")], 3)
           ]
zooTable2 = insert [ ("Location", "San Diego"), ("Biome", "Plains")
                   , ("Category", "Animal"), ("Subtype", "Black")
                   , ("Name", "Bear"), ("Diet", "Omnivore") ] 1 $
            insert [ ("Location", "San Diego"), ("Biome", "Plains")
                   , ("Category", "Animal"), ("Subtype", "Brown")
                   , ("Name", "Bear"), ("Diet", "Omnivore") ] 1 $
            insert [ ("Location", "San Diego"), ("Biome", "Jungle")
                   , ("Category", "Animal"), ("Subtype", "Sun")
                   , ("Name", "Bear"), ("Diet", "Omnivore") ] 1 $
            insert [ ("Location", "San Diego"), ("Biome", "Polar")
                   , ("Category", "Animal"), ("Subtype", "Polar")
                   , ("Name", "Bear"), ("Diet", "Omnivore") ] 1 $
            adjust succ ([ ("Category", "Animal")
                         , ("Diet", "Carnivore")
                         , ("Biome", "Savannah")
                         , ("Location", "San Diego")
                         , ("Name", "Lion")])
            zooTable

testedTable = foldl foldlInsert
              (mempty
                & keyVals .~ [ ("system",   ["x86_64-linux", "x86_64-darwin"])
                             , ("Branch",   ["master", "develop", "PR-feature"])
                             , ("Strategy", ["submodules", "HEADs"])
                             , ("ghcver",   ["ghc844", "ghc865", "ghc882", "ghc890"])
                             , ("debug",    ["Y", "N"])
                             ])
              $ let ls g b d v = ([ ("system", "x86_64-linux")
                                  , ("Strategy", "submodules")
                                  , ("ghcver", g), ("Branch", b)
                                  , ("debug", d) ], v)
                    lH g b d v = ([ ("system", "x86_64-linux")
                                  , ("Strategy", "HEADs")
                                  , ("ghcver", g), ("Branch", b)
                                  , ("debug", d) ], v)
                    ms g b d v = ([ ("system", "x86_64-darwin")
                                  , ("Strategy", "submodules")
                                  , ("ghcver", g), ("Branch", b)
                                  , ("debug", d) ], v)
                    mH g b d v = ([ ("system", "x86_64-darwin")
                                  , ("Strategy", "HEADs")
                                  , ("ghcver", g), ("Branch", b)
                                  , ("debug", d) ], v)
                in [ ls "ghc844" "PR-feature" "Y" "+"
                   , ls "ghc844" "PR-feature" "N" "+"
                   , lH "ghc865" "PR-feature" "N" "FAIL*2"
                   , ls "ghc882" "develop" "N" "FAIL*1"
                   , lH "ghc865" "develop" "N" "+"
                   , ls "ghc844" "master" "Y" "FAIL*1"
                   , ls "ghc844" "master" "N" "+"
                   , lH "ghc844" "master" "Y" "FAIL*1"
                   , lH "ghc865" "master" "N" "FAIL*1"
                   , lH "ghc882" "master" "N" "FAIL*1"
                   , ls "ghc865" "master" "N" "FAIL*1"
                   , lH "ghc844" "develop" "Y" "+"
                   , lH "ghc844" "develop" "N" "+"
                   , lH "ghc882" "PR-feature" "N" "FAIL*1"
                   , ls "ghc882" "PR-feature" "N" "FAIL*1"
                   , ls "ghc865" "PR-feature" "N" "+"
                   , ls "ghc844" "develop" "N" "+"
                   , lH "ghc844" "develop" "Y" "+"
                   , mH "ghc844" "develop" "N" "+"
                   ]

nestedTable = foldl foldlInsert
              (mempty
                & keyVals .~ [ ("millions",  ["0"])
                             , ("thousands", ["0"])
                             , ("hundreds",  ["0"])
                             , ("tens",      ["0"])
                             , ("ones",      ["0"])
                             ]
              )
              [ let keyvals = [("millions", fromString $ show m)
                              ,("thousands", fromString $ show t)
                              ,("hundreds", fromString $ show h)
                              ,("tens", fromString $ show d)
                              ,("ones", fromString $ show o)]
                    value = if (o `rem` 2) == 1 then "odd" else "even"
                in (keyvals, value)
              | m <- [0..2 :: Int]
              , t <- [0..2 :: Int]
              , h <- [1..2 :: Int]
              , d <- [2..2 :: Int]
              , o <- [0..1 :: Int]
              ]
