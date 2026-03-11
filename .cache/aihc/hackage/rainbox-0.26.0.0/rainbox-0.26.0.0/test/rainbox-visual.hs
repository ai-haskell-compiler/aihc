{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
-- | Prints all the boxes in the tutorial.  The output must be
-- visually inspected.

module Main where

import Data.Function ((&))
import Rainbow
import Rainbox
import Rainbox.Tutorial

bicolorStationBox :: BicolorTable
bicolorStationBox = BicolorTable
  { _bctEvenBackground = cyan <> color256 254
  , _bctOddBackground = mempty
  , _bctSpacerWidth = 2
  , _bctAlignments = [left, center, right]
  , _bctRows =
  [ [ [ [ "Red" & fore red ]
      , [ "Orange" & fore (yellow <> color256 220) ]
      , [ "Silver" & fore (white <> grey) ]
      , [ "Blue" & fore blue ]
      ]
    , [ [ "Metro Center" ] ]
    , [ [ "607 13th St NW" ]
      , [ "Washington, ", "DC" & fore red & back white, " 20005" ]
      ]
    ]

  , [ [ [ "Orange" & fore (yellow <> color256 220) ]
      , [ "Silver" & fore (white <> grey) ]
      , [ "Blue" & fore blue ]
      , [ "Green" & fore green ]
      , [ "Yellow" & fore yellow ]
      ]
    , [ [ "L'Enfant Plaza" ] ]
    , [ [ "600 Maryland Ave SW" ]
      , [ "Washington, " , "DC" & fore red & back white, " 20024" ]
      ]
    ]

  , [ [ [ "Red" & fore red ]
      ]
    , [ [ "Silver Spring" ] ]
    , [ [ "8400 Colesville Rd" ]
      , [ "Silver Spring, ", "MD" & fore yellow & back black, " 20910" ]
      ]
    ]

  , [ [ [ "Orange" & fore (yellow <> color256 220) ]
      , [ "Silver" & fore (white <> grey) ]
      ]
    , [ [ "Court House" ] ]
    , [ [ "2100 Wilson Blvd" ]
      , [ "Arlington, " , "VA" & fore cyan & back grey, " 22201" ]
      ]
    ]

  , [ [ [ "Green" & fore green ]
      , [ "Yellow" & fore yellow ]
      ]
    , [ [ "Prince George's Plaza" ]
      ]
    , [ [ "3575 East-West Hwy" ]
      , [ "Hyattsville, ", "MD" & fore yellow & back black, " 20782" ]
      ]
    ]
  ]
  }



printBox :: String -> IO () -> IO ()
printBox lbl act = do
  putStrLn $ replicate 50 '='
  putStrLn ""
  putStrLn $ lbl ++ ":"
  putStrLn ""
  act
  putStrLn ""

main :: IO ()
main = do
  printBox "box1" renderBox1
  printBox "box2" renderBox2
  printBox "box3" renderBox3
  printBox "box4" renderBox4
  printBox "box5" renderBox5
  printBox "verticalStationTable" renderVerticalStationTable
  printBox "horizontalStationTable" renderHorizontalStationTable
  printBox "bicolor station table" (putBicolorTable bicolorStationBox)
