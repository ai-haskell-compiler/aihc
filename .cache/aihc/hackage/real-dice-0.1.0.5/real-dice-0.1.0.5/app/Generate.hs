module Generate (balancedTables, pseudoRandomizedInts) where

import RealDice.Convert.BinaryString (boolsToBin)
import RealDice.Generate (rawBoolFull, rawBoolPrime)
import RealDice.Manipulate (randomizeWithCustomBools)
import StdGenRandomize (randomizeList)

-- | Verify that a list of Ints has the same length as a list of Bools before
--   running a function on them.

-- | This is used to make sure that there is an Int in the pseudo-randomized
--   seed list for each Bool in the RealDice raw data.
checkLengths :: ([Int] -> [Bool] -> [Int]) -> [Int] -> [Bool] -> [Int]
checkLengths f l1 l2 = do
  if length l1 == length l2
    then f l1 l2
    else
      error
        ( "Lengths not equal! PseudoRandomized Int List length: "
            ++ show (length l1)
            ++ " Raw Bool List length: "
            ++ show (length l2)
        )

-- | Prime-length list of integers randomized with StdGen, for use as a seed
--   when generating the RealDice balanced data
psRndIntsPrime :: [Int]
psRndIntsPrime =
  randomizeList
    [1 .. length RealDice.Generate.rawBoolPrime]

-- | Full-length list of integers randomized with StdGen, for use as a seed
--   when generating the RealDice balanced data
psRndIntsFull :: [Int]
psRndIntsFull =
  randomizeList
    [1 .. length RealDice.Generate.rawBoolFull]

-- | Prime-length list of integers randomized with the RealDice raw data
rdIntsPrime :: [Int]
rdIntsPrime =
  checkLengths
    randomizeWithCustomBools
    psRndIntsPrime
    rawBoolPrime

-- | Full-length list of integers randomized with the RealDice raw data
rdIntsFull :: [Int]
rdIntsFull =
  checkLengths
    randomizeWithCustomBools
    psRndIntsFull
    rawBoolFull

-- | Prime-length balanced list of Bools randomized with the RealDice raw data
rdBoolsPrime :: [Bool]
rdBoolsPrime = map odd rdIntsPrime

-- | Full-length balanced list of Bools randomized with the RealDice raw data
rdBoolsFull :: [Bool]
rdBoolsFull = map odd rdIntsFull

-- | Prime-length balanced binary string randomized with the RealDice raw data
rdBinPrime :: String
rdBinPrime = boolsToBin rdBoolsPrime

-- | Full-length balanced binary string randomized with the RealDice raw data
rdBinFull :: String
rdBinFull = boolsToBin rdBoolsFull

-- | The balanced tables for the RealDice library
balancedTables :: String
balancedTables =
  "-- | This module contains balanced tables randomized with the RealDice \n\
  \--   raw data\n\
  \module RealDice.Generate.BalancedTables\n\
  \  ( rdBoolsPrime,\n\
  \    rdBoolsFull,\n\
  \    rdIntsPrime,\n\
  \    rdIntsFull,\n\
  \    rdBinPrime,\n\
  \    rdBinFull,\n\
  \  )\n\
  \where\n\
  \\n\
  \-- | Prime-length list of integers randomized with the RealDice raw data\n\
  \rdIntsPrime :: [Int]\n\
  \rdIntsPrime = "
    ++ show rdIntsPrime
    ++ "\n\
       \\n\
       \-- | Full-length list of integers randomized with the RealDice raw data\n\
       \rdIntsFull :: [Int]\n\
       \rdIntsFull = "
    ++ show rdIntsFull
    ++ "\n\
       \\n\
       \-- | Prime-length balanced list of Bools randomized with the RealDice raw data\n\
       \rdBoolsPrime :: [Bool]\n\
       \rdBoolsPrime = "
    ++ show rdBoolsPrime
    ++ "\n\
       \\n\
       \-- | Full-length balanced list of Bools randomized with the RealDice raw data\n\
       \rdBoolsFull :: [Bool]\n\
       \rdBoolsFull = "
    ++ show rdBoolsFull
    ++ "\n\
       \\n\
       \-- | Prime-length balanced binary string randomized with the RealDice raw data\n\
       \rdBinPrime :: String\n\
       \rdBinPrime = "
    ++ show rdBinPrime
    ++ "\n\
       \\n\
       \-- | Full-length balanced binary string randomized with the RealDice raw data\n\
       \rdBinFull :: String\n\
       \rdBinFull = "
    ++ show rdBinFull

-- | Pseudo-randomized integers generated for the RealDice library
pseudoRandomizedInts :: String
pseudoRandomizedInts =
  "-- | This module contains pseudo-randomized integers for use as a seed\n\
  \--   when generating the RealDice balanced data\n\
  \module RealDice.Generate.PseudoRandomizedInts\n\
  \  ( psRndIntsPrime,\n\
  \    psRndIntsFull\n\
  \  )\n\
  \where\n\
  \\n\
  \-- | Prime-length list of integers randomized with StdGen, for use as a seed\n\
  \--   when generating the RealDice balanced data\n\
  \psRndIntsPrime :: [Int]\n\
  \psRndIntsPrime = "
    ++ show psRndIntsPrime
    ++ "\n\
       \\n\
       \-- | Full-length list of integers randomized with StdGen, for use as a seed\n\
       \--   when generating the RealDice balanced data\n\
       \psRndIntsFull :: [Int]\n\
       \psRndIntsFull = "
    ++ show psRndIntsFull
