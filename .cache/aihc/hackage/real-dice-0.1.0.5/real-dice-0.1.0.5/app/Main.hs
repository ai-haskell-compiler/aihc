module Main where

import Generate (balancedTables, pseudoRandomizedInts)
import RealDice.Convert.CSV (intsToCSV)
import RealDice.Generate.BalancedTables
  ( rdBinFull,
    rdBinPrime,
    rdIntsFull,
    rdIntsPrime,
  )
import RealDice.Generate.RawData (rawBinFull, rawBinPrime)

-- | Generate all the files needed for the RealDice package

-- |  Data generated for use within the RealDice library is written to the
--   `src/` directory in `.hs` files to avoid having to read the data from
--   files at runtime

-- | One such file is list of integers that are pseudo-randomized with `StdGen`
--   to use as a seed for the balanced data

-- | The other file contains the balanced data tables that are used in the
--   random generation functions

-- | These files are set up to be generated so that I can easily add newly
--   gathered data, process it, make sure that the pseudo-randomized lists are
--   the correct length, and then automatically update all the parts of the
--   library that use this data.

-- | In addition to these files, files with binary and CSV data are generated
--   in the `generate-data/` directory for use outside of the RealDice library

-- | In the future, functionality will be added to check for valid seed files
--   in the `data-seed/` directory. If such data is found it will be randomized
--   with the RealDice data to produce custom data files in the
--   `generate-data/` directory

-- | If you would like to do this yourself now, it can be accomplished by using
--   the `RealDice.Manipulate.Randomize` module to randomize the seed data and
--   then use thing `RealDice.Convert.CSV` module to convert the randomized
--   data to CSV format to write to a file
main :: IO ()
main = do
  putStrLn "Generating src files..."
  putStrLn "Generating pseudoRandomized integers:"
  writeFile
    "src/RealDice/Generate/PseudoRandomizedInts.hs"
    pseudoRandomizedInts
  putStrLn "Generating standard RNG tables:"
  writeFile "src/RealDice/Generate/BalancedTables.hs" balancedTables
  putStrLn "Generating data files..."
  putStrLn "Generating raw binary data:"
  writeFile "data-generated/real_dice_raw_prime.bin" rawBinPrime
  writeFile "data-generated/real_dice_raw_full.bin" rawBinFull
  putStrLn "Generating balanced binary data:"
  writeFile "data-generated/real_dice_balanced_prime.bin" rdBinPrime
  writeFile "data-generated/real_dice_balanced_full.bin" rdBinFull
  putStrLn "Generating balanced integer data:"
  writeFile
    "data-generated/real_dice_balanced_integers_prime.csv"
    (intsToCSV rdIntsPrime)
  writeFile
    "data-generated/real_dice_balanced_integers_full.csv"
    (intsToCSV rdIntsFull)
  putStrLn "Files generated successfully!"
