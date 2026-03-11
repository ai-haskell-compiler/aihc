module Common where

import qualified Test.QuickCheck as QC

check :: QC.Testable prop => String -> prop -> IO ()
check msg t = putStr (msg ++ ": ") >> QC.quickCheck t
