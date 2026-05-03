{- ORACLE_TEST pass -}
module CspLetLhsInfixRoundtrip where

f dvcs' =
  unsafePerformIO $
    runAmb $ do
      dvcs <- lift $ readIORef dvcs'
      let
        loop [] = return ()
        loop (d : ds) =
          do
            dvcABinding d
            filterM (liftM not . dvcIsBound) ds >>= loop
       in filterM (liftM not . dvcIsBound) dvcs >>= loop
      lift $ result dvcs
