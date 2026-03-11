-- | Makes a "line of dominos" of processes which each wait for a "Fall"
--   message, then terminate. The dominos are in a "circle" so they're all
--   created before any terminate

{-# LANGUAGE LambdaCase #-}

module Control.Concurrent.NanoErl.Examples.Dominos where

import Control.Concurrent.NanoErl

data Fall = Fall
 deriving (Show)

numDominos :: Int
numDominos = 5*10^5 -- half a million

main :: IO ()
main = runNanoErl $ do
   putStrLn $ "Creating " ++ show numDominos ++ " dominos..."
   _ <- spawn $ makeDominos numDominos Nothing
   return ()

makeDominos :: Int -> Maybe (Pid Fall) -> Process Fall
-- If we're the last one, send 'Fall' to the first
makeDominos 0 (Just firstDomino) = \self -> do
   putStrLn "Knocking down the first domino..."
   firstDomino ! Fall
   receive self $ \case
      Fall -> putStrLn "You knocked 'em all down!"
makeDominos 0 Nothing =
   error "that shouldn't happen"
makeDominos n Nothing = \self -> do
   nextDomino <- spawn $ makeDominos (n-1) (Just self)
   receive self $ \case
      Fall -> nextDomino ! Fall -- die!
makeDominos n justFirst = \self -> do
   nextDomino <- spawn $ makeDominos (n-1) justFirst
   receive self $ \case
      Fall -> nextDomino ! Fall -- die!
