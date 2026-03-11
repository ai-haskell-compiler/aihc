{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Numeric.AD (grad)
import Numeric.MCMC.Hamiltonian

target :: RealFloat a => [a] -> a
target [x0, x1] = negate ((x0 + 2 * x1 - 7) ^ 2 + (2 * x0 + x1 - 5) ^ 2)

gTarget :: [Double] -> [Double]
gTarget = grad target

booth :: Target [Double]
booth = Target target (Just gTarget)

main :: IO ()
main = withSystemRandom . asGenIO $ \gen -> do
  _ <- chain 100 0.05 20 [0, 0] booth gen
  mcmc 100 0.05 20 [0, 0] booth gen

