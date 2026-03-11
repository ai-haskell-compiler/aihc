import Control.Exception (evaluate)
import Control.Monad (replicateM)
import Data.Foldable (for_)
import GHC.Conc (atomically)
import qualified Ki
import qualified System.Random as Random
import qualified System.Random.Stateful as Random
import Test.Tasty.Bench
import TimerWheel
import Prelude

main :: IO ()
main = do
  Ki.scoped \scope -> do
    mainThread <- Ki.fork scope main1
    atomically (Ki.await mainThread)

main1 :: IO ()
main1 = do
  let delays :: [Double]
      delays =
        Random.runStateGen_
          (Random.mkStdGen 0)
          (replicateM 1_000_000 . Random.uniformRM (0, 60 * 5))
  _ <- evaluate (sum delays)

  defaultMain
    [ bench "insert 1m" (whnfIO (insertN (map (realToFrac @Double @Seconds) delays)))
    ]

insertN :: [Seconds] -> IO ()
insertN delays =
  with Config {spokes = 1024, resolution = 1} \wheel -> do
    for_ delays \delay -> do
      register wheel delay (pure ())
