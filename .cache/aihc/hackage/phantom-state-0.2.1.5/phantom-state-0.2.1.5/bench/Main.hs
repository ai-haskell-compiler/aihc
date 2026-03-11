
import Criterion.Main

-- vectors
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

-- functors
import Control.Applicative
import Control.Monad (mapM_,replicateM)
import Data.Foldable (sequenceA_)
import Control.Monad.Trans.Class
-- state
import Control.Monad.Trans.State

-- phantom-state
import Control.Applicative.PhantomState

replicateA_ :: Applicative f => Int -> f a -> f ()
replicateA_ n f = sequenceA_ (replicate n f)

createVector1 :: Int -> V.Vector Int
createVector1 n = V.create $ do
  v <- MV.unsafeNew n
  let step = StateT $ \i -> do
        MV.unsafeWrite v i i
        pure ((),i+1)
  execStateT (replicateA_ n step) 0
  return v

createVector2 :: Int -> V.Vector Int
createVector2 n = V.create $ do
  v <- MV.unsafeNew n
  let step = useAndChangeState $ \i -> do
        MV.unsafeWrite v i i
        pure (i+1)
  runPhantomStateT (replicateA_ n step) 0
  return v

createVector3 :: Int -> V.Vector Int
createVector3 n = V.create $ do
  v <- MV.unsafeNew n
  mapM_ (\i -> MV.unsafeWrite v i i) [0..n-1]
  return v

createVector4 :: Int -> V.Vector Int
createVector4 n = V.create $ do
  v <- MV.unsafeNew n
  let loop = do i <- get
                if i == n
                   then pure ()
                   else do lift $ MV.write v i i
                           modify (+1)
                           loop
  execStateT loop 0
  return v

main :: IO ()
main = defaultMain
  [ bgroup "vector-create"
      [ bench "generate" $ whnf (\i -> V.generate i id) 10000
      , bench "state" $ whnf createVector1 10000
      , bench "phantom-state" $ whnf createVector2 10000
      , bench "mapM_" $ whnf createVector3 10000
      , bench "state-loop" $ whnf createVector4 10000
        ]
  ]
