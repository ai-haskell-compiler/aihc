module Sound.Audacity.Project.Track.Wave.Summary (
   State(State),
   Monad,
   eval,
   Handle,
   createHandle,
   deleteHandle,
   withHandle,
   usingHandle,
   T(Cons, length_, limits_, content_),
   fromBlock,
   attachStarts,
   sequenceFromStorableVector,
   reserve,
   Limits(Limits, min_, max_, rms_),
   defltLimits,
   storeLimits,
   summary,
   ) where

import qualified Data.StorableVector.Lazy as SVL
import qualified Data.StorableVector as SV
import Foreign.Storable.Record as Store
import Foreign.Storable (Storable (..), )

import qualified Data.List.HT as ListHT
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Tuple.HT (mapPair)

import qualified Control.Monad.Trans.State as MS
import qualified Control.Monad.Trans.Reader as MR
import qualified Control.Monad.Trans.Class as MT
import qualified Control.Monad as M
import Control.DeepSeq (NFData, rnf, ($!!), )
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Applicative (liftA3, )

import Prelude hiding (Monad, )


newtype State = State Int
type Monad m = MR.ReaderT FilePath (MS.StateT State m)

eval :: (M.Monad m) => FilePath -> Monad m a -> m a
eval path =
   flip MS.evalStateT (State 0) . flip MR.runReaderT path


data Handle = Handle FilePath (IORef State)

createHandle :: FilePath -> IO Handle
createHandle path =
   fmap (Handle path) $ newIORef $ State 0

deleteHandle :: Handle -> IO ()
deleteHandle _ = return ()

withHandle :: FilePath -> (Handle -> IO a) -> IO a
withHandle path act =
   createHandle path >>= act

usingHandle :: (MonadIO io) => Handle -> Monad io a -> io a
usingHandle (Handle path stateRef) act = do
   oldState <- liftIO $ readIORef stateRef
   (a, newState) <- flip MS.runStateT oldState $ flip MR.runReaderT path act
   liftIO $ writeIORef stateRef newState
   return a


data T =
   Cons {
      length_ :: Int,
      limits_ :: Limits,
      content_ :: SVL.Vector Limits
   }
   deriving Show

instance NFData T where
   rnf (Cons len limits dat) = rnf (len, limits, dat)


fromBlock :: SVL.Vector Float -> T
fromBlock block =
   let sum256 = map accumulate $ SVL.sliceVertical 256 block
       sum65536 = map reduce $ ListHT.sliceVertical 256 sum256
       accumTmp@(_, (_, len)) = reduce sum65536
   in  Cons {
          length_ = len,
          limits_ = limitsFromAccumulators accumTmp,
          content_ =
             SVL.fromChunks $
                (SV.pack $ map limitsFromAccumulators sum256) :
                (SV.pack $ map limitsFromAccumulators sum65536) :
                []
       }


attachStarts :: [T] -> [(Int, T)]
attachStarts xs =
   zipWith
      (\ start block -> ((,) $!! start) block)
      (scanl (+) 0 $ map length_ xs) xs

sequenceFromStorableVector :: Int -> SVL.Vector Float -> [T]
sequenceFromStorableVector blockSize =
   map fromBlock . SVL.sliceVertical blockSize


reserve :: (M.Monad m) => Monad m State
reserve = MT.lift $ do
   s@(State n) <- MS.get
   MS.put $ State (n+1)
   return s



data Limits = Limits {min_, max_, rms_ :: Float}
   deriving Show

instance NFData Limits where
   rnf (Limits ymin ymax yrms) = rnf (ymin, ymax, yrms)

defltLimits :: Limits
defltLimits = Limits {min_ = -1, max_ = 1, rms_ = 0.2}

storeLimits :: Store.Dictionary Limits
storeLimits =
   Store.run $
   liftA3 Limits
      (Store.element min_)
      (Store.element max_)
      (Store.element rms_)

instance Storable Limits where
   sizeOf = Store.sizeOf storeLimits
   alignment = Store.alignment storeLimits
   peek = Store.peek storeLimits
   poke = Store.poke storeLimits



summary :: Int -> SVL.Vector Float -> SV.Vector Limits
summary chunkSize =
   SV.pack . map (limitsFromAccumulators . accumulate) .
   SVL.sliceVertical chunkSize

reduce :: [((Float, Float), (Float, Int))] -> ((Float, Float), (Float, Int))
reduce xs =
   let ((xmin, xmax), (xsqr, len)) = mapPair (unzip, unzip) $ unzip xs
   in  ((minimum xmin, maximum xmax), (sum xsqr, sum len))

limitsFromAccumulators :: ((Float, Float), (Float, Int)) -> Limits
limitsFromAccumulators ((xmin, xmax), (xsqr, len)) =
   Limits xmin xmax (sqrt (xsqr / fromIntegral len))

accumulate :: SVL.Vector Float -> ((Float, Float), (Float, Int))
accumulate chunk =
   ((SVL.foldl' min 1 chunk, SVL.foldl' max (-1) chunk),
    (SVL.foldl' (+) 0 (SVL.map (^(2::Int)) chunk), SVL.length chunk))
