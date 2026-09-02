module Foreign.Marshal.Error
  ( throwIf,
    throwIf_,
    throwIfNeg,
    throwIfNeg_,
    void,
  )
where

import Prelude (Bool, IO, Num (..), Ord (..), String, ioError, return, userError, (>>), (>>=))

throwIf :: (a -> Bool) -> (a -> String) -> IO a -> IO a
throwIf failed describe action =
  action >>= \value ->
    if failed value
      then ioError (userError (describe value))
      else return value

throwIf_ :: (a -> Bool) -> (a -> String) -> IO a -> IO ()
throwIf_ failed describe action = void (throwIf failed describe action)

throwIfNeg :: (Ord a, Num a) => (a -> String) -> IO a -> IO a
throwIfNeg = throwIf (< 0)

throwIfNeg_ :: (Ord a, Num a) => (a -> String) -> IO a -> IO ()
throwIfNeg_ = throwIf_ (< 0)

void :: IO a -> IO ()
void action = action >> return ()
