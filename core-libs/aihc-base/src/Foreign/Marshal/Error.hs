module Foreign.Marshal.Error
  ( throwIf,
    throwIf_,
    throwIfNeg,
    throwIfNeg_,
    throwIfNull,
    void,
  )
where

import Foreign.Ptr (Ptr, nullPtr)
import Prelude (Bool, Eq (..), IO, Num (..), Ord (..), String, const, ioError, return, userError, (>>), (>>=))

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

-- | Throw an error with the message when the action returns a null pointer.
throwIfNull :: String -> IO (Ptr a) -> IO (Ptr a)
throwIfNull message = throwIf (== nullPtr) (const message)

void :: IO a -> IO ()
void action = action >> return ()
