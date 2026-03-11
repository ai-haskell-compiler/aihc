{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable #-}
{- |
ALSA does not distinguish between programming errors and runtime exceptions,
which is sad, but we have to cope with it.
-}
module Sound.ALSA.Exception where

import qualified Control.Exception.Extensible as Exc
import Control.Exception.Extensible (Exception, )

import Data.Typeable (Typeable, )
import Foreign.C.Error (Errno(Errno), ePIPE, errnoToIOError, )
import Foreign.C.String (CString, peekCString, )
import qualified Foreign.C.Types as C

import Prelude hiding (catch, show, )
import qualified Prelude as P

data T = Cons {
   location    :: String,
   description :: String,
   code        :: Errno
   } deriving (Typeable)

instance Show T where
   showsPrec p (Cons l d (Errno c)) =
      showParen (p>10)
         (showString "AlsaException.Cons " .
          shows l . showString " " .
          shows d . showString " " .
          showParen True (showString "Errno " . shows c))

instance Exception T where

checkResult :: Integral a => String -> a -> IO a
checkResult f r =
   if r < 0
     then throw f (Errno (negate (fromIntegral r)))
     else return r

checkResult_ :: Integral a => String -> a -> IO ()
checkResult_ f r = checkResult f r >> return ()

checkResultMaybe :: String -> (C.CInt -> a) -> (C.CInt -> Maybe a) -> C.CInt -> IO a
checkResultMaybe f ok err x =
   if x >= 0
     then return (ok x)
     else case err x of
             Just a -> return a
             _ -> throw f (Errno x)


throw :: String -> Errno -> IO a
throw fun err = do
   d <- strerror err
   Exc.throw Cons
     { location = fun
     , description = d
     , code = err
     }

catch :: IO a -> (T -> IO a) -> IO a
catch = Exc.catch

catchErrno ::
      Errno
   -> IO a -- ^ Action
   -> IO a -- ^ Handler
   -> IO a
catchErrno e x h =
   catch x (\ex -> if code ex == e then h else Exc.throw ex)

catchXRun ::
      IO a -- ^ Action
   -> IO a -- ^ Handler
   -> IO a
catchXRun = catchErrno ePIPE

showErrno :: Errno -> String
showErrno (Errno n) = P.show n

show :: T -> String
show e =
   location e ++ ": " ++
   description e ++ " (" ++ showErrno (code e) ++ ")"

-- | Converts any 'AlsaException.T' into an 'IOError'.
-- This produces better a error message than letting an uncaught
-- 'AlsaException.T' propagate to the top.
rethrow :: IO a -> IO a
rethrow x =
    catch x $ \e ->
       ioError (errnoToIOError (location e)
                               (code e) Nothing Nothing)

-- | Returns the message for an error code.
strerror :: Errno -> IO String
strerror x = peekCString =<< snd_strerror x

foreign import ccall "alsa/asoundlib.h snd_strerror"
  snd_strerror :: Errno -> IO CString
