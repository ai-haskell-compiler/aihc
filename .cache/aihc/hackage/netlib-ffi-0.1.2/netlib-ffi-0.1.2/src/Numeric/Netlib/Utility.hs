module Numeric.Netlib.Utility (
   FortranIO,
   run,
   runChecked,
   check,
   assert,
   ignore,
   cint,
   leadingDim,
   alloca,
   allocaArray,
   bool,
   char,
   string,
   float,
   double,
   complexFloat,
   complexDouble,
   real,
   complex,
   number,
   ) where

import qualified Numeric.Netlib.Class as Class

import qualified Foreign.Marshal.Utils as Marshal
import qualified Foreign.Marshal.Array.Guarded as Array
import qualified Foreign.Marshal.Alloc as Alloc
import qualified Foreign.C.String as CStr
import qualified Foreign.C.Types as C
import Foreign.Storable.Complex ()
import Foreign.Storable (Storable, peek)
import Foreign.Ptr (Ptr)

import Control.Monad.Trans.Cont (ContT(ContT))
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)

import Data.Functor.Compose (Compose(Compose, getCompose))
import Data.Complex (Complex)


type FortranIO r = ContT r IO

run :: FortranIO r (IO a) -> FortranIO r a
run act = act >>= liftIO

runChecked :: String -> FortranIO r (Ptr C.CInt -> IO a) -> FortranIO r a
runChecked name act = do
   info <- alloca
   a <- run $ fmap ($info) act
   liftIO $ check name (peek info)
   return a

check :: String -> IO C.CInt -> IO ()
check msg f = do
   err <- f
   when (err/=0) $ error $ msg ++ ": " ++ show err

assert :: String -> Bool -> IO ()
assert msg success = when (not success) $ error $ "assertion failed: " ++ msg

ignore :: String -> Int -> IO ()
ignore _msg _dim = return ()


cint :: Int -> FortranIO r (Ptr C.CInt)
cint = ContT . Marshal.with . fromIntegral

leadingDim :: Int -> FortranIO r (Ptr C.CInt)
leadingDim = cint . max 1

alloca :: (Storable a) => FortranIO r (Ptr a)
alloca = ContT Alloc.alloca

allocaArray :: (Storable a) => Int -> FortranIO r (Ptr a)
allocaArray = ContT . Array.alloca

bool :: Bool -> FortranIO r (Ptr Bool)
bool = ContT . Marshal.with

char :: Char -> FortranIO r (Ptr C.CChar)
char = ContT . Marshal.with . CStr.castCharToCChar

string :: String -> FortranIO r (Ptr C.CChar)
string = ContT . CStr.withCString

float :: Float -> FortranIO r (Ptr Float)
float = ContT . Marshal.with

double :: Double -> FortranIO r (Ptr Double)
double = ContT . Marshal.with

complexFloat :: Complex Float -> FortranIO r (Ptr (Complex Float))
complexFloat = ContT . Marshal.with

complexDouble :: Complex Double -> FortranIO r (Ptr (Complex Double))
complexDouble = ContT . Marshal.with

newtype Number r a = Number {getNumber :: a -> FortranIO r (Ptr a)}

real :: (Class.Real a) => a -> FortranIO r (Ptr a)
real = getNumber $ Class.switchReal (Number float) (Number double)

complex :: (Class.Real a) => Complex a -> FortranIO r (Ptr (Complex a))
complex =
   getNumber $ getCompose $
   Class.switchReal
      (Compose $ Number complexFloat)
      (Compose $ Number complexDouble)

number :: (Class.Floating a) => a -> FortranIO r (Ptr a)
number =
   getNumber $
   Class.switchFloating
      (Number float) (Number double)
      (Number complexFloat) (Number complexDouble)
