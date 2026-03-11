module IOMcn (
	IOMcn, runIOMcn, (>>>), arr, app,
	getLn, getInt, putLn, isEven) where

import Data.Time

newtype IOMcn a b = IOMcn { getIOMcn :: a -> IO b }

runIOMcn :: IOMcn () a -> IO a
runIOMcn m = getIOMcn m ()

(>>>) :: IOMcn a b -> IOMcn b c -> IOMcn a c
m1 >>> m2 = IOMcn $ \x -> getIOMcn m1 x >>= getIOMcn m2

arr :: (a -> b) -> IOMcn a b
arr f = IOMcn $ return . f

app :: IOMcn (IOMcn a b, a) b
app = IOMcn $ uncurry getIOMcn

getLn :: IOMcn () String
getLn = IOMcn $ const getLine

getInt :: IOMcn () Int
getInt = IOMcn . const $ read <$> getLine

putLn :: IOMcn String ()
putLn = IOMcn putStrLn

isEven :: IOMcn () Bool
isEven = IOMcn . const $ even . floor . utctDayTime <$> getCurrentTime
