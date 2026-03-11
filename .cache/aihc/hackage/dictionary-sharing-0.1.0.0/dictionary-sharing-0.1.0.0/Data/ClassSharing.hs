-- {-#LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving#-}
module Data.ClassSharing 
  ( Shared, Shareable(..), share, unsafeAccess
  , runShared, Ref, DynMap, newRef, unsafeNewRef
  , Typeable
  ) where

import Control.Applicative
import System.IO.Unsafe
import System.IO
import Control.Monad.Fix
import Data.IORef
import qualified Data.Map as M
import Data.Typeable
import Data.Dynamic

type Ref = IORef DynMap
newtype Shared f a = Shared (Shareable f a)
newtype Shareable f a = Shareable {run :: (Ref -> f a)}


runShared :: Shared f a -> Ref -> f a
runShared (Shared x) = run x

-- | Share/memoize a class member of type @f a@. 
share :: (Typeable a, Typeable f) => Shareable f a -> Shared f a
share x = Shared (Shareable $ \r -> memo (run x r) r) where
  memo x r = unsafePerformIO (protect x r)

-- | Should only be used to access class members. A safe wrapper should be defined for every shared class member. Direct access can lead to overriding class member definitions. 
unsafeAccess :: Shared f a -> Shareable f a
unsafeAccess (Shared x) = x


instance Functor f => Functor (Shareable f) where
  fmap f = Shareable . (fmap $ fmap f) . run

instance Applicative f => Applicative (Shareable f) where
  pure                         = Shareable . pure . pure
  Shareable a <*> Shareable b  = Shareable (\r -> a r <*> b r)

instance Alternative f => Alternative (Shareable f) where
  empty = Shareable (const empty)
  Shareable a <|> Shareable b  = Shareable (\r -> a r <|> b r)

unsafeNewRef :: () -> Ref
{-# INLINE unsafeNewRef #-}
unsafeNewRef () = unsafePerformIO 
  (-- putStrLn "Initiating global variable" >>
  newRef)

newRef :: IO Ref
newRef = newIORef dynEmpty

protect :: Typeable a => a -> Ref -> IO a
protect x ref = do
  m <- readIORef ref
  case dynLookup m of
    Just y   ->  -- putStrLn ("Accessing: " ++ (show $ typeOf x)) >> 
                 return y
    Nothing  ->  
                 -- putStrLn ("Initializing: " ++ (show $ typeOf x)) >> 
                 writeIORef ref (dynInsert x m) >> return x

-- |  A dynamic map with type safe
-- insertion and lookup.
newtype DynMap = 
  DynMap (M.Map TypeRep Dynamic) 
  deriving Show

dynEmpty :: DynMap
dynEmpty = DynMap M.empty  
  
dynInsert :: Typeable a => a -> DynMap -> DynMap
dynInsert a (DynMap m) = DynMap (M.insert (typeOf a) (toDyn a) m)

dynLookup :: Typeable a => DynMap -> Maybe a
dynLookup (DynMap m) = hlp fun (error "Data.ClassSharing: This is not supposed to be inspected") where 
    hlp :: Typeable a => 
      (TypeRep -> Maybe a) -> a -> Maybe a
    hlp f a = f (typeOf a)
    fun tr = M.lookup tr m >>= fromDynamic


