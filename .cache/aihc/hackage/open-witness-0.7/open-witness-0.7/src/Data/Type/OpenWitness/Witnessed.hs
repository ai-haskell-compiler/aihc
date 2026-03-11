module Data.Type.OpenWitness.Witnessed
    ( Witnessed
    , unWitnessed
    , newWitnessed
    , WitnessedIORef
    , newWitnessedIORef
    ) where

import Data.IORef
import Data.Type.OpenWitness
import Data.Type.Witness
import Prelude
import Data.Kind

type Witnessed :: forall k. (k -> Type) -> k -> Type
type Witnessed @k = PairType @k IOWitness

unWitnessed :: Witnessed f a -> f a
unWitnessed (MkPairType _ fa) = fa

newWitnessed :: f a -> IO (Witnessed f a)
newWitnessed fa = do
    wit <- newIOWitness
    return $ MkPairType wit fa

type WitnessedIORef = Witnessed IORef

newWitnessedIORef :: a -> IO (WitnessedIORef a)
newWitnessedIORef a = do
    ref <- newIORef a
    newWitnessed ref
