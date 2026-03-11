{-# LANGUAGE
    DataKinds
  , TypeOperators
  , KindSignatures
  , PolyKinds
  , TypeFamilies
  , GeneralizedNewtypeDeriving
  , DeriveGeneric
  , DeriveDataTypeable
  , DeriveFunctor
  , DeriveTraversable
  #-}

module Data.NTuple
  ( NTuple
  , empty
  , proj
  , incl
  , toVector
  , -- * Proxies
    _1
  , _2
  , _3
  , _4
  , _5
  , _6
  , _7
  , _8
  , _9
  , _10
  ) where

import Data.Vector (Vector)
import qualified Data.Vector as V

import GHC.TypeLits
import Data.Proxy (Proxy (..))
import Data.List (intercalate)
import Prelude.Singletons (If)
import Data.Eq.Singletons (PEq ((==)))
import Data.Ord.Singletons (POrd ((>)))

import Data.Data (Data, Typeable)
import GHC.Generics (Generic)


newtype NTuple (size :: Nat) a = NTuple
  { toVector :: Vector a
  } deriving (Eq, Data, Typeable, Generic, Functor, Foldable, Traversable)


instance Show a => Show (NTuple size a) where
  show (NTuple xs) = "( " ++ intercalate ", " (V.toList (show <$> xs)) ++ ")"


empty :: NTuple 0 a
empty = NTuple V.empty


-- | Project an element out of the tuple
proj :: ( n <= size
        , (n > 0) ~ 'True
        , KnownNat n
        )
      => Proxy n -- ^ The index
      -> NTuple size a
      -> a
proj p (NTuple xs) = xs V.! (fromInteger (natVal p) - 1)


-- | Include an element to the tuple, overwriting on an existing index
incl :: ( n <= (size + 1)
        , (n > 0) ~ 'True
        , KnownNat n
        , size' ~ If (n == (size + 1)) (size + 1) size
        )
     => Proxy n -- ^ The index
     -> a
     -> NTuple size a
     -> NTuple size' a
incl p x (NTuple xs) = NTuple $
  let n = fromInteger (natVal p) - 1
      (l,r) = V.splitAt n xs

  in  l V.++ (x `V.cons` tail' r)
  where
    tail' r
      | V.null r  = V.empty
      | otherwise = V.tail r




_1 :: Proxy 1
_1 = Proxy

_2 :: Proxy 2
_2 = Proxy

_3 :: Proxy 3
_3 = Proxy

_4 :: Proxy 4
_4 = Proxy

_5 :: Proxy 5
_5 = Proxy

_6 :: Proxy 6
_6 = Proxy

_7 :: Proxy 7
_7 = Proxy

_8 :: Proxy 8
_8 = Proxy

_9 :: Proxy 9
_9 = Proxy

_10 :: Proxy 10
_10 = Proxy
