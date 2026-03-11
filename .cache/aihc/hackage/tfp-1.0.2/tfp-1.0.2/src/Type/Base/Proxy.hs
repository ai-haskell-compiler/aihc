module Type.Base.Proxy where

import Control.Applicative (Applicative, pure, (<*>), )

import qualified Prelude as P
import Prelude (String, Eq, Functor, fmap)


data Proxy a = Proxy
   deriving (Eq)

instance Functor Proxy where
   fmap _f Proxy = Proxy

instance Applicative Proxy where
   pure _ = Proxy
   Proxy <*> Proxy = Proxy


class Show a where
   showsPrec :: P.Int -> Proxy a -> P.ShowS

instance Show a => P.Show (Proxy a) where
   showsPrec = showsPrec
