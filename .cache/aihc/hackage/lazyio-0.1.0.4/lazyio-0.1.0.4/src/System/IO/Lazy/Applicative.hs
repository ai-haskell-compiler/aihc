module System.IO.Lazy.Applicative where

import qualified Data.ApplicativeChain as Chain
import qualified System.Unsafe as Unsafe
import Control.Applicative (Applicative(pure, (<*>)), )
import Control.Monad (liftM2, )


newtype T a = Cons {decons :: IO (Chain.T a)}

data RunAll = RunAll
   deriving Show

instance Functor T where
   fmap f = Cons . fmap (fmap f) . decons

instance Applicative T where
   pure = Cons . return . Chain.Cons Chain.RunAll
   Cons f <*> Cons x = Cons $ liftM2 (<*>) f x

-- instance MonadIO T where
interleave :: IO a -> T a
interleave = Cons . Unsafe.interleaveIO . fmap (Chain.Cons Chain.RunAll)

run :: T a -> IO a
run = fmap Chain.result . decons

{-
run $ liftA (\ ~( ~(a,b), ~(c,d)) -> a) $ liftA2 (,) (liftA2 (,) (interleave getLine) (interleave getLine)) (liftA2 (,) (interleave getLine) (interleave getLine))
-}
