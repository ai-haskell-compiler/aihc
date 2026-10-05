-- | The unsafe entry points of the lazy 'ST' monad. The lazy 'ST' runs each
-- action in sequence, as 'Control.Monad.ST.Lazy' says, so each of these is
-- the strict function under the conversions between the two monads.
module Control.Monad.ST.Lazy.Unsafe
  ( unsafeInterleaveST,
    unsafeIOToST,
  )
where

import Control.Monad.ST.Lazy (ST, lazyToStrictST, strictToLazyST)
import Control.Monad.ST.Unsafe qualified as Strict
import GHC.Base ((.))
import GHC.IO (IO)

-- | Defer a lazy state action until its result is demanded.
unsafeInterleaveST :: ST s a -> ST s a
unsafeInterleaveST = strictToLazyST . Strict.unsafeInterleaveST . lazyToStrictST

-- | Run an 'IO' action as a lazy state action.
unsafeIOToST :: IO a -> ST s a
unsafeIOToST = strictToLazyST . Strict.unsafeIOToST
