module Control.Monad.Trans.Maybe.Extras where

import Control.Monad.Trans.Maybe
import Data.Maybe

-- | The more useful version is to use 'evalMaybeT' flipped so it can be in a chain of
-- transformer runners, like this @evalCont . (`evalMaybeT` val)@
-- This argument ordering is consistent with 'Control.Monad.Trans.State.evalStateT'.
evalMaybeT :: Functor m => MaybeT m a -> a -> m a
evalMaybeT m a = (fromMaybe a) <$> (runMaybeT m)
