{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- Module      :  Data.Symbol.Unsafe
-- Copyright   :  (c) Harvard University 2009-2011
--             :  (c) Geoffrey Mainland 2011-2014
-- License     :  BSD-style
-- Maintainer  :  Geoffrey Mainland <mainland@cs.drexel.edu>

module Data.Symbol.Unsafe (
    Symbol(..),
    intern,
    unintern
  ) where

import Control.Concurrent.MVar
import Control.DeepSeq
import Data.Data (Data)
#if __GLASGOW_HASKELL__ >= 608
import Data.String
#endif /* __GLASGOW_HASKELL__ >= 608 */
import Data.Typeable (Typeable)
import qualified Data.Map as Map
import System.IO.Unsafe (unsafePerformIO)

data Symbol =  -- | Unique identifier and the string itself
               Symbol {-# UNPACK #-} !Int !String
#if defined(__GLASGOW_HASKELL__)
  deriving (Data, Typeable)
#endif /* defined(__GLASGOW_HASKELL__) */

instance Eq Symbol where
    (Symbol i1 _) == (Symbol i2 _) = i1 == i2

instance Ord Symbol where
    compare (Symbol i1 _) (Symbol i2 _) = compare i1 i2

instance Show Symbol where
    showsPrec d (Symbol _ s) = showsPrec d s

instance Read Symbol where
    readsPrec d t = [(intern s, t') | (s, t') <- readList t]

#if __GLASGOW_HASKELL__ >= 608
instance IsString Symbol where
    fromString = intern
#endif /* __GLASGOW_HASKELL__ >= 608 */

data SymbolEnv = SymbolEnv
    { uniq    :: {-# UNPACK #-} !Int
    , symbols :: !(Map.Map String Symbol)
    }

symbolEnv :: MVar SymbolEnv
{-# NOINLINE symbolEnv #-}
symbolEnv = unsafePerformIO $ newMVar $ SymbolEnv 1 Map.empty

-- We @'deepseq' s@ so that we can guarantee that when we perform the lookup we
-- won't potentially have to evaluate a thunk that might itself call @'intern'@,
-- leading to a deadlock.

-- |Intern a string to produce a 'Symbol'.
intern :: String -> Symbol
{-# NOINLINE intern #-}
intern s = s `deepseq` unsafePerformIO $ modifyMVar symbolEnv $ \env -> do
    case Map.lookup s (symbols env) of
      Nothing  -> do let sym  = Symbol (uniq env) s
                     let env' = env { uniq    = uniq env + 1,
                                      symbols = Map.insert s sym
                                                (symbols env)
                                    }
                     env' `seq` return (env', sym)
      Just sym -> return (env, sym)

-- |Return the 'String' associated with a 'Symbol'.
unintern :: Symbol -> String
unintern (Symbol _ s) = s
