-- Types module
-- By Gregory W. Schwartz

-- | Collects all types used in the program

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Math.RandomTree.Types where

-- Built-in
import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative
import Control.Monad.Trans.Maybe

--Cabal
import Control.Monad.Random

-- Algebraic
data TreeState a = TreeState { size :: Int }

data TreeConfig = TreeConfig { maxSize     :: Int
                             , minSize     :: Int
                             , minChildren :: Int
                             , maxChildren :: Int }

newtype ReaderStateRandom a = ReaderStateRandom
    { runReaderStateRandom :: ReaderT TreeConfig (StateT (TreeState Int) (MaybeT (Rand StdGen))) a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadPlus
             , MonadRandom
             , MonadState (TreeState Int)
             , MonadReader TreeConfig )

instance Alternative ReaderStateRandom where
    (<|>) = mplus
    empty = mzero
