{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wno-unticked-promoted-constructors #-}

-- | This module gives an alternative interface to build and run monad transformer stacks.
--
-- Using this approach is supposed to improve error messages and reduce errors, such as forgetting
-- to add 'TransparentT' at the bottom of the stack.
module Control.Monad.Trans.Compose.Stack where

import Control.Monad.Trans.Compose
import Control.Monad.Trans.Compose.Transparent
import Data.Kind

-- * 'StackT'
--
-- $stackt
--
-- 'StackT' is a more ergonomic way to define a monad transformer stack.

-- | An isomorphism between a 'Stack' and the corresponding monad transformer, which can be built
-- using 'ComposeT'.
--
-- An additional 'TransparentT' will automatically be used at the bottom of the stack.
-- You only have to worry about the semantically relevant transformers.
type family StackT (ts :: Stack) = (t :: (Type -> Type) -> Type -> Type) | t -> ts where
  StackT NilT = TransparentT
  StackT (ts :.|> t) = ComposeT t (StackT ts)

-- ** 'runStackT' and 'RunStackT'
--
-- $runStackt
--
-- Monad transformer stacks, that only consist of t'Control.Monad.Trans.Identity.IdentityT' and
-- t'Control.Monad.Trans.Reader.ReaderT', can be run with 'runStackT'.
--
-- If your transformer stack contains monadic state t'Control.Monad.Trans.Control.StT', you will have to use 'runComposeT' or 'Control.Monad.Trans.Compose.Infix../>'!
-- You can still use 'StackT' for the type definition.

-- | Run a monad transformer stack.
--
-- This takes a 'RunStackT' as an argument containing the individual runners.
--
-- 'runStackT' can only be used for monad transformer stacks without monadic state t'Control.Monad.Trans.Control.StT'.
runStackT :: RunStackT ts m a -> StackT ts m a -> m a
runStackT RunNilT = runTransparentT
runStackT (runRemainingStackT :..> runNextT) = runStackT runRemainingStackT . runNextT . deComposeT

-- | A data type representing the runner function of a monad transformer stack.
--
-- This is basically a heterogeneous list of monad transformer runners.
--
-- 'RunStackT' can only be used for monad transformer stacks without monadic state t'Control.Monad.Trans.Control.StT'.
data RunStackT :: Stack -> (Type -> Type) -> Type -> Type where
  -- | run an empty monad transformer stack
  RunNilT :: RunStackT NilT m a
  -- | run the next monad transformer on a stack
  (:..>) :: RunStackT ts m a -- ^ run remaining stack
         -> (t (StackT ts m) a -> StackT ts m a) -- ^ run next monad transformer
         -> RunStackT (ts :.|> t) m a

infixl 1 :..>

-- * 'Stack'
--
-- $stack
--
-- 'Stack' is used to define monad transformer stacks with 'StackT'.

-- | A data kind representing a monad transformer stack.
--
-- This is basically a type-level list of monad transformers.
type data Stack where
  -- | an empty monad transformer stack
  NilT :: Stack
  -- | add a monad transformer to a stack
  (:.|>) :: Stack -- ^ remaining stack
         -> ((Type -> Type) -> Type -> Type) -- ^ next monad transformer
         -> Stack

infixl 1 :.|>

-- * Examples
--
-- $examples
--
-- Feel free to compare these examples to the ones in "Control.Monad.Trans.Compose".

-- TODO: Qualified identifiers such as 'Control.Monad.Trans.Class.MonadTrans' need a "t"-prefix to
--       work correctly, but it doesn't work with a leading opening parenthesis "(".

-- ** Example 1: Build a transformer stack
--
-- $example1
--
-- Apply the type family 'StackT' to a type of kind 'Stack' and generate a monad transformer stack built with 'ComposeT'.
--
-- @
-- type AppStack = 'NilT' ':.|>' t'Control.Monad.Trans.Reader.ReaderT' 'Bool' ':.|>' CustomT ':.|>' t'Control.Monad.Trans.Reader.ReaderT' 'Char'
-- newtype AppT m a = AppT { unAppT :: StackT AppStack m a }
--   deriving newtype ('Functor', 'Applicative', 'Monad')
--   deriving newtype ('Control.Monad.Trans.Class.MonadTrans', t'Control.Monad.Trans.Control.MonadTransControl', t'Control.Monad.Trans.Control.Identity.MonadTransControlIdentity')
--   deriving newtype MonadCustom
--   deriving newtype ('Control.Monad.Reader.Class.MonadReader' 'Bool')
-- @

-- ** Example 2: Run a transformer stack
--
-- $example2
--
-- Use 'runStackT' and supply it with a 'RunStackT' argument.
--
-- @
-- runAppT :: AppT m a -> m a
-- runAppT appTma = 'runStackT' runAppStackT $ unAppT appTma
--  where
--   runAppStackT = 'RunNilT'
--     ':..>' (\\ tma -> 'Control.Monad.Trans.Reader.runReaderT' tma 'True')
--     ':..>' runCustomT
--     ':..>' runReaderT'
--
--   runReaderT' :: t'Control.Monad.Reader.Class.MonadReader' 'Bool' m => t'Control.Monad.Trans.Reader.ReaderT' 'Char' m a -> m a
--   runReaderT' tma = do
--     bool <- 'Control.Monad.Trans.Reader.ask'
--     let char = if bool then \'Y\' else \'N\'
--     'Control.Monad.Trans.Reader.runReaderT' tma char
-- @
