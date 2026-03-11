module UnconditionalJump
  ( -- * Labels
    Label,
    label,
    goto,

    -- ** Derived @label@ variants
    labelWith,
    labelE,
  )
where

import Control.Exception (Exception (..), SomeException, asyncExceptionFromException, asyncExceptionToException, catch, throwIO)
import Data.Functor.Contravariant (Contravariant (contramap))
import IntSupply (IntSupply)
import IntSupply qualified
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

newtype Label a
  = Label (forall x. a -> IO x)

instance Contravariant Label where
  contramap f (Label g) =
    Label (g . f)

-- | Create a label.
label :: (Label a -> IO a) -> IO a
label f = do
  i <- IntSupply.next supply
  catch (f (Label (throwIO . X i))) \err@(X j x) ->
    if i == j
      then pure (unsafeCoerce x)
      else throwIO err

-- |
-- @
-- labelWith f g h = label (fmap g . h . contramap f)
-- @
labelWith :: (a -> c) -> (b -> c) -> (Label a -> IO b) -> IO c
labelWith f g action =
  label (fmap g . action . contramap f)

-- |
-- @
-- labelE = labelWith Left Right
-- @
labelE :: (Label a -> IO b) -> IO (Either a b)
labelE =
  labelWith Left Right

-- | Go to a label.
goto :: Label a -> a -> IO notreached
goto (Label f) x =
  f x

data X = forall a. X {-# UNPACK #-} !Int a

-- Make X an async exception so it's less likely to be caught and ignored
instance Exception X where
  toException :: X -> SomeException
  toException = asyncExceptionToException

  fromException :: SomeException -> Maybe X
  fromException = asyncExceptionFromException

instance Show X where
  show _ = "«unconditional-jump»"

supply :: IntSupply
supply = unsafePerformIO IntSupply.new
{-# NOINLINE supply #-}
