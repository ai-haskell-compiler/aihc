module Control.Once.Class where

class Once a where
    -- | memoize IO action or function returning IO to be peformed only
    -- once.
    --
    -- Any 'IO' action is suitable, as is any function with 'Hashable'
    -- arguments, returning value in IO monad.  Value of any of type below
    -- are okay to pass to 'once':
    --
    -- @
    -- IO Int
    -- Int -> IO ()
    -- Int -> Double -> IO (Char -> Int)
    -- @
    --
    -- Due implementation limitations, only up to 7 arguments are
    -- supported.
    once :: a -> IO a
