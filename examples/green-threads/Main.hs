{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Main where

import Foreign.C.Types (CInt)
import GHC.IO (IO (..))
import GHC.Prim (fork#, yield#)

foreign import ccall unsafe puts :: Addr# -> IO CInt

forkIO :: IO a -> IO ()
forkIO (IO action) =
  IO
    ( \state ->
        -- IO is a newtype, so unpacking it alone does not enter the action.
        case action of
          forcedAction ->
            case fork# forcedAction state of
              (# nextState, _ #) -> (# nextState, () #)
    )

yield :: IO ()
yield =
  IO
    ( \state ->
        case yield# state of
          nextState -> (# nextState, () #)
    )

main :: IO ()
main =
  puts "Hello world main green thread"#
    >> forkIO
      ( puts "Hello from forked thread"#
          >> yield
          >> puts "Back in forked thread"#
      )
    >> puts "Back in main"#
    >> yield
    -- The child yields back once; yield again so it can finish.
    >> yield
