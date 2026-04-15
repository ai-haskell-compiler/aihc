{- ORACLE_TEST pass -}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE InterruptibleFFI #-}

module X where

import Foreign.C.Types (CInt (..))

foreign import capi interruptible "termbox.h tb_peek_event"
  tb_peek_event :: IO CInt
