{-# LANGUAGE CPP              #-}
{-# LANGUAGE InterruptibleFFI #-}
{-# LANGUAGE Trustworthy      #-}
module Lukko.Internal.FillBytes (fillBytes) where

import Foreign.Marshal.Utils (fillBytes)
