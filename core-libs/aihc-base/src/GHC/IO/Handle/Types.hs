-- | Handle types. The definitions live in "GHC.Internal.IO.Types".
module GHC.IO.Handle.Types
  ( Handle (..),
    Handle__ (..),
    showHandle,
    checkHandleInvariants,
    BufferList (..),
    HandleType (..),
    isReadableHandleType,
    isWritableHandleType,
    isReadWriteHandleType,
    BufferMode (..),
    Newline (..),
    NewlineMode (..),
    nativeNewline,
    universalNewlineMode,
    noNewlineTranslation,
    nativeNewlineMode,
  )
where

import GHC.Internal.IO.Types
  ( BufferList (..),
    BufferMode (..),
    Handle (..),
    HandleType (..),
    Handle__ (..),
    Newline (..),
    NewlineMode (..),
    checkHandleInvariants,
    isReadWriteHandleType,
    isReadableHandleType,
    isWritableHandleType,
    nativeNewline,
    nativeNewlineMode,
    noNewlineTranslation,
    showHandle,
    universalNewlineMode,
  )
