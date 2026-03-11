-- | Interface to exo_helper
module Exomizer.Extern.Crunch
    ( crunchBackwards
    , deCrunchForward
    ) where

import           Data.ByteString        (ByteString)
import           Foreign
import           Foreign.C
import           System.IO.Unsafe

import           Exomizer.Data.Args
import           Exomizer.Extern.Args
import           Exomizer.Extern.Membuf

foreign import ccall safe "crunch_backwards"
  c_crunch_backwards
    :: Ptr Membuf
    -> Ptr Membuf
    -> Ptr CrunchOptions
    -> Ptr CrunchInfo
    -> IO ()

-- | @since 1.0.0
crunchBackwards :: CrunchOptions -> ByteString -> (ByteString, CrunchInfo)
crunchBackwards co bs =
  unsafePerformIO $
    withMembuf $ \inPtr -> do
      withMembuf $ \outPtr -> do
        withCrunchOptions co $ \coPtr -> do
          withCrunchInfo $ \ciPtr -> do
            membufAppend inPtr bs
            c_crunch_backwards inPtr outPtr coPtr ciPtr
            membufGet outPtr

foreign import ccall safe "decrunch"
  c_decrunch
    :: CInt
    -> Ptr Membuf
    -> Ptr Membuf
    -> IO ()

-- | @since 1.0.0
deCrunchForward :: ByteString -> ByteString
deCrunchForward bs =
  unsafePerformIO $
    withMembuf $ \inPtr -> do
      withMembuf $ \outPtr -> do
        membufAppend inPtr bs
        c_decrunch 0 inPtr outPtr
        membufGet outPtr
