{-# LANGUAGE ForeignFunctionInterface #-}
module Sound.JACK.FFIFree where

import Foreign.Ptr (Ptr)
import Foreign.C.String (CString)

-- | Free an array of NULL terminated port names returned by 'get_ports' and 'port_get_all_connections'.
foreign import ccall "static jack/jack.h jack_free"
  freePortNameArray :: Ptr CString -> IO ()
