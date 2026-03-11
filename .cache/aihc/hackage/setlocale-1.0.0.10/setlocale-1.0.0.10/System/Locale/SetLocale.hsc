{-# LANGUAGE DeriveDataTypeable
           , ForeignFunctionInterface #-}

-- Copyright (c) 2014, Sven Bartscher
-- Look the file LICENSE.txt in the toplevel directory of the source tree for
-- more information.

module System.Locale.SetLocale ( Category(..)
                               , categoryToCInt
                               , setLocale
                               )
    where

#include <locale.h>

import Data.Typeable (Typeable)
import Foreign.C ( peekCString
                 , CString
                 , withCString
                 , CInt(CInt) -- Newtypes are only allowed in foreign
                 )            -- declarations if their constructor is
                              -- in scope.
import Foreign.Ptr (nullPtr)

data Category = LC_ALL
              | LC_COLLATE
              | LC_CTYPE
              | LC_MESSAGES
              | LC_MONETARY
              | LC_NUMERIC
              | LC_TIME
                deriving (Bounded, Enum, Eq, Ord, Read, Show, Typeable)

categoryToCInt :: Category -> CInt
categoryToCInt LC_ALL = #const LC_ALL
categoryToCInt LC_COLLATE = #const LC_COLLATE
categoryToCInt LC_CTYPE = #const LC_CTYPE
#ifdef LC_MESSAGES
categoryToCInt LC_MESSAGES = #const LC_MESSAGES
#else
categoryToCInt LC_MESSAGES = -1
#endif
categoryToCInt LC_MONETARY = #const LC_MONETARY
categoryToCInt LC_NUMERIC = #const LC_NUMERIC
categoryToCInt LC_TIME = #const LC_TIME

foreign import ccall "locale.h setlocale" c_setlocale :: CInt -> CString -> IO CString

setLocale :: Category -> Maybe String -> IO (Maybe String)
#ifndef LC_MESSAGES
setLocale LC_MESSAGES _ = return Nothing
#endif
setLocale c Nothing = c_setlocale (categoryToCInt c) nullPtr >>= checkReturn
setLocale c (Just locale) = (withCString locale $ c_setlocale $ categoryToCInt c)
                            >>= checkReturn

checkReturn :: CString -> IO (Maybe String)
checkReturn r
    | r == nullPtr = return Nothing
    | otherwise = fmap Just $ peekCString r
