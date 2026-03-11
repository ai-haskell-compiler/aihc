{- -*- Mode: haskell; -*-
Haskell magic Interface
Copyright (C) 2005 John Goerzen <jgoerzen@complete.org>

This code is under a 3-clause BSD license; see COPYING for details.
-}

{- |
   Module     : Magic.Init
   Copyright  : Copyright (C) 2005 John Goerzen
   License    : BSD

   Maintainer : John Goerzen,
   Maintainer : jgoerzen\@complete.org
   Stability  : provisional
   Portability: portable

Initialization and shutdown for magic programs

Written by John Goerzen, jgoerzen\@complete.org
-}

module Magic.Init(magicOpen, magicLoad, magicLoadDefault)
where

import Foreign.Ptr
import Foreign.C.String
import Magic.Types
import Foreign.C.Types
import Magic.Utils
import Magic.TypesLL
import Foreign.Marshal.Utils

{- | Create a Magic object.  You must call either 'magicLoadDefault'
or 'magicLoad' after this.
-}
magicOpen :: [MagicFlag] -> IO Magic
magicOpen mfl =
    fromMagicPtr "magicOpen" (magic_open flags)
    where flags = flaglist2int mfl

{- | Load the system's default magic database. -}
magicLoadDefault :: Magic -> IO ()
magicLoadDefault m = withMagicPtr m (\cmagic ->
    checkIntError "magicLoadDefault" m $ magic_load cmagic nullPtr)

{- | Load the specified magic database(s).  The given string may contain
multiple colon-separated pathnames. -}
magicLoad :: Magic -> String -> IO ()
magicLoad m s = withMagicPtr m (\cmagic ->
    withCString s (\cs ->
     checkIntError "magicLoad" m $ magic_load cmagic cs))
    
foreign import ccall unsafe "magic.h magic_open"
  magic_open :: CInt -> IO (Ptr CMagic)

foreign import ccall unsafe "magic.h magic_load"
  magic_load :: Ptr CMagic -> CString -> IO CInt