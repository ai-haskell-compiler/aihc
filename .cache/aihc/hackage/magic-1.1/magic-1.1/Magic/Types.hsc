{-  -*- Mode: haskell; -*-
Haskell magic Interface
Copyright (C) 2005 John Goerzen <jgoerzen@complete.org>

This code is under a 3-clause BSD license; see COPYING for details.
-}

{- |
   Module     : Magic.Types
   Copyright  : Copyright (C) 2005 John Goerzen
   License    : BSD

   Maintainer : John Goerzen,
   Maintainer : jgoerzen\@complete.org
   Stability  : provisional
   Portability: portable

Types for magic programs.

Written by John Goerzen, jgoerzen\@complete.org
-}

module Magic.Types(Magic,
                   MagicFlag(..))
where
import Foreign.Ptr
import Data.Word
import Data.Int
import Foreign.C.Types
import Foreign.ForeignPtr
import Magic.Data
import Magic.TypesLL

#include <magic.h>

{- | Main Magic object type.

Magic objects are automatically closed (and memory freed) when they are
garbage-collected by Haskell.  There is no need to explicitly close them.
-}
type Magic = ForeignPtr CMagic

