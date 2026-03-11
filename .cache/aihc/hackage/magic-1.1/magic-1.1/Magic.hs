{- -*- Mode: haskell; -*-
Haskell Magic Interface
Copyright (C) 2005 John Goerzen <jgoerzen@complete.org>

This code is under a 3-clause BSD license; see COPYING for details.
-}

{- |
   Module     : Magic
   Copyright  : Copyright (C) 2005 John Goerzen
   License    : BSD

   Maintainer : John Goerzen,
   Maintainer : jgoerzen\@complete.org
   Stability  : provisional
   Portability: portable

Top-level Magic module.

Written by John Goerzen, jgoerzen\@complete.org

Foo bar
-}

module Magic (-- * Basic Types
             module Magic.Types,
             -- * Initialization
             module Magic.Init,
             -- * Operation
             module Magic.Operations
            )
where
import Magic.Types
import Magic.Init
import Magic.Operations
