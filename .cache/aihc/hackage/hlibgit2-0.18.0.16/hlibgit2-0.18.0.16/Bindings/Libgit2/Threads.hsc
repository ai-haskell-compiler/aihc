{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2.h>
module Bindings.Libgit2.Threads where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
#ccall git_threads_init , IO (CInt)
#ccall git_threads_shutdown , IO ()
