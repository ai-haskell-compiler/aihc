{- |
This module provides type-safe access to filepath manipulations
independent from the operating system.

Normally you would import 'System.Path'
since this contains types fixed to your host system
and otherwise generic functions.
However, importing this explicitly
allows for manipulation of non-native paths.
-}
module System.Path.Generic (
    module System.Path.Internal,
    Core.System,
    ) where

import qualified System.Path.Internal as Core
import System.Path.Internal hiding (
    System(..),
    extSeparator, isExtSeparator,
    searchPathSeparator, isSearchPathSeparator,
    testAll,
    )
