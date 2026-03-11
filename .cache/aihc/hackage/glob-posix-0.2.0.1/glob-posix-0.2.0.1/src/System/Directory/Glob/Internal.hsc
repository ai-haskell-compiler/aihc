{-|
Module      : System.Directory.Glob.Internal
Copyright   : (c) Reuben D'Netto 2016
License     : Apache 2.0
Maintainer  : rdnetto@gmail.com
Portability : POSIX
-}
module System.Directory.Glob.Internal where

import Data.Bits ((.|.))
import Data.Monoid ((<>))
import Foreign.C.Types (CInt(..))

#include <glob.h>


-- | Control flags for glob. Use 'globDefaults' if you have no special requirements.
--   To combine multiple flags, use the '<>' operator (re-exported here for convenience).
--   See <https://linux.die.net/man/3/glob man glob(3)> for more information.
data GlobFlag = GlobFlag !CInt

-- | Default value - equivalent to 0 for the C function.
globDefaults :: GlobFlag
globDefaults = GlobFlag 0


instance Semigroup GlobFlag where
    (GlobFlag a) <> (GlobFlag b) = GlobFlag (a .|. b)

instance Monoid GlobFlag where
    mempty = globDefaults


-- | Used for mutation of an existing structure - for internal use only.
#enum GlobFlag, GlobFlag, GLOB_APPEND
-- | Append a @/@ to each entry that is the path of a directory.
#enum GlobFlag, GlobFlag, GLOB_MARK
-- | If there are no matches, return the original pattern.
#enum GlobFlag, GlobFlag, globNoCheck  = GLOB_NOCHECK
-- | Disable the use of @\@ for escaping metacharacters.
#enum GlobFlag, GlobFlag, globNoEscape = GLOB_NOESCAPE
-- | Do not sort the entries before returning them.
#enum GlobFlag, GlobFlag, globNoSort   = GLOB_NOSORT

-- GNU extensions
#ifdef linux_HOST_OS
-- | Enable CSH-style brace expansion. e.g. foo.{txt,md}. Supports nested braces. (GNU extension)
#enum GlobFlag, GlobFlag, GLOB_BRACE
-- | Enables globNoCheck if the pattern contains no metacharacters. (GNU extension)
#enum GlobFlag, GlobFlag, globNoMagic  = GLOB_NOMAGIC
-- | Only return directories, if it is cheap to do so. (GNU extension)
#enum GlobFlag, GlobFlag, globOnlyDir  = GLOB_ONLYDIR
-- | Allow leading '.' to be matched by metacharacters.
#enum GlobFlag, GlobFlag, GLOB_PERIOD
-- | Substitute home directory for '~' or '~user' prefixes.
#enum GlobFlag, GlobFlag, GLOB_TILDE
-- | Like globTilde, but return no matches if there is no such user.
#enum GlobFlag, GlobFlag, GLOB_TILDE_CHECK
#endif

