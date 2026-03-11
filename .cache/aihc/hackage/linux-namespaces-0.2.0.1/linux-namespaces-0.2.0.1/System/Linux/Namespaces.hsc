{- |
Module      : System.Linux.Namespaces

Stability   : provisional
Portability : non-portable (requires Linux)

This module provides bindings to the @unshare(2)@ and @setns(2)@ linux
system calls. The former can be used to create new namespaces and move
the calling process to them, whereas the latter can be used to move the
calling process to an already existing namespace created by some other
process.

Note that linux provides another function related to namespaces which is
not supported by this module: @clone(2)@. This function works like
@fork(2)@ and is used to create new namespaces (like @unshare(2)@).
Unfortunately, like @fork(2)@, it does not interact well with GHC'c RTS
which is why it has been omitted from this module.

/Note/: Using this module in a program that uses the threaded RTS does
not make much sense. Namespaces are per process/thread and manipulating
them in one thread will not affect the namespaces of the other threads
of the same process. The threaded RTS makes it is hard to predict what
OS thread will be used to run the haskell threads. Therefore, using this
module in such applications will result in unpredictable behavior.
Similarly, using this module in @ghci@ is problematic too.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Linux.Namespaces
    (
    -- * Main types and functions
      Namespace(..)
    , unshare
    , setNamespace

    -- * Utility functions
    , enterNamespace
    , NamespaceID
    , getNamespaceID

    -- * User/Group mappings
    , UserMapping(..)
    , GroupMapping(..)
    , writeUserMappings
    , writeGroupMappings

    -- * Setting offsets for virtualized clocks
    , Clock(..)
    , setClockOffset

    -- * Example
    -- $example
    ) where

#define _GNU_SOURCE
#include <sched.h>

import Foreign
import Foreign.C
import System.Posix.Types (Fd(..), ProcessID, UserID, GroupID, EpochTime)
import System.Posix.IO
import System.Posix.Files (readSymbolicLink)
import Control.Exception (bracket)
import Data.List (foldl')
import Data.Char (isDigit)
import Control.Arrow (first)
import Control.Monad (when)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString       as S
import Data.ByteString (ByteString)
import System.IO.Error (modifyIOError, ioeSetLocation)

--------------------------------------------------------------------------------

-- | Types of namespaces.
data Namespace
    = IPC
    | Network
    | Mount
    | PID
    | User
    | UTS
    | CGroup
    | Time
  deriving (Show, Read, Eq, Bounded, Enum)

toCloneFlags :: Namespace -> CInt
toCloneFlags ns =
    case ns of
        IPC     -> (#const CLONE_NEWIPC)
        Network -> (#const CLONE_NEWNET)
        Mount   -> (#const CLONE_NEWNS)
        PID     -> (#const CLONE_NEWPID)
        User    -> (#const CLONE_NEWUSER)
        UTS     -> (#const CLONE_NEWUTS)
        CGroup  -> (#const CLONE_NEWCGROUP)
#ifdef CLONE_NEWTIME
        Time    -> (#const CLONE_NEWTIME)
#else
        Time    -> error "CLONE_NEWTIME is not defined"
#endif

toProcName :: Namespace -> String
toProcName ns =
    case ns of
        IPC     -> "ipc"
        Network -> "net"
        Mount   -> "mnt"
        PID     -> "pid"
        User    -> "user"
        UTS     -> "uts"
        CGroup  -> "cgroup"
        Time    -> "time"

-- | Detach the process from one or more namespaces and move it to new
-- ones. See the man page of @unshare(2)@ for more details.
unshare :: [Namespace] -> IO ()
unshare nss =
    throwErrnoIfMinus1_ "unshare" $ c_unshare flags
  where
    flags = foldl' (.|.) 0 (map toCloneFlags nss)

-- | Move the process to an already existing namespace. See the man page
-- of @setns(2)@ for more details. See also 'enterNamespace' for a
-- slightly higher level version of this function.
setNamespace
    :: Fd -- ^ A file descriptor referring to a namespace file in a
          -- @\/proc\/[pid]\/ns\/@ directory.
    -> Maybe Namespace -- ^ Specify the namespace type that the file
                       -- descriptor must refer to. If the two types do not
                       -- match, the function will fail. Use 'Nothing' to
                       -- allow any type.
    -> IO ()
setNamespace fd mns =
    throwErrnoIfMinus1_ "setNamespace" $ c_setns fd nstype
  where
    nstype = maybe 0 toCloneFlags mns

--------------------------------------------------------------------------------

-- | Move the process to an already existing namespace. This is a wrapper
-- around 'setNamespace'. This function requires @\/proc@ to be mounted.
enterNamespace
    :: ProcessID -- ^ The @pid@ of any process in the target namespace.
    -> Namespace -- ^ The type of the namespace.
    -> IO ()
enterNamespace pid ns =
    bracket openFd' closeFd $ \fd ->
        setNamespace fd (Just ns)
  where
    openFd' = ioeSetLoc "enterNamespace" $
#if MIN_VERSION_unix(2,8,0)
        openFd path ReadOnly defaultFileFlags {nonBlock = True}
#else
        openFd path ReadOnly Nothing defaultFileFlags {nonBlock = True}
#endif
    path = toProcPath (Just pid) ns

-- | A unique namespace id.
newtype NamespaceID = NamespaceID CInt
  deriving (Eq, Ord, Enum, Integral, Num, Real)

instance Show NamespaceID where
    show (NamespaceID x) = show x

instance Read NamespaceID where
    readsPrec prec s = map (first NamespaceID) $ readsPrec prec s

-- | Retrieve the id of a Namespace. Useful for debugging. This
-- function requires @\/proc@ to be mounted.
getNamespaceID
    :: Maybe ProcessID -- ^ The @pid@ of any process in the target
                       -- namespace. Use 'Nothing' for the namespace
                       -- of the calling process.
    -> Namespace       -- ^ The type of the namespace.
    -> IO NamespaceID
getNamespaceID mpid ns = do
    s <- ioeSetLoc "getNamespaceID" $ readSymbolicLink path
    let s' = takeWhile isDigit $ dropWhile (not . isDigit) s
    return (read s')
  where
    path = toProcPath mpid ns

--------------------------------------------------------------------------------

-- | A single user mapping, used with user namespaces. See
-- @user_namespaces(7)@ for more details.
data UserMapping = UserMapping UserID UserID Int
  deriving (Show, Read, Eq)

-- | A single group mapping, used with user namespaces. See
-- @user_namespaces(7)@ for more details.
data GroupMapping = GroupMapping GroupID GroupID Int
  deriving (Show, Read, Eq)

-- | Define the user mappings for the specified user namespace. This
-- function requires @\/proc@ to be mounted. See @user_namespaces(7)@
-- for more details.
writeUserMappings
    :: Maybe ProcessID -- ^ The @pid@ of any process in the target user
                       -- namespace. Use 'Nothing' for the namespace
                       -- of the calling process.
    -> [UserMapping]   -- ^ The mappings.
    -> IO ()
writeUserMappings mpid ms =
    ioeSetLoc "writeUserMappings" $
        writeProcFile path (C.pack s)
  where
    path = toProcDir mpid ++ "/uid_map"
    s = concatMap toStr ms
    toStr (UserMapping o i l) = show o ++ " " ++ show i ++ " " ++ show l ++ "\n"

-- | Define the group mappings for the specified user namespace. This
-- function requires @\/proc@ to be mounted. See @user_namespaces(7)@
-- for more details.
writeGroupMappings
    :: Maybe ProcessID -- ^ The @pid@ of any process in the target user
                       -- namespace. Use 'Nothing' for the namespace
                       -- of the calling process.
    -> [GroupMapping]  -- ^ The mappings.
    -> Bool            -- ^ Prevent processes in the child user namespace
                       -- from calling @setgroups@. This is needed if the
                       -- calling process does not have the @CAP_SETGID@
                       -- capability in the parent namespace.
    -> IO ()
writeGroupMappings mpid ms denySetgroups =
    ioeSetLoc "writeGroupMappings" $ do
        when denySetgroups $
            writeProcFile (dir ++ "/setgroups") (C.pack "deny")
        writeProcFile (dir ++ "/gid_map") (C.pack s)
  where
    dir = toProcDir mpid
    s = concatMap toStr ms
    toStr (GroupMapping o i l) =
        show o ++ " " ++ show i ++ " " ++ show l ++ "\n"

--------------------------------------------------------------------------------

-- | The virtualized clock whose offset is set
-- @time_namespaces(7)@ for more details.
data Clock = Monotonic | Boottime
  deriving (Show, Read, Eq)

-- | Set the offset for a virtualized clock. This can only be called before any
-- process has been created in the time namespace. This function requires
-- @\/proc@ to be mounted. See @time_namespaces(7)@ for more details.
setClockOffset
    :: Clock           -- ^ Specify the clock whose offset is set.
    -> EpochTime       -- ^ The seconds component of the offset. This value
                       -- can be negative.
    -> CLong           -- ^ The nanoseconds component of the offset. This
                       -- value must not be negative.
    -> IO ()
setClockOffset clock offsetSecs offsetNanosecs =
    ioeSetLoc "setClockOffset" $ do
        writeProcFile "/proc/self/timens_offsets" (C.pack s)
  where
    s = concat [clockId, " ", show offsetSecs, " ", show offsetNanosecs]
    clockId = case clock of
        Monotonic -> "monotonic"
        Boottime -> "boottime"

--------------------------------------------------------------------------------

writeProcFile :: FilePath -> ByteString -> IO ()
writeProcFile path bs =
#if MIN_VERSION_unix(2,8,0)
    bracket (openFd path WriteOnly defaultFileFlags) closeFd $ \fd ->
#else
    bracket (openFd path WriteOnly Nothing defaultFileFlags) closeFd $ \fd ->
#endif
        S.useAsCStringLen bs $ \(ptr, nb) ->
            fdWriteBuf fd (castPtr ptr) (fromIntegral nb) >> return ()

toProcPath :: Maybe ProcessID -> Namespace -> String
toProcPath mpid ns = toProcDir mpid ++ "/ns/" ++ toProcName ns
{-# INLINE toProcPath #-}

toProcDir :: Maybe ProcessID -> String
toProcDir mpid = "/proc/" ++ maybe "self" show mpid
{-# INLINE toProcDir #-}

ioeSetLoc :: String -> IO r -> IO r
ioeSetLoc loc = modifyIOError (flip ioeSetLocation loc)

--------------------------------------------------------------------------------

foreign import ccall unsafe "unshare"
    c_unshare :: CInt -> IO CInt

foreign import ccall unsafe "setns"
    c_setns :: Fd -> CInt -> IO CInt

--------------------------------------------------------------------------------

-- $example
-- Here's an example of creating a new network namespace. We also create
-- a user namespace. This allows us to execute the program as an
-- unprivileged user.
--
-- > import System.Process
-- > import System.Posix.User
-- > import System.Linux.Namespaces
-- >
-- > main :: IO ()
-- > main = do
-- >     putStrLn "*** Network interfaces in the parent namespace ***"
-- >     callCommand "ip addr"
-- >     putStrLn ""
-- >
-- >     -- find the uid, we must do that before unshare
-- >     uid <- getEffectiveUserID
-- >
-- >     unshare [User, Network]
-- >     -- map current user to user 0 (i.e. root) inside the namespace
-- >     writeUserMappings Nothing [UserMapping 0 uid 1]
-- >
-- >     -- enable the loopback interface
-- >     -- we can do that because we are root inside the namespace
-- >     callCommand "ip link set dev lo up"
-- >
-- >     putStrLn "*** Network interfaces in the new namespace ***"
-- >     callCommand "ip addr"
