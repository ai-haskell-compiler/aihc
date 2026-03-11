--------------------------------------------------------------------------------
-- |
--
-- Module      :  Data.Memoizer.Session
-- Description :  Sessions for command memoizers
-- Copyright   :  (c) Alice Rixte 2024
-- License     :  BSD 3
-- Maintainer  :  alice.rixte@u-bordeaux.fr
-- Stability   :  unstable
-- Portability :  portable
--
-- Memoize several sessions and switch between them.
--
-- This enables the use of the @\\ghcisession{My Session}@ and
-- @\\ghcicontinue{My Session}@ command in the @ghci@ LuaTex package.
--
-- = Usage
--
--
-- Let us store the result of some commands (we alternate between @memo@ and
-- @memo'@ to avoid recursive definitions)
--
-- >>> import Prelude hiding (lookup)
--
-- >>> memo = initSession "main" :: SessionMemoizer String String String
-- >>> memo' = storeResult "x=1" "" memo
-- >>> memo = storeResult "y=2" "" memo'
-- >>> memo' = storeResult "x+y" "3" memo
--
-- Let use create a new session:
--
-- >>> memo = newSession "My Session" memo'
-- >>> memo' = storeResult "a=1" "" memo
--
-- Now if we create a new session called "main" again, we can use the memoized
-- values:
--
-- >>> memo = newSession "main" memo'
-- >>> lookup "x=1" memo
-- Just ""
--
--
-- But we still have some commands to add to "My Session":
--
-- >>> memo' = continueSession "My Session" memo
-- >>> memo = storeResult "a" "1" memo'
--
-- Now let's restart "My Session":
--
-- >>> memo' = newSession "My Session" memo
-- >>> lookup "a=1" memo
-- Just ""
-- >>> memo = nextCmd memo'
-- >>> lookup "a" memo
-- Just "1"
--
--------------------------------------------------------------------------------

module Data.Memoizer.Sessions
  ( SessionMemoizer (..)
  , initSession
  , newSession
  , continueSession
  , storeResult
  , deleteResult
  , lookup
  , nextCmd
  ) where

import Prelude hiding (lookup)
import qualified Data.Memoizer.Commands as Cmd
import qualified Data.Map as Map

-- | A container of  memoizers for sequences of commands.
--
-- * @k@ is the key representing the name of a session
-- * @a@ is the type of commands
-- * @b@ is the result of a command
--
data SessionMemoizer k a b = SessionMemoizer
  { sessionMap :: Map.Map k (Cmd.CmdMemoizer a b)
  , currentSession :: k
  }
  deriving (Show,Eq)

undefinedSession :: String
undefinedSession = "UndefinedSession : The current Session does not exist.\
  \ This should never happen. Please report this as a bug."

lookupCmd :: Ord k => SessionMemoizer k a b -> Cmd.CmdMemoizer a b
lookupCmd (SessionMemoizer ms k) = case Map.lookup k ms of
  Nothing -> error $ "lookup : " ++ undefinedSession
  Just m -> m

mapCmd :: Ord k =>
  (Cmd.CmdMemoizer a b -> Cmd.CmdMemoizer a b)
  -> SessionMemoizer k a b -> SessionMemoizer k a b
mapCmd f sm@(SessionMemoizer ms k) =
  sm {sessionMap = Map.insert k (f (lookupCmd sm)) ms }

-------------------------------------------------------------------------------

-- | Create a new session memoizer using a default session.
--
initSession :: Ord k => k -> SessionMemoizer k a b
initSession k = SessionMemoizer (Map.insert k Cmd.empty Map.empty) k

-- | Add a new session to memoize. If that session already existes, it is
-- @'Data.Memoizer.Commands.restart'@ed.
--
newSession :: Ord k => k -> SessionMemoizer k a b -> SessionMemoizer k a b
newSession k (SessionMemoizer ms _) =
  case Map.lookup k ms of
    Nothing -> SessionMemoizer (Map.insert k Cmd.empty ms) k
    Just m -> SessionMemoizer (Map.insert k (Cmd.restart m) ms) k

-- | Continue an existing session.
--
continueSession :: Ord k => k -> SessionMemoizer k a b -> SessionMemoizer k a b
continueSession k m = m {currentSession = k}

-- | Lookup the memoized result of the current session.
--
lookup :: (Eq a, Ord k) => a -> SessionMemoizer k a b -> Maybe b
lookup a  = Cmd.lookup a . lookupCmd

-- | Move the current session to the next command.
--
nextCmd :: Ord k => SessionMemoizer k a b -> SessionMemoizer k a b
nextCmd = mapCmd Cmd.nextCmd

-- | Store a result in the current session.
--
-- This will prevent access to any memoized result of the current session until
-- @'newSession'@ is used.
--
storeResult :: Ord k => a -> b -> SessionMemoizer k a b -> SessionMemoizer k a b
storeResult a b = mapCmd (Cmd.storeResult a b)

-- | Delete the current result in the current session.
--
-- This will prevent access to any memoized result  of the current session until
-- @'newSession'@ is used.
--
deleteResult :: Ord k
  =>  SessionMemoizer k a b -> SessionMemoizer k a b
deleteResult = mapCmd Cmd.deleteResult
