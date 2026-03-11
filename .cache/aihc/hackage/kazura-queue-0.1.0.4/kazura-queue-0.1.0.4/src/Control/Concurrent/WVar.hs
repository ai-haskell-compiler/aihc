{-# LANGUAGE BangPatterns #-}

-- | WVar is waitable 'IORef'.
--   It is similar to 'MVar' but different at some points.
--
-- * The latest (cached) value can be read
--     while someone is updating the value.
-- * Put operation can overwrite the value if the value is fresh
--      and cannot be blocked for waiting empty.
-- * WVar is strict. It means that the new value storing into the WVar
--     will be evaluated (WHNF) before actual storing.
--
-- There are two states in the user viewpoint.
--
-- [@Fresh@]    The 'WVar' is not being updated.
--              This state corresponds to full state of 'MVar'.
-- [@Updating@] The 'WVar' is being updated by someone.
--              This state corresponds to empty state of 'MVar'.
--              However, cached previous value can be read while Updating.

module Control.Concurrent.WVar
    (
      -- * WVar
      -- $wvar
      WVar
    , newWVar
    , takeWVar
    , tryTakeWVar
    , putWVar
    , readWVar
    , readFreshWVar
    , tryReadFreshWVar
      -- * WCached
      -- $wcached
    , WCached(..)
    , WTicket
    , cacheWVar
    , recacheWCached
    , readWTicket
    , takeWCached
    , tryTakeWCached
    , putWCached
    , tryPutWCached
    , readWCached
    , readFreshWCached
    , tryReadFreshWCached
    )
  where

import           Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Monad           as M
import qualified Data.Atomics            as Atm
import           Data.IORef              (IORef)
import qualified Data.IORef              as Ref

------------------------------
-- $wvar
--   Main functions of 'WVar'.

-- | "a" is the type of data in the WVar.
newtype WVar a = WVar (IORef (WContent a))
    deriving Eq

-- | Create a fresh 'WVar' that contains the supplied value.
{-# INLINE newWVar #-}
newWVar :: a -> IO (WVar a)
newWVar !a = WVar <$> Ref.newIORef WContent
    { wvalue = a
    , wstate = Fresh
    }

-- | Take the value of a 'WVar' like 'Control.Concurrent.MVar.takeMVar'.
--   It blocks when the 'WVar' is being updated.
{-# INLINE takeWVar #-}
takeWVar :: WVar a -> IO a
takeWVar wv = do
    wc <- cacheWVar wv
    wt1 <- takeWCached wc
    return $ readWTicket wt1

-- | Non-blocking version of 'takeWVar'.
{-# INLINE tryTakeWVar #-}
tryTakeWVar :: WVar a -> IO (Bool, a)
tryTakeWVar wv = cacheWVar wv >>= go
  where
    go wc = do
        (suc, wt1) <- tryTakeWCached wc
        case (suc, wstate (Atm.peekTicket wt1)) of
            (False, Fresh) -> go $ WCached wv wt1
            _              -> return (suc, readWTicket wt1)

-- | Put the supplied value into a 'WVar'.
--   It performs simple "write" when the 'WVar' is Fresh.
--   When the supplied value is already evaluated, it never blocks.
{-# INLINE putWVar #-}
putWVar :: WVar a -> a -> IO ()
putWVar wv a = do
    wc <- cacheWVar wv
    M.void $ putWCached wc a

-- | Read the cached value of the 'WVar'. It never blocks.
{-# INLINE readWVar #-}
readWVar :: WVar a -> IO a
readWVar (WVar ref) = wvalue <$> Ref.readIORef ref

-- | Read the fresh value of the 'WVar'.
--   It blocks and waits for a fresh value
--     when the 'WVar' is being updated by someone.
{-# INLINE readFreshWVar #-}
readFreshWVar :: WVar a -> IO a
readFreshWVar wv = do
    wc <- cacheWVar wv
    readWTicket <$> readFreshWCached wc

-- | Non-blocking version of 'readFreshWVar'
{-# INLINE tryReadFreshWVar #-}
tryReadFreshWVar :: WVar a -> IO (Bool, a)
tryReadFreshWVar wv = do
    wc <- cacheWVar wv
    (suc, wt1) <- tryReadFreshWCached wc
    return (suc, readWTicket wt1)

------------------------------
-- $wcached
--   Low level types and functions of 'WVar'.

-- | WCached consists of WVar and its cached ticket.
data WCached a = WCached
    { cachedVar    :: {-# UNPACK #-} !(WVar a)
    , cachedTicket ::                WTicket a
    } deriving Eq

instance Show a => Show (WCached a) where
    show (WCached _ wt) = "WCached " ++ show (readWTicket wt)

type WTicket a = Atm.Ticket (WContent a)
data WContent a = WContent
    { wvalue :: !a
    , wstate :: !(WState a)
    }

instance Show a => Show (WContent a) where
    show wcnt = concat [show (wstate wcnt), " ", show (wvalue wcnt)]

-- | State of the 'WVar'
data WState a =
      Fresh
      -- ^ The value of the 'WVar' is fresh. Not updating now.
    | Updating
      -- ^ The value of the 'WVar' is updating.
      --   No one wait for the fresh value.
    | Waiting  {-# UNPACK #-} !(MVar (WTicket a))
      -- ^ The value of WVar is updating
      --     and the fresh value is waited for by someone.

instance Show (WState a) where
    show Fresh        = "Fresh"
    show Updating     = "Updating"
    show (Waiting  _) = "Waiting"

-- | Cache the current value of the 'WVar' and create 'WCached'.
{-# INLINE cacheWVar #-}
cacheWVar :: WVar a -> IO (WCached a)
cacheWVar wv@(WVar ref) = do
    wt <- Atm.readForCAS ref
    return WCached { cachedVar = wv, cachedTicket = wt }

-- | Recache the 'WCached'.
--
--   @recacheWCached = cacheWVar . cachedVar@
recacheWCached :: WCached a -> IO (WCached a)
recacheWCached = cacheWVar . cachedVar

-- | Read the value of the 'WTicket'
{-# INLINE readWTicket #-}
readWTicket :: WTicket a -> a
readWTicket = wvalue . Atm.peekTicket

-- | Take the value of the 'WCached' like 'Control.Concurrent.MVar.takeMVar'.
--   It blocks when the 'WCached' is being updated.
{-# INLINE takeWCached #-}
takeWCached :: WCached a -> IO (WTicket a)
takeWCached wc@(WCached wv@(WVar ref) wt0) = do
    let wcnt0 = Atm.peekTicket wt0
    (suc, wt2) <- case wstate wcnt0 of
        Fresh -> do
            (suc, wt1) <- Atm.casIORef ref wt0 wcnt0 { wstate = Updating }
            if suc
                then return (True, wt1)
                else return (False, wt1)
        Updating -> do
            wt1 <- beginWaitWCached wc
            return (False, wt1) -- mv will be read in next time
        Waiting mv -> do
            wt1 <- MVar.readMVar mv
            return (False, wt1)
    if suc
        then return wt2
        else takeWCached $ WCached wv wt2

-- | Non-blocking version of 'takeWCached'.
{-# INLINE tryTakeWCached #-}
tryTakeWCached :: WCached a -> IO (Bool, WTicket a)
tryTakeWCached (WCached (WVar ref) wt0) = do
    let wcnt0 = Atm.peekTicket wt0
    case wstate wcnt0 of
        Fresh      -> Atm.casIORef ref wt0 wcnt0 { wstate = Updating }
        Updating   -> return (False, wt0)
        Waiting  _ -> return (False, wt0)

-- | Put the value to the 'WCached'.
--   It performs simple "write" when the 'WVar' is /Fresh/.
--   When the supplied value is already evaluated, it never blocks.
{-# INLINE putWCached #-}
putWCached :: WCached a -> a -> IO (WTicket a)
putWCached wc0 !a = do
    (suc, wt1) <- tryPutWCached wc0 a
    if suc
        then return wt1
        else do
            let wc1 = wc0 { cachedTicket = wt1 }
            putWCached wc1 a

-- | Put the value to a 'WCached'.
--   It performs simple "write" when the 'WVar' is /Fresh/.
--   It fails when the cache is obsoleted.
--   When the supplied value is already evaluated, it never blocks.
{-# INLINE tryPutWCached #-}
tryPutWCached :: WCached a -> a -> IO (Bool, WTicket a)
tryPutWCached (WCached (WVar ref) wt0) !a = do
    let !wcnt1 = WContent { wvalue = a, wstate = Fresh }
    (suc, wt1) <- Atm.casIORef ref wt0 wcnt1
    M.when suc $ case wstate $ Atm.peekTicket wt0 of
        -- putMVar is never blocked
        --   because it's done after casIORef had succeeded.
        Waiting mv -> MVar.putMVar mv wt1
        _          -> return ()
    return (suc, wt1)

-- | Read the cached value of the 'WCached'. It never blocks.
{-# INLINE readWCached #-}
readWCached :: WCached a -> a
readWCached (WCached _ wt0) = readWTicket wt0

-- | Read the /Fresh/ value of the 'WCached'.
--   It blocks and waits for a /Fresh/ value
--     when the 'WCached' is being updated by someone.
{-# INLINE readFreshWCached #-}
readFreshWCached :: WCached a -> IO (WTicket a)
readFreshWCached wc@(WCached wv wt0) = do
    let wcnt0 = Atm.peekTicket wt0
    (suc, wt2) <- case wstate wcnt0 of
        Fresh -> return (True, wt0)
        Updating -> do
            wt1 <- beginWaitWCached wc
            return (False, wt1)
        Waiting mv -> do
            wt1 <- MVar.readMVar mv
            return (True, wt1)
    if suc
        then return wt2
        else readFreshWCached $ WCached wv wt2

-- | Non-blocking version of 'readFreshWCached'
{-# INLINE tryReadFreshWCached #-}
tryReadFreshWCached :: WCached a -> IO (Bool, WTicket a)
tryReadFreshWCached (WCached _ wt0) = case wstate $ Atm.peekTicket wt0 of
    Fresh -> return (True,  wt0)
    _     -> return (False, wt0)

{-# INLINE beginWaitWCached #-}
beginWaitWCached :: WCached a -> IO (WTicket a)
beginWaitWCached (WCached (WVar ref) wt0) = do
    mv <- MVar.newEmptyMVar
    let wcnt0 = Atm.peekTicket wt0
        wcnt1 = wcnt0 { wstate = Waiting mv }
    snd <$> Atm.casIORef ref wt0 wcnt1

