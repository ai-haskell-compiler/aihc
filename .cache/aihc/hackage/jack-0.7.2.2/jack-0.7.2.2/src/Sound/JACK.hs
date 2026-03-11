{-# LANGUAGE ForeignFunctionInterface #-}
{-
    JACK bindings for Haskell
    Copyright (C) 2011-2013 Henning Thielemann
    Copyright (C) 2007 Soenke Hahn

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along
    with this program; if not, write to the Free Software Foundation, Inc.,
    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
-}

{-|

The Jack module defines types and functions that allows you to
use the JACK Audio Connection Kit.

-}
module Sound.JACK (
    -- * general stuff
    Client(..),
    newClient,
    newClientDefault,
    disposeClient,
    withClient,
    withClientDefault,
    clientClose,

    activate,
    deactivate,
    withActivation,

    PortType,
    Direction, Input, Output,
    JackFFI.UnknownType, UnknownDirection,

    Port(..),
    newPort,
    disposePort,
    withPort,

    PortSet,
    setOfPort,
    setOfPorts,

    Process,
    connect,
    disconnect,
    makeProcess,
    setProcess,
    withProcess,

    Freewheel,
    makeFreewheel,
    setFreewheel,
    withFreewheel,

    BufferSize,
    makeBufferSize,
    setBufferSize,
    withBufferSize,

    SampleRate,
    makeSampleRate,
    setSampleRate,
    withSampleRate,

    PortRename,
    makePortRename,
    setPortRename,
    withPortRename,

    GraphOrder,
    makeGraphOrder,
    setGraphOrder,
    withGraphOrder,

    XRun,
    makeXRun,
    setXRun,
    withXRun,

    Latency,
    LatencyCallbackMode,
    makeLatency,
    setLatency,
    withLatency,
    JackFFI.jackCaptureLatency,
    JackFFI.jackPlaybackLatency,

    JackFFI.LatencyRange(..),
    getLatencyRange,
    setLatencyRange,
    recomputeTotalLatencies,

    getBufferSize,
    getSampleRate,
    lastFrameTime,

    ClientRegistration,
    makeClientRegistration,
    setClientRegistration,
    withClientRegistration,

    JackFFI.PortId,
    makePortRegistration,
    setPortRegistration,
    withPortRegistration,

    PortConnect,
    makePortConnect,
    setPortConnect,
    withPortConnect,

    portById,
    portByName,

    portName,
    portShortName,
    portAliases,

    getPorts,
    portGetAllConnections,

    narrowPort,
    narrowPortType,
    narrowPortDirection,
    switchUnknownTypePort,
    switchUnknownDirectionPort,

    JackFFI.NFrames(JackFFI.NFrames),
    nframesIndices, nframesBounds,
    quit, waitForBreakAndClose,
    waitForBreak,

    -- * Exceptions
    handleExceptions,

    ) where

import Sound.JACK.Private
        (Port(Port), PortType, PortTypeString, portTypeString, Client(Client),
         bracket, bracket_, liftErrno, withCString, alloca, )

import qualified Sound.JACK.Exception as JackExc
import qualified Sound.JACK.FFI as JackFFI
import qualified Sound.JACK.FFIFree as JackFFIFree
import Sound.JACK.FFI.MIDI (EventBuffer, )
import Sound.JACK.FFI
        (Process, Input, Output, Freewheel, BufferSize, SampleRate,
         ClientRegistration, PortRename, PortConnect, GraphOrder,
         XRun, Latency, LatencyCallbackMode,
         nframesIndices, nframesBounds, )

import qualified Control.Monad.Exception.Synchronous as Sync
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Exception as Exc
import Control.Monad (join)
import Control.Applicative (Const(Const), (<$>), (<*>), (<$), )

import Foreign.Marshal.Utils (with)
import qualified Foreign.Marshal.Array as Array
import qualified Foreign.C.String as CString
import qualified Foreign.C.Types as C
import Foreign.Storable (peek, )
import Foreign.Ptr
        (Ptr, FunPtr, nullPtr, castPtr, freeHaskellFunPtr, nullFunPtr, )
import Foreign.C.String (CString, peekCString, )
import Foreign.C.Error (Errno(Errno), eOK)
import qualified Data.EnumBitSet as ES

import Control.Concurrent (MVar, putMVar, newEmptyMVar, takeMVar, threadDelay)

import qualified System.IO as IO
import Data.Monoid (Monoid, mempty, mappend, )
import Data.Semigroup (Semigroup, (<>), )



class Direction dir where
    switchDir :: f Input -> f Output -> f dir

instance Direction Input  where switchDir f _ = f
instance Direction Output where switchDir _ f = f


type DirFlagSet = Const JackFFI.PortFlagSet

dirFlags :: Direction dir => Const JackFFI.PortFlagSet dir
dirFlags = switchDir (Const JackFFI.portIsInput) (Const JackFFI.portIsOutput)


{- |
Type argument for Jack ports
where we do not know
whether it is an input or an output port.
-}
data UnknownDirection = UnknownDirection

_dummyUnknownDirection :: UnknownDirection
_dummyUnknownDirection = UnknownDirection



-- | Constructs a new Jack client.
newClient ::
    (JackExc.ThrowsStatus e) =>
       String -- ^ name of the JACK server
    -> String -- ^ name of the client
    -> Sync.ExceptionalT e IO Client
newClient server name =
    withCString server $ \cserverS ->
    withCString name $ \cclientS ->
    alloca $ \status -> do
        let opt = ES.fromEnums [JackFFI.ServerName, JackFFI.NoStartServer]
        client <- Trans.lift $ JackFFI.client_open cclientS opt status cserverS
        _ <- checkStatus client status
        return (Client client)

-- | Creates a new JACK client with the @default@ server
newClientDefault ::
    (JackExc.ThrowsStatus e) =>
       String -- ^ name of the client
    -> Sync.ExceptionalT e IO Client
newClientDefault name = newClient defaultServer name

defaultServer :: String
defaultServer = "default"

disposeClient ::
    (JackExc.ThrowsErrno e) =>
    Client -> Sync.ExceptionalT e IO ()
disposeClient client =
    liftErrno $ JackFFI.client_close (getClient client)

{- |
Run a block of code with a newly allocated client.
Do not use the client outside the block.
-}
withClient ::
    (JackExc.ThrowsStatus e) =>
       String -- ^ name of the JACK server
    -> String -- ^ name of the client
    -> (Client -> Sync.ExceptionalT e IO a)
    -> Sync.ExceptionalT e IO a
withClient server name =
    bracket
        (newClient server name)
        (Trans.lift . fmap (const ()) .
         JackFFI.client_close . getClient)

withClientDefault ::
    (JackExc.ThrowsStatus e) =>
       String -- ^ name of the client
    -> (Client -> Sync.ExceptionalT e IO a)
    -> Sync.ExceptionalT e IO a
withClientDefault =
    withClient defaultServer


checkStatus ::
    (JackExc.ThrowsStatus e) =>
       Ptr a
    -> Ptr JackFFI.StatusSet
    -> Sync.ExceptionalT e IO JackFFI.StatusSet
checkStatus c s = do
    errCode <- Trans.lift $ peek s
    Sync.assertT (JackExc.status errCode) (c /= nullPtr)
    return errCode


newPortByType ::
    (PortType typ, Direction dir,
     JackExc.ThrowsPortRegister e) =>
    PortTypeString typ -> DirFlagSet dir -> Client -> String ->
    Sync.ExceptionalT e IO (Port typ dir)
newPortByType (Const portTyp) (Const dir) (Client client) name =
    withPortName name $ \cPortName ->
    withCString portTyp $ \pType -> do
--     putStrLn ("register..." ++ show (client, cstring, pType, inout, 0))
        port <-
            Trans.lift $
            JackFFI.port_register client cPortName
                (JackFFI.PortType pType) dir 0
        Sync.assertT JackExc.portRegister (port/=nullPtr)
        return $ Port port

{- |
Better use 'withPort' that also handles freeing the port.
-}
newPort ::
    (PortType typ, Direction dir,
     JackExc.ThrowsPortRegister e) =>
       Client -- ^ Jack client
    -> String -- ^ name of the input port
    -> Sync.ExceptionalT e IO (Port typ dir)
newPort = newPortByType portTypeString dirFlags


disposePort ::
    (PortType typ, Direction dir,
     JackExc.ThrowsErrno e) =>
    Client -> Port typ dir -> Sync.ExceptionalT e IO ()
disposePort client =
    liftErrno . JackFFI.port_unregister (getClient client) . getPort

{- |
Creates a new port for the given client and delete it after usage.
The port manages audio or MIDI data in input or output direction
depending on the Port type.
Usually the required port type can be inferred from following actions
that use that port.

Do not use the port outside the enclosed block.
-}
withPort ::
    (PortType typ, Direction dir,
     JackExc.ThrowsPortRegister e,
     JackExc.ThrowsErrno e) =>
       Client -- ^ Jack client
    -> String -- ^ name of the input port
    -> (Port typ dir -> Sync.ExceptionalT e IO a)
    -> Sync.ExceptionalT e IO a
withPort client name =
    bracket (newPort client name) (disposePort client)



-- | activates the given Jack client
activate ::
    (JackExc.ThrowsErrno e) =>
    Client -> Sync.ExceptionalT e IO ()
activate client =
    liftErrno $ JackFFI.activate $ getClient client

deactivate ::
    (JackExc.ThrowsErrno e) =>
    Client -> Sync.ExceptionalT e IO ()
deactivate client =
    liftErrno $ JackFFI.deactivate $ getClient client

withActivation ::
    (JackExc.ThrowsErrno e) =>
    Client ->
    Sync.ExceptionalT e IO () ->
    Sync.ExceptionalT e IO ()
withActivation client =
    bracket_ (activate client) (deactivate client)

-- | closes the given Jack client without causing any trouble (hopefully)
clientClose ::
    (JackExc.ThrowsErrno e) =>
    Client -> PortSet ->
    Sync.ExceptionalT e IO ()
clientClose client (PortSet ports) = do
    mapM_ (liftErrno . JackFFI.port_unregister (getClient client)) ports
    deactivate client
    disposeClient client


{-
dummy use of C that we imported in order to expose CInt constructor
-}
_dummy :: C.CInt
_dummy = undefined

foreign import ccall "wrapper"
    makeProcess :: Process arg -> IO (FunPtr (Process arg))


getClient :: Client -> Ptr JackFFI.Client
getClient (Client x) = x

getPort :: Port typ dir -> Ptr (JackFFI.Port typ)
getPort (Port x) = x

withPortName ::
    String -> (JackFFI.PortName -> Sync.ExceptionalT e IO a) ->
    Sync.ExceptionalT e IO a
withPortName name f =
    withCString name $ f . JackFFI.PortName

peekPortNameArray :: Ptr CString -> IO [String]
peekPortNameArray a =
    if a == nullPtr
    then return []
    else Exc.finally
            (mapM peekCString =<< Array.peekArray0 nullPtr a)
            (JackFFIFree.freePortNameArray a)

-- | Returns the names of all existing ports.
getPorts :: Client -- ^ the Jack client
    -> IO [String] -- ^ the names as a list of strings
getPorts client =
    CString.withCString "" $ \empty -> do
        JackFFI.get_ports (getClient client) empty empty 0
            >>= peekPortNameArray


connect ::
    (JackExc.ThrowsErrno e) =>
    Client -> String -> String ->
    Sync.ExceptionalT e IO ()
connect client outport inport =
    withPortName outport $ \ outCString ->
    withPortName inport  $ \ inCString  ->
    liftErrno $ JackFFI.connect (getClient client) outCString inCString


disconnect ::
    (JackExc.ThrowsErrno e) =>
    Client -> String -> String ->
    Sync.ExceptionalT e IO ()
disconnect client outport inport =
    withPortName outport $ \ outCString ->
    withPortName inport  $ \ inCString  ->
    liftErrno $ JackFFI.disconnect (getClient client) outCString inCString


{- |
A collection of mixed types of ports.
It is mainly needed for freeing all allocated ports.
-}
newtype PortSet = PortSet [Ptr (JackFFI.Port ())]

instance Semigroup PortSet where
    PortSet a <> PortSet b = PortSet (a <> b)

instance Monoid PortSet where
    mempty = PortSet mempty
    mappend = (<>)

setOfPort ::
    (PortType typ, Direction dir) =>
    Port typ dir -> PortSet
setOfPort =
    PortSet . (:[]) . castPtr . getPort

setOfPorts ::
    (PortType typ, Direction dir) =>
    [Port typ dir] -> PortSet
setOfPorts =
    PortSet . map (castPtr . getPort)



{-# DEPRECATED quit "Write your own function instead." #-}
quit ::
    MVar () -> Client -> PortSet -> IO ()
quit mvar client ports = do
    putStrLn "quitting..."
    Sync.resolveT
        (\(Errno e) ->
           IO.hPutStrLn IO.stderr $
           "exception when closing with errno: " ++ show e)
        (clientClose client ports)
    threadDelay 1000000
    putMVar mvar ()

{-# DEPRECATED waitForBreakAndClose "Write your own function instead." #-}
waitForBreakAndClose ::
    Client -> PortSet -> IO ()
waitForBreakAndClose client ports = do
    mvar <- newEmptyMVar
    Exc.finally waitForBreak (quit mvar client ports)
    takeMVar mvar

waitForBreak :: IO ()
waitForBreak =
    let go = getLine >> go
    in  go


handleExceptions ::
    Sync.ExceptionalT JackExc.All IO () ->
    IO ()
handleExceptions =
    Sync.resolveT $ IO.hPutStrLn IO.stderr . JackExc.toStringWithHead


{-# INLINE withCallback #-}
withCallback ::
    (JackExc.ThrowsErrno e) =>
    (callback -> IO (FunPtr callbackExt)) ->
    (Client -> FunPtr callbackExt -> Ptr () -> Sync.ExceptionalT e IO ()) ->
    Client -> callback ->
    Sync.ExceptionalT e IO a ->
    Sync.ExceptionalT e IO a
withCallback makeCallback setCallback client proc act =
    bracket
        (do
            procPtr <- Trans.lift $ makeCallback proc
            setCallback client procPtr nullPtr
            return procPtr)
        (\procPtr -> do
            setCallback client nullFunPtr nullPtr
            Trans.lift $ freeHaskellFunPtr procPtr)
        (const act)


setProcess ::
    (JackExc.ThrowsErrno e) =>
    Client ->
    FunPtr (Process arg) ->
    Ptr arg ->
    Sync.ExceptionalT e IO ()
setProcess client procPtr arg =
    liftErrno $ JackFFI.set_process_callback (getClient client) procPtr arg

{- |
The callback function must respond in real-time,
i.e. in a bounded amout of time.
That is, strictly spoken it must not wait for anything,
e.g. it must not wait for locks and it must not allocate memory.
In Haskell this is practically impossible
because even simplest operations allocate memory.
If the callback needs to much time, JACK will shut down your client.
The best you can do is to hope that nothing evil happens.
-}
withProcess ::
    (JackExc.ThrowsErrno e) =>
    Client ->
    (JackFFI.NFrames -> Sync.ExceptionalT Errno IO ()) ->
    Sync.ExceptionalT e IO a ->
    Sync.ExceptionalT e IO a
withProcess =
    withCallback
        (\proc ->
            makeProcess $ \ nframes _arg ->
                Sync.switchT return (\() -> return eOK) $ proc nframes)
        setProcess

foreign import ccall "wrapper"
    makeFreewheel ::
        JackFFI.Freewheel arg -> IO (FunPtr (JackFFI.Freewheel arg))

setFreewheel ::
    (JackExc.ThrowsErrno e) =>
    Client ->
    FunPtr (JackFFI.Freewheel arg) ->
    Ptr arg ->
    Sync.ExceptionalT e IO ()
setFreewheel client procPtr arg =
    liftErrno $ JackFFI.set_freewheel_callback (getClient client) procPtr arg

withFreewheel ::
    (JackExc.ThrowsErrno e) =>
    Client ->
    (Bool -> IO ()) ->
    Sync.ExceptionalT e IO a ->
    Sync.ExceptionalT e IO a
withFreewheel =
    withCallback
        (\fw ->
            makeFreewheel $ \ starting _arg ->
                fw (if starting == 0 then False else True))
        setFreewheel

foreign import ccall "wrapper"
    makeBufferSize ::
        JackFFI.BufferSize arg -> IO (FunPtr (JackFFI.BufferSize arg))

setBufferSize ::
    (JackExc.ThrowsErrno e) =>
    Client ->
    FunPtr (JackFFI.BufferSize arg) ->
    Ptr arg ->
    Sync.ExceptionalT e IO ()
setBufferSize client procPtr arg =
    liftErrno $ JackFFI.set_buffer_size_callback (getClient client) procPtr arg

withBufferSize ::
    (JackExc.ThrowsErrno e) =>
    Client ->
    (JackFFI.NFrames -> Sync.ExceptionalT Errno IO ()) ->
    Sync.ExceptionalT e IO a ->
    Sync.ExceptionalT e IO a
withBufferSize =
    withCallback
        (\bs ->
            makeBufferSize $ \ nframes _arg ->
                Sync.switchT return (\() -> return eOK) $ bs nframes)
        setBufferSize

foreign import ccall "wrapper"
    makeSampleRate ::
        JackFFI.SampleRate arg -> IO (FunPtr (JackFFI.SampleRate arg))

setSampleRate ::
    (JackExc.ThrowsErrno e) =>
    Client ->
    FunPtr (JackFFI.SampleRate arg) ->
    Ptr arg ->
    Sync.ExceptionalT e IO ()
setSampleRate client procPtr arg =
    liftErrno $ JackFFI.set_sample_rate_callback (getClient client) procPtr arg

withSampleRate ::
    (JackExc.ThrowsErrno e) =>
    Client ->
    (JackFFI.NFrames -> Sync.ExceptionalT Errno IO ()) ->
    Sync.ExceptionalT e IO a ->
    Sync.ExceptionalT e IO a
withSampleRate =
    withCallback
        (\bs ->
            makeSampleRate $ \ nframes _arg ->
                Sync.switchT return (\() -> return eOK) $ bs nframes)
        setSampleRate

foreign import ccall "wrapper"
    makePortRename ::
        JackFFI.PortRename arg -> IO (FunPtr (JackFFI.PortRename arg))

setPortRename ::
    (JackExc.ThrowsErrno e) =>
    Client ->
    FunPtr (JackFFI.PortRename arg) ->
    Ptr arg ->
    Sync.ExceptionalT e IO ()
setPortRename client procPtr arg =
    liftErrno $ JackFFI.set_port_rename_callback (getClient client) procPtr arg

withPortRename ::
    (JackExc.ThrowsErrno e) =>
    Client ->
    (JackFFI.PortId -> String -> String -> IO ()) ->
    Sync.ExceptionalT e IO a ->
    Sync.ExceptionalT e IO a
withPortRename =
    withCallback
        (\pr ->
            makePortRename $ \ portid n1 n2 _arg ->
                join $
                pr portid
                    <$> peekCString (JackFFI.deconsPortName n1)
                    <*> peekCString (JackFFI.deconsPortName n2))
        setPortRename

foreign import ccall "wrapper"
    makeGraphOrder ::
        JackFFI.GraphOrder arg -> IO (FunPtr (JackFFI.GraphOrder arg))

setGraphOrder ::
    (JackExc.ThrowsErrno e) =>
    Client ->
    FunPtr (JackFFI.GraphOrder arg) ->
    Ptr arg ->
    Sync.ExceptionalT e IO ()
setGraphOrder client procPtr arg =
    liftErrno $ JackFFI.set_graph_order_callback (getClient client) procPtr arg

withGraphOrder ::
    (JackExc.ThrowsErrno e) =>
    Client ->
    Sync.ExceptionalT Errno IO () ->
    Sync.ExceptionalT e IO a ->
    Sync.ExceptionalT e IO a
withGraphOrder =
    withCallback
        (\go -> makeGraphOrder $ \ _arg -> Sync.switchT return (\() -> return eOK) go)
        setGraphOrder

foreign import ccall "wrapper"
    makeXRun :: JackFFI.XRun arg -> IO (FunPtr (JackFFI.XRun arg))

setXRun ::
    (JackExc.ThrowsErrno e) =>
    Client ->
    FunPtr (JackFFI.XRun arg) ->
    Ptr arg ->
    Sync.ExceptionalT e IO ()
setXRun client procPtr arg =
    liftErrno $ JackFFI.set_xrun_callback (getClient client) procPtr arg

withXRun ::
    (JackExc.ThrowsErrno e) =>
    Client ->
    Sync.ExceptionalT Errno IO () ->
    Sync.ExceptionalT e IO a ->
    Sync.ExceptionalT e IO a
withXRun =
    withCallback
        (\go -> makeXRun $ \ _arg -> Sync.switchT return (\() -> return eOK) go)
        setXRun

foreign import ccall "wrapper"
    makeLatency :: JackFFI.Latency arg -> IO (FunPtr (JackFFI.Latency arg))

setLatency ::
    (JackExc.ThrowsErrno e) =>
    Client ->
    FunPtr (JackFFI.Latency arg) ->
    Ptr arg ->
    Sync.ExceptionalT e IO ()
setLatency client procPtr arg =
    liftErrno $ JackFFI.set_latency_callback (getClient client) procPtr arg

withLatency ::
    (JackExc.ThrowsErrno e) =>
    Client ->
    (LatencyCallbackMode -> IO ()) ->
    Sync.ExceptionalT e IO a ->
    Sync.ExceptionalT e IO a
withLatency =
    withCallback
        (\lc ->
            makeLatency $ \ mode _arg ->
                lc mode)
        setLatency

getLatencyRange ::
    Port typ dir -> LatencyCallbackMode -> IO JackFFI.LatencyRange
getLatencyRange (Port ptr) mode =
    with (JackFFI.LatencyRange (JackFFI.NFrames 0) (JackFFI.NFrames 0)) $
    \rptr -> JackFFI.port_get_latency_range ptr mode rptr >> peek rptr

setLatencyRange ::
    Port typ dir -> LatencyCallbackMode -> JackFFI.LatencyRange -> IO ()
setLatencyRange (Port ptr) mode range =
    with range $ JackFFI.port_set_latency_range ptr mode

recomputeTotalLatencies ::
    JackExc.ThrowsErrno e => Client -> Sync.ExceptionalT e IO ()
recomputeTotalLatencies (Client ptr) =
    liftErrno $ JackFFI.recompute_total_latencies ptr

getBufferSize :: Client -> IO Int
getBufferSize (Client ptr) =
    fmap fromIntegral $ JackFFI.get_buffer_size ptr

getSampleRate :: Client -> IO Int
getSampleRate (Client ptr) =
    fmap fromIntegral $ JackFFI.get_sample_rate ptr

{-
Call this function from within a callback
in order to obtain the start time of the current block.
-}
lastFrameTime :: Client -> IO JackFFI.NFrames
lastFrameTime (Client client) =
    JackFFI.last_frame_time client

-- | Create a client registration callback 'FunPtr'.
foreign import ccall "wrapper"
    makeClientRegistration ::
        JackFFI.ClientRegistration arg ->
        IO (FunPtr (JackFFI.ClientRegistration arg))

-- | Set the client registration callback.
setClientRegistration ::
    (JackExc.ThrowsErrno e) =>
    Client ->
    FunPtr (JackFFI.ClientRegistration arg) ->
    Ptr arg ->
    Sync.ExceptionalT e IO ()
setClientRegistration client procPtr arg =
    liftErrno $ JackFFI.set_client_registration_callback (getClient client) procPtr arg

withClientRegistration ::
    (JackExc.ThrowsErrno e) =>
    Client ->
    (String -> Bool -> IO ()) ->
    Sync.ExceptionalT e IO a ->
    Sync.ExceptionalT e IO a
withClientRegistration =
    withCallback
        (\proc -> makeClientRegistration $
            \ namePtr registered _arg -> do
                name <- peekCString namePtr
                proc name (registered/=0))
        setClientRegistration



-- | Create a port registration callback 'FunPtr'.
foreign import ccall "wrapper"
    makePortRegistration ::
        JackFFI.PortRegistration arg ->
        IO (FunPtr (JackFFI.PortRegistration arg))

-- | Set the port registration callback.
setPortRegistration ::
    (JackExc.ThrowsErrno e) =>
    Client ->
    FunPtr (JackFFI.PortRegistration arg) ->
    Ptr arg ->
    Sync.ExceptionalT e IO ()
setPortRegistration client procPtr arg =
    liftErrno $ JackFFI.set_port_registration_callback (getClient client) procPtr arg

withPortRegistration ::
    (JackExc.ThrowsErrno e) =>
    Client ->
    (JackFFI.PortId -> Bool -> IO ()) ->
    Sync.ExceptionalT e IO a ->
    Sync.ExceptionalT e IO a
withPortRegistration =
    withCallback
        (\proc -> makePortRegistration $
            \ portA registered _arg ->
                proc portA (registered/=0))
        setPortRegistration


-- | Create a port connect callback 'FunPtr'.
foreign import ccall "wrapper"
    makePortConnect ::
        JackFFI.PortConnect arg -> IO (FunPtr (JackFFI.PortConnect arg))

-- | Set the port connect callback.
setPortConnect ::
    (JackExc.ThrowsErrno e) =>
    Client ->
    FunPtr (JackFFI.PortConnect arg) ->
    Ptr arg ->
    Sync.ExceptionalT e IO ()
setPortConnect client procPtr arg =
    liftErrno $ JackFFI.set_port_connect_callback (getClient client) procPtr arg

withPortConnect ::
    (JackExc.ThrowsErrno e) =>
    Client ->
    (JackFFI.PortId -> JackFFI.PortId -> Bool -> IO ()) ->
    Sync.ExceptionalT e IO a ->
    Sync.ExceptionalT e IO a
withPortConnect =
    withCallback
        (\proc -> makePortConnect $
            \ portA portB connected _arg ->
                proc portA portB (connected/=0))
        setPortConnect


portById ::
    Client -> JackFFI.PortId ->
    IO (Port JackFFI.UnknownType UnknownDirection)
portById client portId =
    fmap Port $
    JackFFI.port_by_id (getClient client) portId

portByName ::
    Client -> String ->
    IO (Port JackFFI.UnknownType UnknownDirection)
portByName client name =
    fmap Port $
    CString.withCString name $ JackFFI.port_by_name (getClient client) . JackFFI.PortName

-- | Return the full port name, including the @client_name:@ prefix.
portName :: Port typ dir -> IO String
portName (Port port) =
    peekCString . JackFFI.deconsPortName =<< JackFFI.port_name port

-- | Return the short port name, not including the @client_name:@ prefix.
portShortName :: Port typ dir -> IO String
portShortName (Port port) = JackFFI.port_short_name port >>= peekCString

portType :: Port JackFFI.UnknownType dir -> IO String
portType (Port port) =
    peekCString . JackFFI.deconsPortType =<< JackFFI.port_type port

castPort :: Port typ0 dir0 -> Port typ1 dir1
castPort (Port port) = Port $ castPtr port

{- | Return the port aliases, including the @client_name:@ prefixes.

This is especially useful for external midi devices,
as the alias names are usually more descriptive than @system:midi_capture_1@.
-}
portAliases :: Port typ dir -> IO [String]
portAliases (Port port) = do
    sz <- fmap fromIntegral JackFFI.port_name_size
    Array.allocaArray sz $ \s1 ->
      Array.allocaArray sz $ \s2 -> do
        let ss = [s1, s2]
        Array.withArray ss $ \ptr -> do
          cnt <- JackFFI.port_get_aliases port ptr
          if cnt <= 2
            then mapM peekCString $ take (fromIntegral cnt) ss
            else error $ "port_get_aliases returned " ++ show cnt ++ " aliases"

-- | Return all the port names a given port is connected to.
--
-- This function must not be called from a JACK event callback.
portGetAllConnections ::
    Client -> Port typ dir -> IO [String]
portGetAllConnections client (Port port) =
    JackFFI.port_get_all_connections (getClient client) port
        >>= peekPortNameArray

narrowPort ::
    (PortType typ, Direction dir, JackExc.ThrowsPortMismatch e) =>
    Port JackFFI.UnknownType UnknownDirection ->
    Sync.ExceptionalT e IO (Port typ dir)
narrowPort port =
    narrowPortType =<< narrowPortDirection port

narrowPortType ::
    (PortType typ, JackExc.ThrowsPortMismatch e) =>
    Port JackFFI.UnknownType dir ->
    Sync.ExceptionalT e IO (Port typ dir)
narrowPortType port = do
    typ <- Trans.lift $ portType port
    liftExc $ narrowPortTypeMaybe portTypeString typ port

narrowPortDirection ::
    (Direction dir, JackExc.ThrowsPortMismatch e) =>
    Port typ UnknownDirection ->
    Sync.ExceptionalT e IO (Port typ dir)
narrowPortDirection (Port port) = do
    flags <- Trans.lift $ JackFFI.port_flags port
    liftExc $ narrowPortDirectionMaybe dirFlags flags (Port port)

switchUnknownTypePort ::
    (JackExc.ThrowsPortMismatch e) =>
    Port JackFFI.UnknownType dir ->
    (Port C.CFloat dir -> Sync.ExceptionalT e IO a) ->
    (Port EventBuffer dir -> Sync.ExceptionalT e IO a) ->
    Sync.ExceptionalT e IO a
switchUnknownTypePort port audio midi = do
    typ <- Trans.lift $ portType port
    join $ liftExc $
        altExc
            (audio <$> narrowPortTypeMaybe portTypeString typ port)
            (midi  <$> narrowPortTypeMaybe portTypeString typ port)

switchUnknownDirectionPort ::
    (JackExc.ThrowsPortMismatch e) =>
    Port typ UnknownDirection ->
    (Port typ Input -> Sync.ExceptionalT e IO a) ->
    (Port typ Output -> Sync.ExceptionalT e IO a) ->
    Sync.ExceptionalT e IO a
switchUnknownDirectionPort (Port port) input output = do
    flags <- Trans.lift $ JackFFI.port_flags port
    join $ liftExc $
        altExc
            (input  <$> narrowPortDirectionMaybe dirFlags flags (Port port))
            (output <$> narrowPortDirectionMaybe dirFlags flags (Port port))

narrowPortTypeMaybe ::
    (PortType typ, JackExc.ThrowsPortMismatch e) =>
    PortTypeString typ -> String -> Port JackFFI.UnknownType dir ->
    Sync.Exceptional e (Port typ dir)
narrowPortTypeMaybe (Const portTyp) typ port =
    castPort port <$
    Sync.assert (JackExc.portMismatch JackExc.TypeMismatch) (portTyp == typ)

narrowPortDirectionMaybe ::
    (Direction dir, JackExc.ThrowsPortMismatch e) =>
    DirFlagSet dir -> JackFFI.PortFlagSet -> Port typ UnknownDirection ->
    Sync.Exceptional e (Port typ dir)
narrowPortDirectionMaybe (Const dir) flags (Port port) =
    Port port <$
    Sync.assert
        (JackExc.portMismatch JackExc.DirectionMismatch)
        (not $ ES.disjoint flags dir)

liftExc :: (Monad m) => Sync.Exceptional e a -> Sync.ExceptionalT e m a
liftExc = Sync.ExceptionalT . return

altExc :: Sync.Exceptional e a -> Sync.Exceptional e a -> Sync.Exceptional e a
altExc x y = Sync.switch (const y) Sync.Success x
