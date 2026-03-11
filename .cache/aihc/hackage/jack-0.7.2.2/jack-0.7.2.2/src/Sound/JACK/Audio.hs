module Sound.JACK.Audio (
    Sample, Port, withPort,

    withProcessMono,
    withProcessStereo,

    getBufferPtr,
    getBufferArray,

    mainMono,
    mainStereo,
    ) where

import qualified Sound.JACK.Private as Priv
import qualified Sound.JACK as Jack
import Sound.JACK.Private (Client, )
import Sound.JACK (Direction, Input, Output, )

import qualified Sound.JACK.Exception as JackExc
import qualified Sound.JACK.FFI as JackFFI
import Sound.JACK.FFI (NFrames, nframesIndices, nframesBounds, )

import qualified Control.Monad.Exception.Synchronous as Sync
import qualified Control.Monad.Trans.Class as Trans

import Foreign.ForeignPtr (newForeignPtr_, )
import Foreign.Ptr (Ptr, )
import Foreign.C.Error (Errno, )
import qualified Foreign.C.Types as C

import System.Environment (getProgName)

import Data.Array.Storable (StorableArray, readArray, writeArray, )
import Data.Array.Unsafe (unsafeForeignPtrToStorableArray, )


type Sample = C.CFloat

type Port = Priv.Port Sample


withPort ::
    (Direction dir,
     JackExc.ThrowsPortRegister e,
     JackExc.ThrowsErrno e) =>
       Client -- ^ Jack client
    -> String -- ^ name of the input port
    -> (Port dir -> Sync.ExceptionalT e IO a)
    -> Sync.ExceptionalT e IO a
withPort = Jack.withPort


getBufferPtr ::
    (Direction dir) =>
    Port dir -> NFrames -> IO (Ptr Sample)
getBufferPtr (Priv.Port port) nframes =
    JackFFI.port_get_buffer port nframes

getBufferArray ::
    (Direction dir) =>
    Port dir -> NFrames -> IO (StorableArray NFrames Sample)
getBufferArray port nframes =
    flip unsafeForeignPtrToStorableArray (nframesBounds nframes)
        =<< newForeignPtr_
        =<< getBufferPtr port nframes


mainMono :: (Sample -> IO Sample) -> IO ()
mainMono fun = do
    name <- getProgName
    Jack.handleExceptions $
        Jack.withClientDefault name $ \client ->
        Jack.withPort client "input" $ \input ->
        Jack.withPort client "output" $ \output ->
        withProcessMono client input fun output $
            Jack.withActivation client $ Trans.lift $ do
                putStrLn $ "started " ++ name ++ "..."
                Jack.waitForBreak

withProcessMono ::
    (JackExc.ThrowsErrno e) =>
    Client ->
    Port Input -> (Sample -> IO Sample) ->
    Port Output ->
    Sync.ExceptionalT e IO a ->
    Sync.ExceptionalT e IO a
withProcessMono client input fun output =
    Jack.withProcess client $ wrapMonoFun input fun output

wrapMonoFun ::
    Port Input -> (Sample -> IO Sample) ->
    Port Output ->
    NFrames -> Sync.ExceptionalT Errno IO ()
wrapMonoFun input fun output nframes = Trans.lift $ do
    inArr <- getBufferArray input nframes
    outArr <- getBufferArray output nframes
    mapM_ (applyToArraysMono inArr fun outArr) (nframesIndices nframes)

applyToArraysMono ::
       StorableArray NFrames Sample -> (Sample -> IO Sample)
    -> StorableArray NFrames Sample
    -> NFrames -> IO ()
applyToArraysMono inArr fun outArr i =
    readArray inArr i >>= fun >>= writeArray outArr i

mainStereo :: ((Sample, Sample) -> IO (Sample, Sample)) -> IO ()
mainStereo fun = do
    name <- getProgName
    Jack.handleExceptions $
        Jack.withClientDefault name $ \client ->
        Jack.withPort client "inputLeft" $ \inputLeft ->
        Jack.withPort client "inputRight" $ \inputRight ->
        Jack.withPort client "outputLeft" $ \outputLeft ->
        Jack.withPort client "outputRight" $ \outputRight ->
        withProcessStereo client
                inputLeft inputRight fun
                outputLeft outputRight $
            Jack.withActivation client $ Trans.lift $ do
                putStrLn $ "started " ++ name ++ "..."
                Jack.waitForBreak


withProcessStereo ::
    (JackExc.ThrowsErrno e) =>
    Client ->
    Port Input -> Port Input ->
    ((Sample, Sample) -> IO (Sample, Sample)) ->
    Port Output -> Port Output ->
    Sync.ExceptionalT e IO a ->
    Sync.ExceptionalT e IO a
withProcessStereo client inputLeft inputRight fun outputLeft outputRight =
    Jack.withProcess client $
    wrapStereoFun inputLeft inputRight fun outputLeft outputRight


wrapStereoFun ::
    Port Input -> Port Input ->
    ((Sample, Sample) -> IO (Sample, Sample)) ->
    Port Output -> Port Output ->
    NFrames -> Sync.ExceptionalT Errno IO ()
wrapStereoFun iL iR fun oL oR nframes = Trans.lift $ do
    inLArr <- getBufferArray iL nframes
    inRArr <- getBufferArray iR nframes
    outLArr <- getBufferArray oL nframes
    outRArr <- getBufferArray oR nframes
    mapM_ (applyToArraysStereo inLArr inRArr fun outLArr outRArr) (nframesIndices nframes)

applyToArraysStereo :: StorableArray NFrames Sample
    -> StorableArray NFrames Sample
    -> ((Sample, Sample) -> IO (Sample, Sample))
    -> StorableArray NFrames Sample
    -> StorableArray NFrames Sample
    -> NFrames -> IO ()
applyToArraysStereo iL iR fun oL oR i = do
    l <- readArray iL i
    r <- readArray iR i
    (l', r') <- fun (l, r)
    writeArray oL i l'
    writeArray oR i r'
