{-# LANGUAGE ForeignFunctionInterface #-}
-- |
-- Maintainer:  xanxys@gmail.com
-- Stability:   experimental
-- Portability: non-portable
--
-- Binding to PulseAudio Simple API (original documentation:
--  <http://freedesktop.org/software/pulseaudio/doxygen/simple_8h.html>)
--
-- playback example(output 440Hz sine wave for 10 seconds):
-- 
-- @
--  main=do
--      s<-simpleNew Nothing \"example\" Play Nothing \"this is an example application\"
--          (SampleSpec (F32 LittleEndian) 44100 1) Nothing Nothing
--      simpleWrite s ([sin $ 2*pi*440*(t/44100)|t<-[1..44100*10]] :: [Float])
--      simpleDrain s
--      simpleFree s
-- @
--
-- recording example(record for 10 seconds):
--
-- @
--  main=do
--      s<-simpleNew Nothing \"example\" Record Nothing \"this is an example application\"
--          (SampleSpec (F32 LittleEndian) 44100 1) Nothing Nothing
--      xs<-simpleRead s $ 44100*10 :: IO [Float]
--      simpleFree s
-- @
--
-- Note that recording starts when 'simpleNew' is called.
-- 
module Sound.Pulse.Simple
    (simpleNew
    ,simpleFree
    ,simpleGetLatency
    ,simpleRead
    ,simpleReadRaw
    ,simpleWrite
    ,simpleWriteRaw
    ,simpleDrain
    ,simpleFlush
    ,Simple
    ,SampleSpec(..)
    ,SampleFormat(..)
    ,Compression(..)
    ,Endian(..)
    ,Direction(..)
    ,ChannelPosition(..)
    ,ChannelPan(..)
    ,BufferAttr(..))
    where
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable


#include <pulse/simple.h>
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)




foreign import ccall "pa_simple_new" pasNew
    :: CString -> CString -> CInt -> CString -> CString -> Ptr SampleSpec -> Ptr ChannelMap
    -> Ptr BufferAttr -> Ptr CInt -> IO (Ptr PASimple)
foreign import ccall "pa_simple_free" pasFree :: Ptr PASimple -> IO ()
foreign import ccall "pa_simple_write" pasWrite :: Ptr PASimple -> Ptr CUChar -> CInt -> Ptr CInt -> IO CInt
foreign import ccall "pa_simple_read" pasRead :: Ptr PASimple -> Ptr CUChar -> CInt -> Ptr CInt -> IO CInt
foreign import ccall "pa_simple_drain" pasDrain :: Ptr PASimple -> Ptr CInt -> IO CInt
foreign import ccall "pa_simple_get_latency" pasGetLatency :: Ptr PASimple -> Ptr CInt -> IO CUInt -- 64 bit dep.
foreign import ccall "pa_simple_flush" pasFlush :: Ptr PASimple -> IO CInt


newtype Simple=Simple (Ptr PASimple)
data Direction=Play|Record


data SampleSpec=SampleSpec SampleFormat Int Int -- ^ format, sampling rate, #channels

data SampleFormat
    =U8 Compression -- ^ 8 bit unsigned (optionally compressed using 'Compression')
    |S16 Endian     -- ^ 16 bit signed
    |S24 Endian     -- ^ 24 bit signed
    |S2432 Endian   -- ^ 24 bit signed padded to 32 bit
    |S32 Endian     -- ^ 32 bit signed
    |F32 Endian     -- ^ 32 bit float in [-1,1]
data Compression=Raw|ALaw|MuLaw
data Endian=BigEndian|LittleEndian

newtype ChannelMap=ChannelMap [ChannelPosition]
data ChannelPosition
    =ChannelMono
    |ChannelNormal   ChannelPan
    |ChannelFront    ChannelPan
    |ChannelRear     ChannelPan
    |ChannelTopRear  ChannelPan
    |ChannelTopFront ChannelPan
    |ChannelLFE -- ^ low frequency effects
    |ChannelSubwoofer
    |ChannelFrontCenterLeft -- ^ equivalent to PA_CHANNEL_POSITION_FRONT_LEFT_OF_CENTER
    |ChannelFrontCenterRight
    |ChannelSideLeft
    |ChannelSideRight
    |ChannelTopCenter
    |ChannelAux Int -- 0-31


data ChannelPan=PanLeft|PanRight|PanCenter



data BufferAttr=BufferAttr (Maybe Int) (Maybe Int) (Maybe Int) (Maybe Int) (Maybe Int)
-- ^ max length, target length, prebuffer, minimum request, fragment size


-- | hidden struct
data PASimple=PASimple







instance Enum Direction where
    toEnum=undefined
    fromEnum Play=1
    fromEnum Record=2

instance Storable SampleSpec where
    alignment _= #{alignment pa_sample_spec}
    sizeOf _= #{size pa_sample_spec}
    poke ptr (SampleSpec fmt rate nch)=do
        #{poke pa_sample_spec,format}   ptr $ fromEnum fmt
        #{poke pa_sample_spec,rate}     ptr rate
        #{poke pa_sample_spec,channels} ptr nch

instance Enum SampleFormat where
    toEnum=undefined
    fromEnum (U8 Raw)=0
    fromEnum (U8 ALaw)=1
    fromEnum (U8 MuLaw)=2
    fromEnum x=case x of
        S16 e -> 3+f e
        F32 e -> 5+f e
        S32 e -> 7+f e
        S24 e -> 9+f e
        S2432 e -> 11+f e
        where f LittleEndian=0; f BigEndian=1

instance Storable ChannelMap where
    alignment _= #{alignment pa_channel_map}
    sizeOf _= #{alignment pa_channel_map}
    poke ptr (ChannelMap ps)=withArray (map fromEnum ps) $ \_ps -> do
        #{poke pa_channel_map,channels} ptr $ length ps
        #{poke pa_channel_map,map} ptr _ps

instance Enum ChannelPosition where
    toEnum=undefined
    fromEnum x=case x of
        ChannelMono -> 0
        ChannelNormal p -> 1+f p
        ChannelFront p -> 1+f p
        ChannelRear p -> 4+f p
        ChannelLFE -> 7
        ChannelSubwoofer -> 7
        ChannelFrontCenterLeft -> 8
        ChannelFrontCenterRight -> 9
        ChannelSideLeft -> 10
        ChannelSideRight -> 11
        ChannelAux n -> 12+n
        ChannelTopCenter -> 44
        ChannelTopFront p -> 45+f p
        ChannelTopRear p -> 48+f p
        where f PanLeft=0; f PanRight=1; f PanCenter=2

instance Storable BufferAttr where
    alignment _= #{alignment pa_buffer_attr}
    sizeOf _= #{alignment pa_buffer_attr}
    poke ptr (BufferAttr ml tl pb mr fs)=do
        #{poke pa_buffer_attr,maxlength} ptr $ f ml
        #{poke pa_buffer_attr,tlength}   ptr $ f tl
        #{poke pa_buffer_attr,prebuf}    ptr $ f pb
        #{poke pa_buffer_attr,minreq}    ptr $ f mr
        #{poke pa_buffer_attr,fragsize}  ptr $ f fs
        where f=maybe 0xffffffff id


 


-- | Establish connection to pulseaudio server. You usually don't need to specify optional fields.
simpleNew
    :: Maybe String -- ^ server name
    -> String -- ^ client name
    -> Direction -- ^ Play or Record
    -> Maybe String -- ^ name of sink or source
    -> String -- ^ description of client
    -> SampleSpec
    -> Maybe [ChannelPosition] -- ^ label channels
    -> Maybe BufferAttr -- ^ buffer size, etc
    -> IO Simple
simpleNew server client dir dev desc spec chmap attr=liftM Simple $
    withMaybeCString server $ \_server->
    withCString client $ \_client ->
    withMaybeCString dev $ \_dev ->
    withCString desc $ \_desc ->
    withStorable spec $ \_spec -> 
    withMaybeStorable (liftM ChannelMap chmap) $ \_chmap ->
    withMaybeStorable attr $ \_attr ->
        pasNew _server _client (fromIntegral $ fromEnum dir) _dev _desc _spec _chmap _attr nullPtr


-- | Read raw data from buffer.
simpleReadRaw :: Simple -> Int -> IO BS.ByteString
simpleReadRaw (Simple x) size=
    BS.create size $ \ptr->pasRead x (castPtr ptr) (fromIntegral size) nullPtr >> return ()

-- | Write raw data to buffer.
simpleWriteRaw :: Simple -> BS.ByteString -> IO ()
simpleWriteRaw (Simple x) (BS.PS ptr ofs size)=do
    withForeignPtr ptr $ \p->pasWrite x (castPtr p) (fromIntegral size) nullPtr
    return ()

-- | Read from buffer. (non-blocking if specified # of samples already exist in the internal buffer)
simpleRead
    :: Storable a
    => Simple
    -> Int -- ^ number of samples to read
    -> IO [a]
simpleRead s n=simpleReadHack undefined s n

simpleReadHack :: Storable a => a -> Simple -> Int -> IO [a]
simpleReadHack dummy (Simple x) n=do
    let size=fromIntegral $ n*sizeOf dummy
    rs<-allocaArray n $ \ptr->pasRead x (castPtr ptr) size nullPtr >> peekArray n ptr
    return rs
    
-- | Write to buffer. (blocks until buffer is /almost/ consumed)
simpleWrite :: Storable a => Simple -> [a] -> IO ()
simpleWrite (Simple x) xs=do
    let n=length xs
        size=fromIntegral $ n*sizeOf (head xs)
    allocaArray n $ \ptr->pokeArray ptr xs >> pasWrite x (castPtr ptr) size nullPtr
    return ()

-- | Flush playback buffer.
simpleFlush :: Simple -> IO ()
simpleFlush (Simple x)=pasFlush x >> return ()

-- | Block until playback buffer is completely consumed.
simpleDrain :: Simple -> IO ()
simpleDrain (Simple x)=pasDrain x nullPtr >> return ()

-- | Close the connection.
simpleFree :: Simple -> IO ()
simpleFree (Simple x)=pasFree x

-- | Get current latency in microseconds.
simpleGetLatency :: Simple -> IO Integer
simpleGetLatency (Simple x)=liftM fromIntegral $ pasGetLatency x nullPtr


withMaybeStorable :: Storable a => Maybe a -> (Ptr a -> IO b) -> IO b
withMaybeStorable Nothing f=f nullPtr
withMaybeStorable (Just x) f=withStorable x f

withStorable :: Storable a => a -> (Ptr a -> IO b) -> IO b
withStorable x f=alloca (\ptr->poke ptr x>> f ptr)

withMaybeCString :: Maybe String -> (CString -> IO a) -> IO a
withMaybeCString Nothing f=f nullPtr
withMaybeCString (Just s) f=withCString s f

