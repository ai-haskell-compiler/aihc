module Sound.JACK.Private where

import qualified Sound.JACK.Exception as JackExc
import qualified Sound.JACK.FFI as JackFFI
import Sound.JACK.FFI.MIDI (EventBuffer, )

import qualified Control.Monad.Exception.Synchronous as Sync
import qualified Control.Exception as Exc
import Control.Applicative (Const(Const))

import qualified Foreign.Marshal.Alloc as Alloc
import qualified Foreign.C.String as CString
import Foreign.Storable (Storable, )
import Foreign.Ptr (Ptr, )
import qualified Foreign.C.Types as C
import Foreign.C.Error (Errno, eOK, )


-- | Handles of Jack clients
newtype Client = Client (Ptr JackFFI.Client)


-- | Jack Port Type
class PortType typ where
    switchPortType :: f C.CFloat -> f EventBuffer -> f typ

instance PortType C.CFloat    where switchPortType f _ = f
instance PortType EventBuffer where switchPortType _ f = f

type PortTypeString typ = Const String typ

portTypeString :: PortType typ => PortTypeString typ
portTypeString =
    switchPortType (Const "32 bit float mono audio") (Const "8 bit raw midi")


newtype Port typ dir = Port (Ptr (JackFFI.Port typ))


withCString ::
    String ->
    (CString.CString -> Sync.ExceptionalT e IO b) ->
    Sync.ExceptionalT e IO b
withCString str f =
    Sync.ExceptionalT $
    CString.withCString str (Sync.runExceptionalT . f)

alloca ::
    (Storable a) =>
    (Ptr a -> Sync.ExceptionalT e IO b) ->
    Sync.ExceptionalT e IO b
alloca f =
    Sync.ExceptionalT $
    Alloc.alloca (Sync.runExceptionalT . f)

{-
Mixing of explicit exceptions
with "unchecked" exceptions embedded in IO monad is really not nice.
We should switch to an exception-free IO monad
like explicit-exception:SIO.
-}
bracket ::
    Sync.ExceptionalT e IO handle ->
    (handle -> Sync.ExceptionalT e IO ()) ->
    (handle -> Sync.ExceptionalT e IO a) ->
    Sync.ExceptionalT e IO a
bracket open close act =
    Sync.ExceptionalT $
    Exc.bracket
        (Sync.runExceptionalT open)
        {-
        Exception of 'close' cannot be maintained,
        since we can only propagate an exception
        or generate a new one.
        Generally exceptions in closing functions are a bad idea.
        -}
        (\r ->
            case r of
                Sync.Success a -> Sync.runExceptionalT $ close a
                Sync.Exception e -> return $ Sync.Exception e)
        (\r ->
            case r of
                Sync.Success a -> Sync.runExceptionalT $ act a
                Sync.Exception e -> return $ Sync.Exception e)

bracket_ ::
    Sync.ExceptionalT e IO () ->
    Sync.ExceptionalT e IO () ->
    Sync.ExceptionalT e IO a ->
    Sync.ExceptionalT e IO a
bracket_ open close act =
    bracket open (\() -> close) (\() -> act)


liftErrno ::
    (JackExc.ThrowsErrno e) =>
    IO Errno -> Sync.ExceptionalT e IO ()
liftErrno =
    Sync.ExceptionalT .
    fmap (\err ->
        Sync.assert (JackExc.errno err) $ err == eOK)
