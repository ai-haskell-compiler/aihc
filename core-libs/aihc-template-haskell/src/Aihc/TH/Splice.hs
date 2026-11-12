{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Run a splice in the guest of an engine image.
--
-- The compiler runs a splice in a WebAssembly instance. This module is the
-- part of that work that is in the instance. The compiler makes a closure
-- that applies 'runSplice' to the body of the splice, and the guest runs
-- the closure. The result goes to the compiler as one message.
--
-- The message is a tree. Each value of an algebraic data type is a node
-- with the name of its constructor and one child for each field. Thus, the
-- message has the same constructors as the Template Haskell syntax tree,
-- and this module has no knowledge of them.
--
-- The encoding of a tree is:
--
-- > tree     = 0 text count tree*   a constructor and its fields
-- >          | 1 text               an integer, in decimal
-- >          | 2 count              a character, as its code point
-- >          | 3 text               a string
-- >          | 4 text text          a ratio: numerator, denominator
-- > text     = count byte*          UTF-8, with the number of bytes
-- > count    = an unsigned LEB128 number
--
-- A splice runs in the monad 'Splice', which is the 'Quasi' monad of this
-- module. An operation that needs the compiler sends a 'Request', which is
-- a tree with the same encoding, and waits for the reply. A reply that has
-- bytes is also a tree with the same encoding. The instance has no other
-- access to the compiler or to the build machine.
module Aihc.TH.Splice
  ( runSplice,
    runExpressionSplice,
    runTypeSplice,
    runDeclarationsSplice,
  )
where

import Control.Exception (SomeException, try)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Bifunctor (first)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Char (chr, ord)
import Data.Data (ConstrRep (..), Data, DataRep (..), cast, constrRep, dataTypeOf, dataTypeRep, fromConstrM, gmapQ, mkCharConstr, mkIntegralConstr, mkRealConstr, readConstr, showConstr, toConstr)
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Proxy (Proxy (..))
import Data.Ratio (denominator, numerator, (%))
import Data.Typeable (TypeRep, Typeable, typeRep)
import Data.Word (Word8)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Array (peekArray, withArrayLen)
import Foreign.Ptr (Ptr)
import Language.Haskell.TH.Syntax (Dec, Exp, Info, Name, Q, Quasi (..), Type, runQ)
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (readMaybe)

-- | Send a message to the host.
foreign import ccall unsafe "aihc_engine_send" engineSend :: Ptr Word8 -> Int -> IO ()

-- | Send a request to the host and wait for the reply. The result is the
-- number of bytes of the reply.
foreign import ccall unsafe "aihc_engine_request" engineRequest :: Ptr Word8 -> Int -> IO Int

-- | Copy the reply of the last request to memory of the guest.
foreign import ccall unsafe "aihc_engine_reply" engineReply :: Ptr Word8 -> IO ()

-- | Run a splice and send its result to the host.
runSplice :: (Data a) => Q a -> IO ()
runSplice splice = do
  result <- spliceAction (runQ splice)
  withArrayLen (encodeTree result []) (flip engineSend)

-- | 'runSplice' for the body of an expression splice. The type makes sure
-- that the body gives an expression.
runExpressionSplice :: Q Exp -> IO ()
runExpressionSplice = runSplice

-- | 'runSplice' for the body of a type splice. The type makes sure that
-- the body gives a type.
runTypeSplice :: Q Type -> IO ()
runTypeSplice = runSplice

-- | 'runSplice' for the body of a declaration splice. The type makes sure
-- that the body gives a list of declarations.
runDeclarationsSplice :: Q [Dec] -> IO ()
runDeclarationsSplice = runSplice

-- | The monad of a splice in the guest.
newtype Splice a = Splice {spliceAction :: IO a}

instance Functor Splice where
  fmap function (Splice action) = Splice (fmap function action)

instance Applicative Splice where
  pure value = Splice (pure value)
  Splice function <*> Splice action = Splice (function <*> action)

instance Monad Splice where
  Splice action >>= next = Splice (action >>= spliceAction . next)

instance MonadFail Splice where
  fail message = Splice (fail message)

instance MonadIO Splice where
  liftIO = Splice

-- | What a splice asks the compiler to do.
data Request
  = -- | Report an error ('True') or a warning ('False').
    Report Bool String
  | -- | Give the facts of a name. The reply is an @Either String Info@
    -- value: an error message or the facts.
    Reify Name
  deriving (Data)

-- | Send a request and give the bytes of the reply.
sendRequest :: Request -> IO [Word8]
sendRequest request =
  withArrayLen (encodeTree request []) $ \size bytes -> do
    replySize <- engineRequest bytes size
    if replySize == 0
      then pure []
      else allocaBytes replySize $ \reply -> do
        engineReply reply
        peekArray replySize reply

-- | The reports of each 'qRecover' that is in progress, with the last one
-- first. The reports of the code in a 'qRecover' only go to the compiler
-- when that code completes with no error.
recoverReports :: IORef [[Request]]
recoverReports = unsafePerformIO (newIORef [])
{-# NOINLINE recoverReports #-}

-- | The state of 'qGetQ' and 'qPutQ': one value for each type. The instance
-- runs the splices of one module, so the state is the state of that module.
userState :: IORef [(TypeRep, Dynamic)]
userState = unsafePerformIO (newIORef [])
{-# NOINLINE userState #-}

-- | Give a report to the 'qRecover' that is in progress, or to the compiler.
report :: Request -> IO ()
report request = do
  pending <- readIORef recoverReports
  case pending of
    [] -> void (sendRequest request)
    current : outer -> writeIORef recoverReports ((request : current) : outer)

-- | Stop a splice at an operation that this module does not have.
unsupported :: String -> Splice a
unsupported operation =
  Splice $ do
    report (Report True ("The compiler does not support `" ++ operation ++ "' in a splice."))
    fail "Template Haskell failure"

instance Quasi Splice where
  -- The counter of the library gives each name a number that is unique in
  -- the instance.
  qNewName = Splice . qNewName
  qReport isError message = Splice (report (Report isError message))
  qRecover (Splice handler) (Splice body) =
    Splice $ do
      modifyIORef' recoverReports ([] :)
      result <- try body
      pending <- readIORef recoverReports
      let (reports, outer) =
            case pending of
              current : rest -> (reverse current, rest)
              [] -> ([], [])
      writeIORef recoverReports outer
      case result of
        Right value | not (any isErrorReport reports) -> value <$ mapM_ report reports
        Right _ -> handler
        Left (_ :: SomeException) -> handler
    where
      isErrorReport (Report isError _) = isError
  qGetQ = Splice (lookupState Proxy)
    where
      lookupState :: forall a. (Typeable a) => Proxy a -> IO (Maybe a)
      lookupState proxy = do
        values <- readIORef userState
        pure (lookup (typeRep proxy) values >>= fromDynamic)
  qPutQ value =
    Splice (modifyIORef' userState (\values -> (key, toDyn value) : filter ((/= key) . fst) values))
    where
      key = typeRep (Just value)
  qLookupName _ _ = unsupported "lookupName"
  qReify name =
    Splice $ do
      reply <- sendRequest (Reify name)
      case decodeValue reply of
        Just (Right info) -> pure (info :: Info)
        Just (Left message) -> do
          report (Report True message)
          fail "Template Haskell failure"
        Nothing -> do
          report (Report True "The reply of the compiler to `reify' is not valid.")
          fail "Template Haskell failure"
  qReifyFixity _ = unsupported "reifyFixity"
  qReifyType _ = unsupported "reifyType"
  qReifyInstances _ _ = unsupported "reifyInstances"
  qReifyRoles _ = unsupported "reifyRoles"
  qReifyAnnotations _ = unsupported "reifyAnnotations"
  qReifyModule _ = unsupported "reifyModule"
  qReifyConStrictness _ = unsupported "reifyConStrictness"
  qLocation = unsupported "location"
  qGetPackageRoot = unsupported "getPackageRoot"
  qAddDependentFile _ = unsupported "addDependentFile"
  qAddTempFile _ = unsupported "addTempFile"
  qAddTopDecls _ = unsupported "addTopDecls"
  qAddForeignFilePath _ _ = unsupported "addForeignFilePath"
  qAddModFinalizer _ = unsupported "addModFinalizer"
  qAddCorePlugin _ = unsupported "addCorePlugin"
  qIsExtEnabled _ = unsupported "isExtEnabled"
  qExtsEnabled = unsupported "extsEnabled"
  qPutDoc _ _ = unsupported "putDoc"
  qGetDoc _ = unsupported "getDoc"

-- | The value of the bytes of a tree. The tree must have the constructors
-- of the type of the value.
decodeValue :: (Data a) => [Word8] -> Maybe a
decodeValue bytes =
  case runDecoder decoder bytes of
    Just (value, []) -> Just value
    _ -> Nothing

-- | A decoder gives a value and the bytes after it.
newtype Decoder a = Decoder {runDecoder :: [Word8] -> Maybe (a, [Word8])}

instance Functor Decoder where
  fmap function (Decoder decode) = Decoder (fmap (first function) . decode)

instance Applicative Decoder where
  pure value = Decoder (\bytes -> Just (value, bytes))
  Decoder function <*> Decoder argument =
    Decoder $ \bytes -> do
      (function', rest) <- function bytes
      (argument', rest') <- argument rest
      Just (function' argument', rest')

instance Monad Decoder where
  Decoder decode >>= next =
    Decoder $ \bytes -> do
      (value, rest) <- decode bytes
      runDecoder (next value) rest

failDecoder :: Decoder a
failDecoder = Decoder (const Nothing)

-- | Decode a value of a type with a 'Data' instance. A string is one text,
-- as 'encodeTree' gives it.
decoder :: forall a. (Data a) => Decoder a
decoder =
  case cast "" :: Maybe a of
    Just _ -> do
      tag <- byteDecoder
      if tag == 3 then textDecoder >>= maybe failDecoder pure . cast else failDecoder
    Nothing -> do
      tag <- byteDecoder
      case (tag, dataTypeRep dataType) of
        (0, AlgRep _) -> do
          name <- textDecoder
          count <- countDecoder
          constructor <- maybe failDecoder pure (readConstr dataType name)
          value <- fromConstrM decoder constructor
          if length (gmapQ (const ()) value) == count then pure value else failDecoder
        (1, IntRep) -> do
          number <- textDecoder >>= maybe failDecoder pure . readMaybe
          fromConstrM failDecoder (mkIntegralConstr dataType (number :: Integer))
        (2, CharRep) -> do
          code <- countDecoder
          if code > 0x10FFFF then failDecoder else fromConstrM failDecoder (mkCharConstr dataType (chr code))
        (4, FloatRep) -> do
          top <- textDecoder >>= maybe failDecoder pure . readMaybe
          bottom <- textDecoder >>= maybe failDecoder pure . readMaybe
          if bottom == 0 then failDecoder else fromConstrM failDecoder (mkRealConstr dataType ((top :: Integer) % bottom))
        _ -> failDecoder
  where
    dataType = dataTypeOf (undefined :: a)

byteDecoder :: Decoder Word8
byteDecoder =
  Decoder $ \case
    byte : rest -> Just (byte, rest)
    [] -> Nothing

countDecoder :: Decoder Int
countDecoder = go 0 0
  where
    go :: Int -> Int -> Decoder Int
    go shift number = do
      byte <- byteDecoder
      let number' = number .|. (fromIntegral (byte .&. 127) `shiftL` shift)
      if byte >= 128 then go (shift + 7) number' else pure number'

-- | A text: the number of bytes, then the UTF-8 bytes.
textDecoder :: Decoder String
textDecoder = do
  size <- countDecoder
  bytes <- Decoder (\input -> if length (take size input) == size then Just (splitAt size input) else Nothing)
  maybe failDecoder pure (fromUtf8 bytes)

-- | The characters of UTF-8 bytes.
fromUtf8 :: [Word8] -> Maybe String
fromUtf8 bytes =
  case bytes of
    [] -> Just []
    lead : rest
      | lead < 0x80 -> (chr (fromIntegral lead) :) <$> fromUtf8 rest
      | lead >= 0xF0 -> sequence' 3 (fromIntegral lead .&. 0x07) rest
      | lead >= 0xE0 -> sequence' 2 (fromIntegral lead .&. 0x0F) rest
      | lead >= 0xC0 -> sequence' 1 (fromIntegral lead .&. 0x1F) rest
      | otherwise -> Nothing
  where
    sequence' :: Int -> Int -> [Word8] -> Maybe String
    sequence' size initial rest =
      let (continuations, after) = splitAt size rest
       in if length continuations == size && all (\byte -> byte .&. 0xC0 == 0x80) continuations
            then (chr (foldl (\code byte -> (code `shiftL` 6) .|. (fromIntegral byte .&. 0x3F)) initial continuations) :) <$> fromUtf8 after
            else Nothing

-- | The bytes of a value, before the bytes that follow it.
encodeTree :: (Data a) => a -> [Word8] -> [Word8]
encodeTree value =
  case cast value of
    Just string -> (3 :) . encodeText string
    Nothing ->
      case constrRep constructor of
        AlgConstr _ -> (0 :) . encodeText (showConstr constructor) . encodeCount (length fields) . foldr (.) id fields
        IntConstr number -> (1 :) . encodeText (show number)
        CharConstr char -> (2 :) . encodeCount (ord char)
        FloatConstr ratio -> (4 :) . encodeText (show (numerator ratio)) . encodeText (show (denominator ratio))
  where
    constructor = toConstr value
    fields = gmapQ encodeTree value

encodeText :: String -> [Word8] -> [Word8]
encodeText string rest = encodeCount (length bytes) (bytes ++ rest)
  where
    bytes = concatMap utf8 string

encodeCount :: Int -> [Word8] -> [Word8]
encodeCount number rest
  | number < 128 = fromIntegral number : rest
  | otherwise = fromIntegral ((number .&. 127) .|. 128) : encodeCount (number `shiftR` 7) rest

-- | The UTF-8 bytes of a character.
utf8 :: Char -> [Word8]
utf8 char
  | code < 0x80 = [fromIntegral code]
  | code < 0x800 = [lead 0xC0 6, continuation 0]
  | code < 0x10000 = [lead 0xE0 12, continuation 6, continuation 0]
  | otherwise = [lead 0xF0 18, continuation 12, continuation 6, continuation 0]
  where
    code = ord char
    lead prefix shift = fromIntegral (prefix .|. (code `shiftR` shift))
    continuation shift = fromIntegral (0x80 .|. ((code `shiftR` shift) .&. 0x3F))
