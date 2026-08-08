{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module System.Exit
  ( ExitCode (..),
    exitWith,
    exitFailure,
    exitSuccess,
    die,
  )
where

import Control.Exception (Exception (..), throwIO)
import GHC.IO (IO (..))
import GHC.IO.Console (writeOutputByte)
import GHC.IO.Exception (ioError, userError)
import GHC.Int (Int (..))
import GHC.Internal.Char (Char (C#))
import GHC.Prim (MutableByteArray#, RealWorld, and#, int2Word#, mutableByteArrayContents#, newPinnedByteArray#, ord#, word2Int#, (+#), (==#))
import GHC.Ptr (Ptr (..))
import GHC.Read ()
import System.IO (hPutBuf, stderr)
import Prelude

data ExitCode
  = ExitSuccess
  | ExitFailure Int

instance Eq ExitCode where
  ExitSuccess == ExitSuccess = True
  ExitFailure left == ExitFailure right = left == right
  _ == _ = False
  left /= right = not (left == right)

instance Ord ExitCode where
  compare ExitSuccess ExitSuccess = EQ
  compare ExitSuccess (ExitFailure _) = LT
  compare (ExitFailure _) ExitSuccess = GT
  compare (ExitFailure left) (ExitFailure right) = compare left right
  left < right = compare left right == LT
  left <= right = compare left right /= GT
  left > right = compare left right == GT
  left >= right = compare left right /= LT
  max left right =
    case compare left right of
      GT -> left
      _ -> right
  min left right =
    case compare left right of
      GT -> right
      _ -> left

instance Show ExitCode where
  showsPrec _ ExitSuccess = showString "ExitSuccess"
  showsPrec precedence (ExitFailure status) =
    showParen
      (precedence > 10)
      (showString "ExitFailure " . showsPrec 11 status)

instance Read ExitCode where
  readsPrec precedence input =
    readExitSuccess input
      ++ readParen (precedence > 10) readExitFailure input

readExitSuccess :: ReadS ExitCode
readExitSuccess input =
  case lex input of
    (token, rest) : _ ->
      case token == "ExitSuccess" of
        True -> [(ExitSuccess, rest)]
        False -> []
    _ -> []

readExitFailure :: ReadS ExitCode
readExitFailure input =
  case lex input of
    (token, rest) : _ ->
      case token == "ExitFailure" of
        True -> readExitFailureStatus (reads rest)
        False -> []
    _ -> []

readExitFailureStatus :: [(Int, String)] -> [(ExitCode, String)]
readExitFailureStatus [] = []
readExitFailureStatus ((status, rest) : results) =
  (ExitFailure status, rest) : readExitFailureStatus results

instance Exception ExitCode where
  displayException = show

exitWith :: ExitCode -> IO a
exitWith ExitSuccess = throwIO ExitSuccess
exitWith code@(ExitFailure status) =
  case status == 0 of
    True -> ioError (userError "exitWith: invalid argument (ExitFailure 0)")
    False -> throwIO code

exitFailure :: IO a
exitFailure = exitWith (ExitFailure 1)

exitSuccess :: IO a
exitSuccess = exitWith ExitSuccess

die :: String -> IO a
die message = do
  writeDieMessage message
  exitFailure

writeDieMessage :: String -> IO ()
writeDieMessage characters = do
  buffer <- newDieBuffer 4096#
  case buffer of
    DieBuffer rawBuffer -> writeDieChunks rawBuffer 0# (characters ++ "\n")

data DieBuffer = DieBuffer (MutableByteArray# RealWorld)

newDieBuffer :: Int# -> IO DieBuffer
newDieBuffer size =
  IO
    ( \state ->
        case newPinnedByteArray# size state of
          (# allocatedState, buffer #) ->
            (# allocatedState, DieBuffer buffer #)
    )

writeDieChunks :: MutableByteArray# RealWorld -> Int# -> String -> IO ()
writeDieChunks buffer count characters =
  case characters of
    [] -> writeDieBuffer buffer count
    character : remaining ->
      case (==#) count 4096# of
        1# -> do
          writeDieBuffer buffer count
          writeDieChunks buffer 0# characters
        _ -> do
          writeDieByte buffer count character
          writeDieChunks buffer ((+#) count 1#) remaining

writeDieBuffer :: MutableByteArray# RealWorld -> Int# -> IO ()
writeDieBuffer buffer count =
  case (==#) count 0# of
    1# -> return ()
    _ -> hPutBuf stderr (Ptr (mutableByteArrayContents# buffer) :: Ptr ()) (I# count)

writeDieByte :: MutableByteArray# RealWorld -> Int# -> Char -> IO ()
writeDieByte buffer offset (C# character) =
  writeOutputByte buffer offset (word2Int# (and# (int2Word# (ord# character)) (int2Word# 255#)))
