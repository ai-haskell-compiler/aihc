
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}

--------------------------------------------------------------------
-- |
-- Module : System.Hclip
-- Copyright : (c) Jens Thomas
-- License : BSD3
--
-- Maintainer: Jens Thomas <jetho@gmx.de>
-- Stability : experimental
-- Portability: non-portable (GADTs, CPP, DeriveDataTypeable)
--
-- A small cross-platform library for reading and modifying the system clipboard.
-- 
--------------------------------------------------------------------

module System.Hclip (
        getClipboard,
        setClipboard,
        modifyClipboard,
        modifyClipboard_,
        clearClipboard,
        ClipboardException(..)
  ) where


import System.Info (os)
import System.Process (runInteractiveCommand, readProcessWithExitCode, waitForProcess)
import System.IO (Handle, hPutStr, hClose)
import Data.Monoid 
import System.IO.Strict (hGetContents) -- see http://hackage.haskell.org/package/strict
import System.Exit (ExitCode(..))
import Data.List (intercalate, genericLength)
import Control.Exception (Exception, throw, throwIO, bracket, bracket_)
import Data.Typeable (Typeable)
import Control.Applicative ((<$>))
import Control.Monad ((>=>), liftM)

-- | for Windows support
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import System.Win32.Mem (globalAlloc, globalLock, globalUnlock, copyMemory, gHND)
import Graphics.Win32.GDI.Clip (openClipboard, closeClipboard, emptyClipboard, getClipboardData,
                                setClipboardData, ClipboardFormat, isClipboardFormatAvailable, cF_TEXT)
import Foreign.C (withCAString, peekCAString)
import Foreign.Ptr (castPtr, nullPtr)
#endif


type StdIn      = Handle
type StdOut     = Handle
type IOAction a = (StdIn, StdOut) -> IO a


-- | Clipboard Commands
data Command a where
    GetClipboard   :: Command (IO String)
    SetClipboard   :: String -> Command (IO ())


-- | Supported Platforms
data Platform = Linux
              | Darwin
              | Windows


-- | Exceptions
data ClipboardException = UnsupportedOS String
                        | NoTextualData
                        | MissingCommands [String]
                        deriving (Typeable)
                    
instance Exception ClipboardException

instance Show ClipboardException where
    show (UnsupportedOS s)     = "Unsupported Operating System: " ++ s
    show NoTextualData          = "Clipboard doesn't contain textual data."
    show (MissingCommands cmds) = "Hclip requires " ++ apps ++ " installed."
        where apps = intercalate " or " cmds


-- | Read clipboard contents.
getClipboard :: IO String
getClipboard = dispatch GetClipboard

-- | Set clipboard contents.
setClipboard :: String -> IO ()
setClipboard = dispatch . SetClipboard

-- | Apply function to clipboard and return its new contents.
modifyClipboard :: (String -> String) -> IO String
modifyClipboard f = do
    modified <- f <$> getClipboard
    setClipboard modified
    return modified

-- | Apply function to clipboard.
modifyClipboard_ :: (String -> String) -> IO ()
modifyClipboard_ = flip liftM getClipboard >=> setClipboard

-- | Delete Clipboard contents.
clearClipboard :: IO ()
clearClipboard = setClipboard ""


-- | Dispatch on the type of the Operating System.
dispatch cmd = execute (resolveOS os) cmd
    where
        resolveOS "linux"   = Linux
        resolveOS "darwin"  = Darwin
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
        resolveOS "mingw32" = Windows
#endif
        resolveOS unknownOS = throw . UnsupportedOS $ unknownOS


-- | Platform-specific execution.
execute :: Platform -> Command a -> a

execute Linux cmd@GetClipboard     = resolveLinuxApp cmd >>= flip withExternalApp readOutHandle
execute Linux cmd@(SetClipboard s) = resolveLinuxApp cmd >>= flip withExternalApp (writeInHandle s)

execute Darwin GetClipboard     = withExternalApp "pbpaste" readOutHandle
execute Darwin (SetClipboard s) = withExternalApp "pbcopy" $ writeInHandle s

-- | Windows: use WinAPI
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
execute Windows GetClipboard =
    bracket_ (openClipboard nullPtr) closeClipboard $ do
        isText <- isClipboardFormatAvailable cF_TEXT
        if isText
            then do
                h <- getClipboardData cF_TEXT
                bracket (globalLock h) globalUnlock $ peekCAString . castPtr
            else throwIO NoTextualData

execute Windows (SetClipboard s) =
    withCAString s $ \cstr -> do
        mem <- globalAlloc gHND memSize
        bracket (globalLock mem) globalUnlock $ \space -> do
            copyMemory space (castPtr cstr) memSize
            bracket_ (openClipboard nullPtr) closeClipboard $ do
                emptyClipboard
                setClipboardData cF_TEXT space
                return ()
    where
        memSize = genericLength s + 1
#endif


-- | Determine the correct Linux command.
resolveLinuxApp :: Command a -> IO String
resolveLinuxApp cmd = decode cmd <$> chooseFirstApp ["xsel", "xclip"] 
    where
        decode :: Command a -> String -> String
        decode GetClipboard     "xsel"  = "xsel -b -o"
        decode (SetClipboard _) "xsel"  = "xsel -b -i"
        decode GetClipboard     "xclip" = "xclip -selection c -o"
        decode (SetClipboard _) "xclip" = "xclip -selection c"

-- | Run external app and apply action to the file handles.
withExternalApp :: String -> IOAction a -> IO a 
withExternalApp app action = 
    bracket (runInteractiveCommand app)
            (\(inp, outp, stderr, pid) -> mapM_ hClose [inp, outp, stderr] >> waitForProcess pid)
            (\(inp, outp, _, _)        -> action (inp, outp))

-- | Search for installed programs and return the first match.
chooseFirstApp :: [String] -> IO String
chooseFirstApp apps = do
    results <- mapM whichCommand apps
    maybe (throwIO $ MissingCommands apps)
          return
          (getFirst . mconcat $ map First results)

-- | Check if cmd is installed by using the which command.
whichCommand :: String -> IO (Maybe String)
whichCommand cmd = do
    (exitCode,_,_) <- readProcessWithExitCode "which" [cmd] ""
    case exitCode of
        ExitSuccess   -> return $ Just cmd
        ExitFailure _ -> return Nothing

readOutHandle :: IOAction String
readOutHandle = hGetContents . stdout

writeInHandle :: String -> IOAction ()
writeInHandle s = flip hPutStr s . stdin

stdin, stdout :: (StdIn, StdOut) -> Handle
stdin = fst
stdout = snd

