-- |
-- Module      :  EndOfExe2
-- Copyright   :  (c) Oleksandr Zhabenko 2019-2024
-- License     :  MIT
-- Maintainer  :  oleksandr.zhabenko@yahoo.com
--
-- A small library to deal with executable endings. Uses a Maybe data representation inside an IO monad.
--
-- It is a fork of now deprecated library [mmsyn3](https://hackage.haskell.org/package/mmsyn3).

{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK -show-extensions #-}

module EndOfExe2 where

import GHC.Base
import GHC.List
import qualified System.Directory as D (findExecutable)
import Data.Maybe (isJust,isNothing,fromJust)
import System.IO.Unsafe (unsafePerformIO,unsafeDupablePerformIO)

-- | Can be used instead of 'System.Info.os' to check whether the executable ends in \".exe\". The function returns 'IO' 'Nothing' if there is neither 
-- @ys@ nor @(ys ++ ".exe")@ names for executables in the search path. It can also search in other locations and its behaviour is OS dependent. For more information, please, refer to the link: <https://hackage.haskell.org/package/directory-1.3.4.0/docs/System-Directory.html#v:findExecutable>
-- For more information, how the executable is searched, see also the following address: <https://hackage.haskell.org/package/process-1.6.18.0/docs/System-Process.html#t:CmdSpec> 
maybeEndOfExecutable :: String -> IO (Maybe String)
maybeEndOfExecutable ys = do
  xs <- D.findExecutable ys
  if isJust xs 
    then return xs
    else do
      zs <- D.findExecutable (ys ++ ".exe")
      if isJust zs
        then return zs
        else return Nothing

-- | The function 'endOfExe' returns 'IO' \"\" if no executable found by 'D.findExecutable'. Otherwise, it returns its path in the 'IO' monad.
endOfExe :: String -> IO String
endOfExe ys = do
  xs <- D.findExecutable ys
  if isJust xs 
    then return . fromJust $ xs
    else do
      zs <- D.findExecutable (ys ++ ".exe")
      if isJust zs
        then return . fromJust $ zs
        else return ""
                                  
-- | Gets the proper name of the executable in the system (it must be seen in the directories in the @PATH@ variable). 
-- Further you can adopt it to be used 
-- inside the 'System.Process.callCommand' as the name of the executable
showE :: String -> Maybe String
showE xs 
  | null xs = Nothing
  | otherwise = unsafePerformIO . maybeEndOfExecutable $ xs
{-# INLINE showE #-}

-- | Similar to 'showE' but uses 'unsafeDupablePerformIO', which is more efficient, but for the multiprocessor can lead to executing the IO action multiple times.
showEDup :: String -> Maybe String
showEDup xs 
  | null xs = Nothing
  | otherwise = unsafeDupablePerformIO . maybeEndOfExecutable $ xs
{-# INLINE showEDup #-}

-- | If executable not found, then returns empty 'String'.
showE0 :: String -> String
showE0 xs 
  | null xs = ""
  | otherwise = unsafePerformIO . endOfExe $ xs
{-# INLINE showE0 #-}

-- | If executable not found, then returns empty 'String'. Uses 'unsafeDupablePerformIO'.
showE0Dup :: String -> String
showE0Dup xs 
  | null xs = ""
  | otherwise = unsafeDupablePerformIO . endOfExe $ xs
{-# INLINE showE0Dup #-}

