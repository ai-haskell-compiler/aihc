{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.WebDriverWrapper.Selenium (startSelenium, getSeleniumIfNeeded) where

import Test.WebDriverWrapper.Constants (defaultSeleniumJarUrl, seleniumPath, downloadPath, geckoDriverPath, seleniumLogPath, chromeDriverPath)
import Test.WebDriverWrapper.Helpers (download)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import Control.Monad (unless)
import GHC.IO.Handle ( Handle, hGetLine, hClose )
import System.Process (ProcessHandle, createProcess)
import Data.String.Interpolate (i)
import System.IO (openFile)
import GHC.IO.IOMode (IOMode(..))
import Control.Retry (retryOnError, RetryStatus (..))
import System.IO.Error (isEOFError)
import UnliftIO.Retry ( constantDelay, limitRetries )
import Data.Foldable.Extra (orM)
import Data.List (isInfixOf)
import System.Process.Run (proc)
import Test.WebDriver.Capabilities (Browser)
import Test.WebDriver (Browser(..))

-- | Starts Selenium and waits for its ok message ( "Selenium Server is up and running" ) to show up at the log file.
-- Takes in a `Browser` (since it needs to use the correct webdriver).
-- Returns the handles for the Selenium process.
startSelenium :: Browser -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
startSelenium browser = do
    geckoPath  <- geckoDriverPath
    chromePath <- chromeDriverPath
    selPath <- seleniumPath
    logFile <- seleniumLogPath
    writeFile logFile "" -- Create file if it doesn't exist, clears it if it does. Needs to happen for "logFileHasReadyMessage" to work correctly. 
    let
        webdriverArgs = case browser of 
            Firefox {} -> [i|-Dwebdriver.gecko.driver=#{geckoPath}|]
            Chrome {}  -> [i|-Dwebdriver.chrome.driver=#{chromePath}|]
            _          -> error "unsuported browser"

        selArgs = webdriverArgs : ["-jar", selPath ,"-log", logFile]
        processParams = proc "java" selArgs
    processHandles <- createProcess processParams
    waitForSeleniumStart
    return processHandles

waitForSeleniumStart :: IO()
waitForSeleniumStart = do
    logFile <- seleniumLogPath
    fileHandle <-  openFile logFile ReadMode
    succeeded <- retryOnError  retryPolicy'
        (\_ e -> return $ isEOFError e)
        (\retryStatus -> if rsIterNumber retryStatus < 599 then logFileHasReadyMessage fileHandle else return False)
    hClose fileHandle
    unless succeeded $ error $ "Couldn't start Selenium successfully. Check " <> logFile <> " for more information."
        where
            -- waits a minute, retrying every 100ms
            retryPolicy' = constantDelay 100000 <> limitRetries 600

logFileHasReadyMessage :: Handle -> IO Bool
logFileHasReadyMessage fileHandle = orM remainingLines
    where
        remainingLines = repeat hasReadyMessage
        hasReadyMessage = (readyMessage `isInfixOf`) <$> hGetLine fileHandle

        readyMessage = "Selenium Server is up and running"

-- | Checks if @selenium@ is in the `downloadPath`. If not, download it. 
getSeleniumIfNeeded :: IO ()
getSeleniumIfNeeded = do
    selPath <- seleniumPath
    hasSelenium  <- doesFileExist selPath
    unless hasSelenium getSelenium

getSelenium :: IO ()
getSelenium = do
    downloadPath' <- downloadPath
    selPath       <- seleniumPath
    createDirectoryIfMissing True downloadPath'
    download defaultSeleniumJarUrl selPath
