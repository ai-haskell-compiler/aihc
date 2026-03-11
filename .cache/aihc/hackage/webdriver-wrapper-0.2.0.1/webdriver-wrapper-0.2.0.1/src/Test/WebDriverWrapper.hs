{- |
Module : Test.WebDriverWrapper
Description : end-user functions.

The wrapped functions (`wrappedRunSession` and `wrappedRunWD`) will download Selenium and Firefox's webdriver (geckodriver) if they're not already on the `Test.WebDriverWrapper.Constants.downloadPath`, then start Selenium before running the webdriver equivalent function (`runSession` and `runWD`). They kill the Selenium process at the end of their execution. 
-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Test.WebDriverWrapper (wrappedRunSession, wrapWebDriverFunction, wrappedFirefoxRunWD, wrappedChromeRunWD) where

import System.Process (cleanupProcess)
import Control.Concurrent.Async (concurrently_)
import Test.WebDriverWrapper.Selenium (getSeleniumIfNeeded, startSelenium)
import Test.WebDriverWrapper.GeckoDriver (getGeckoDriverIfNeeded)
import Test.WebDriver
    ( runSession,
      Capabilities(..),
      WDConfig(wdCapabilities),
      Browser(..) )
import Test.WebDriver.Types (WD, WDSession)
import Test.WebDriver.Monad (runWD)
import Control.Exception (bracket)
import Test.WebDriverWrapper.ChromeDriver (getChromeDriverIfNeeded)

-- | Same as `runSession`, but starts Selenium before execution and kills Selenium after execution. 
-- Will download Selenium or the browser's webdriver (geckodriver or chromedriver) if any is missing. 
wrappedRunSession :: WDConfig -> WD a -> IO a
wrappedRunSession conf wd = wrapWebDriverFunction (browser $ wdCapabilities conf) (conf, wd) (uncurry runSession)

-- | Same as `runWD`, but starts Selenium before execution and kills Selenium after execution. 
-- Will download Selenium or Firefox's webdriver (geckodriver) if any is missing. 
wrappedFirefoxRunWD ::  WDSession -> WD a -> IO a
wrappedFirefoxRunWD session wd = wrapWebDriverFunction (Firefox{}) (session, wd) (uncurry runWD)

-- | Same as `runWD`, but starts Selenium before execution and kills Selenium after execution. 
-- Will download Selenium or Chrome's webdriver (chromedriver) if any is missing. 
wrappedChromeRunWD ::  WDSession -> WD a -> IO a
wrappedChromeRunWD session wd = wrapWebDriverFunction (Chrome{}) (session, wd) (uncurry runWD)

-- | Runs a function in between starting and killing Selenium. Takes in the arguments and the function, in that order. 
-- Will download Selenium and the Browser's webdriver (geckodriver or chromedriver) if any is missing. 
wrapWebDriverFunction :: Browser -> a -> (a -> IO b) -> IO b
wrapWebDriverFunction browser' webdriverArgs webdriverFunction = do
    case browser' of
        Firefox {}             -> downloadIfMissingGecko
        Chrome  {chromeBinary} -> downloadIfMissingChrome chromeBinary
        _                      -> error "unsuported browser"

    bracket
        (startSelenium browser')
        cleanupProcess
        (const $ webdriverFunction webdriverArgs)

-- | Dowloads Selenium or Firefox's webdriver (geckodriver) if they're missing. 
downloadIfMissingGecko :: IO()
downloadIfMissingGecko =  concurrently_ getSeleniumIfNeeded getGeckoDriverIfNeeded

-- | Dowloads Selenium or Firefox's webdriver (geckodriver) if they're missing. 
-- Takes a @chromeBinary@'s path, whose @chromedriver@ version will match. 
downloadIfMissingChrome :: Maybe FilePath -> IO()
downloadIfMissingChrome chromeBinary' =  concurrently_ getSeleniumIfNeeded $ getChromeDriverIfNeeded chromeBinary'
