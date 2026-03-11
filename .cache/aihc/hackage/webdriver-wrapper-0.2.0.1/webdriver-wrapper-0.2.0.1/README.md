# webdriver-wrapper

This package was created so end-users don't need to manually manage Selenium or their webdriver when dealing with the [webdriver](https://hackage.haskell.org/package/webdriver) package.

This package is strongly inspired by [sandwich-webdriver](https://hackage.haskell.org/package/sandwich-webdriver). My original use-case didn't require the sandwich test suite, though, so I made this package which focuses on being simpler and more minimalistic. 

## Extra dependencies
You must have Java installed, since Selenium is distributed as a `.jar` file. I'm running OpenJDK 17 on my machine, and it just works.

## How to use
The `wrappedRunSession` function is a drop-in replacement for `runSession`. It will download, if needed, Selenium and a webdriver to `~/.local/haskell-webdriver-wrapper/{your_architecture}` (on Linux) or `%APPDATA%/haskell-webdriver-wrapper/{your_architecture}` (on Windows). Then it starts Selenium and runs your `WD a` computation, closing Selenium whether WD succeeds or fails. 

For `runWD`, there are two functions: `wrappedFirefoxRunWD` and `wrappedChromeRunWD`. They're also drop-in replacements for `runWD`, and will also handle everything related to Selenium - the only difference being, as you can imagine, `wrappedFirefoxRunWD` runs your session on Firefox and `wrappedChromeRunWD` runs your session on Chrome. 