[![Build Status](https://travis-ci.org/olorin/haskell-nagios-check.svg?branch=master)](https://travis-ci.org/olorin/haskell-nagios-check)

# nagios-check

Write Nagios (or Icinga, Shinken, et cetera) plugins in Haskell.

## Documentation

On [Hackage](https://hackage.haskell.org/package/nagios-check).

## Example usage

```haskell
{-# LANGUAGE OverloadedStrings #-}

import System.Nagios.Plugin

universeCheck :: Double -> NagiosPlugin ()
universeCheck pi' = do
    addResult OK "universe passes basic consistency tests"
    if (pi' < (3.0 :: Double)) then addResult Critical "universe broken, π < 3" else return ()

main :: IO ()
main = runNagiosPlugin (universeCheck 3.1415)
```

# Dependencies

nagios-check has been tested with GHC 7.8 and 7.10.
