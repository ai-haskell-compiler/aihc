# Version history for hwk

## 0.6 (2021-06-14)
- add --words mode: applies function to list of words in each line
- experimental shell mode
- initial config fixes

## 0.5 (2020-10-13)
- file arguments can now provide input
- experimental --run mode to execute IO
- eval no longer prints type
- add --config-dir option

## 0.4 (2020-10-12)
- add --eval mode
- refactor: drop ToList and ToString classes
- import Data.List.Extra by default
- --all: remove trailing newline
- add examples/

## 0.3 (2020-10-11)
- add --line mode: takes a function on a String - applied to every line
  - uses new ToString class (not ToList)
- add --type-check: prints the type of a given function

## 0.2 (2020-10-10)
- first release by Jens Petersen
- uses hint library and Hwk configuration module

## 0.1.0.0 (2016-2017)
- original project by Lukas Martinelli
  https://github.com/lukasmartinelli/hwk
