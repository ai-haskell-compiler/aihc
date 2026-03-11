# Haskell `timespan` package

[![CircleCI](https://circleci.com/gh/agrafix/timespan.svg?style=svg)](https://circleci.com/gh/agrafix/timespan)
[![Hackage](https://img.shields.io/hackage/v/timespan.svg)](http://hackage.haskell.org/package/timespan)

The `timespan` packages provides an abstract data type for time spans. You can create them using well named quantities (`seconds`) and
turn them back into a specified quantity. There are also some wrapper functions around commonly used functions that usually represent
time spans as `Int` that now explicitly take a `timespan`.
