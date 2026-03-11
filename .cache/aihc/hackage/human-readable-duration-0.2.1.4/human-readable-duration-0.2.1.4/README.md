[![Build Status](https://travis-ci.org/yogsototh/human-readable-duration.svg?branch=master)](https://travis-ci.org/yogsototh/human-readable-duration)


[![Hackage](https://img.shields.io/hackage/v/human-readable-duration.svg?maxAge=2592000)](https://hackage.haskell.org/package/human-readable-duration)
[![human-readable-duration](http://stackage.org/package/human-readable-duration/badge/lts)](http://stackage.org/lts-2/package/human-readable-duration)
[![human-readable-duration](http://stackage.org/package/human-readable-duration/badge/nightly)](http://stackage.org/lts-2/package/human-readable-duration)

[![Issue Count](https://codeclimate.com/github/yogsototh/human-readable-duration/badges/issue_count.svg)](https://codeclimate.com/github/yogsototh/human-readable-duration)


# Human Readable Duration

This is a minimal Haskell library to display duration.
It is mostly unsafe as it uses only `Int` and `String`s (not even `Text` nor `ByteString`).

## Usage

~~~ {.haskell}
> import Data.Duration
> let duration = 2*ms + 3*oneSecond + 2*minute + 33*day + 2*year
> humanReadableDuration duration
"2 years 33 days 2 min 3s 2ms"
> getYears duration
2
> getDays duration
763
> getMs duration
65923323002
~~~

## Install

Install [`stack`](http://github.com/commercialhaskell/stack).

~~~
stack build
~~~
