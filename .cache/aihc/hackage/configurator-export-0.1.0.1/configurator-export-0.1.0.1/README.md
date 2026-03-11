configurator-export
===================

[![configurator-export on Stackage LTS 4](http://stackage.org/package/configurator-export/badge/lts-4)](http://stackage.org/lts-4/package/configurator-export)
[![configurator-export on Stackage LTS](http://stackage.org/package/configurator-export/badge/lts)](http://stackage.org/lts/package/configurator-export)
[![configurator-export on Stackage Nightly](http://stackage.org/package/configurator-export/badge/nightly)](http://stackage.org/nightly/package/configurator-export)

[![Build Status](https://travis-ci.org/mstksg/configurator-export.svg?branch=master)](https://travis-ci.org/mstksg/configurator-export)

Pretty printers and exporters for 'Config's from the great
*[configurator](http://hackage.haskell.org/package/configurator)* library.

All results are intended to be valid parsing files in the configuration file
syntax of the library.

For a full round trip:

~~~haskell
main = do
  cfg <- load [Required "config.cfg"]
  writeConf "config.cfg" cfg
~~~

This should load the config file, parse it, and then re-export it, rewriting
the original config file.  The result should be an identical configuration
file (with keys potentially re-arranged and re-sorted, comments removed, etc.)

Can also export/print any `HashMap Name Value`, in the form exported from a
`Config` using `getMap`.  Modify a map yourself to dynically
generate/customize configuration files!

Sample output:

~~~haskell
foo {
    bar {
        baz1  = true
        baz2  = [1, 0.6, "hello", true]
    }
    aardvark  = "banana"
    monkey    = [true, false, 1.9e-3]
    zebra     = 24
}

foo2 {
    bar = 8.1e-8
}

apple   = ["cake", true]
orange  = 8943
~~~

Further configuration on sorting of keys, displaying of bools and floats, etc.
is possible by passing in custom `ConfStyle` style option values.


