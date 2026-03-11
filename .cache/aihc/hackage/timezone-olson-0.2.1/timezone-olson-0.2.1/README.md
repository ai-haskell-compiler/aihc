# timezone-olson

On Hackage: [timezone-olson](http://hackage.haskell.org/package/timezone-olson)

This package provides a parser and renderer for binary Olson timezone
files whose format is specified in RFC 8536.

Functions are provided for
converting the parsed data into `TimeZoneSeries` objects from the
[timezone-series](http://hackage.haskell.org/package/timezone-series)
package. On many platforms, binary Olson timezone
files suitable for use with this package are available in the
directory `/usr/share/zoneinfo` and its subdirectories on your computer.

See also the
[timezone-olson-th](http://hackage.haskell.org/package/timezone-olson-th)
package for a way to include timezone informaton from a binary Olson
timezone file at compile time.

Copyright (c) 2010-2021 Yitzchak Gale. All rights reserved.

For licensing information, see the BSD3-style license in the file
LICENSE that was originally distributed by the author together with
this file.

This package is part of the [time-ng project](http://projects.haskell.org/time-ng/).

## Testing utilities

This package also provides two Haskell files, each of which can be
compiled into a command-line utility that might be helpful for testing
purposes.

zhdump.hs: A clone of zdump(8), including most of its bugs, that
           is usually present on systems that have an Olson timezone
           database, except hzdump takes paths to timezone files
           instead of timezone identifiers on the command line.

catTZ:     Read and parse a timezone file, then render it. With the
           -i flag, interprets the timezone data as a TimeZoneSeries
           object before rendering.
