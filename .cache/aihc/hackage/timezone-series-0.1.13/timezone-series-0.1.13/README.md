# timezone-series

On Hackage: [timezone-series](http://hackage.haskell.org/package/timezone-series)

This package endows `Data.Time`, from the time package, with several
additional data types for enhanced processing of timezones.

A `TimeZoneSeries` represents a timezone. It describes the timezone with
a set of `Data.Time.TimeZone` objects, each describing the clock setting
in the timezone for a specific period of history during which the
clocks do not change.  The `TimeZoneSeries` as a whole describes what
clock setting was in effect in the timezone at any given time during
the period it covers.

A `ZoneSeriesTime` is a moment in time within a specific timezone.  It
is different than a `Data.Time.ZonedTime` in that a `ZonedTime` specifies
a moment in time for a specific way that the clocks were set in a
timezone. It is the responsibilty of the creator of a `ZonedTime` to
verify that given clock setting was actually in effect at the given
moment in time. A `ZonedTime` for one moment in time cannot be used
reliably on its own to create another `ZonedTime` for a different moment
in the same timezone - information is needed about what clock setting
was in effect in the timezone at the new moment.

This package also provides variants for the functions in Data.Time
that convert between UTC time and local time. These variants take
a `TimeZoneSeries`, representing a timezone, not just a specific
clock setting with the timezone, so the correctness of their results
does not depend on independent knowledge of what clock setting
was in effect at the given time.

Most operating systems provide information about timezone series for
the local timezone and for many other timezones of the world.  On MS
Windows systems, this information can be read from the registry. On
other systems, this information is typically provided in the form of
Olson timezone files: `/etc/localtime` (or some other file) for the
local timezone, and files located in `/usr/share/zoneinfo/` or
`/etc/zoneinfo/` (or some other directory) for other timezones.
See the
[timezone-olson](http://hackage.haskell.org/package/timezone-olson)
and
[timezone-olson-th](http://hackage.haskell.org/package/timezone-olson-th)
packages for more information about reading and creating Olson
timezone files.

Copyright (c) 2010-2021 Yitzchak Gale. All rights reserved.

For licensing information, see the BSD3-style license in the file
LICENSE that was originally distributed by the author together with
this file.

This package is part of the [time-ng project](http://projects.haskell.org/time-ng/).
