leapseconds-announced
=====================

This libraray provides a static [`Data.Time.Clock.TAI.LeapSecondTable`][1]
"containing" the leap seconds announced at library release time.
A new version of the library is released every time the International
Earth Rotation and Reference Systems Service (IERS) announces a new
leap second at http://hpiers.obspm.fr/eoppc/bul/bulc/bulletinc.dat.

This module is intended to provide a quick-and-dirty leap second solution
for one-off analyses concerned only with the past and present (i.e. up
until the next as of yet unannounced leap second), or for applications
which can afford to be recompiled against an updated library as often
as every six months.

[1]: https://hackage.haskell.org/package/time/docs/Data-Time-Clock-TAI.html


Usage
-----

Import `Data.Time.Clock.AnnouncedLeapSeconds` to bring the leap
second table `lst` into scope. Here is a usage example:

```haskell
import Data.Time
import Data.Time.Clock.TAI
import Data.Time.Clock.AnnouncedLeapSeconds

-- | Convert from UTC to TAI.
utcToTAITime' = utcToTAITime lst
-- | Convert from TAI to UTC.
taiToUTCTime' = taiToUTCTime lst

-- | Add a length of time to a UTC time, respecting leap seconds
--   (as opposed to Data.Time.Clock.addUTCTime).
addUTCTime' :: DiffTime -> UTCTime -> Maybe UTCTime
addUTCTime' dt utc = taiToUTCTime' . addAbsoluteTime dt =<< utcToTAITime' utc
```
