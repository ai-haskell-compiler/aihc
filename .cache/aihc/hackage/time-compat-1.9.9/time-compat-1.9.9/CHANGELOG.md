# 1.9.9

- Support `time-1.15`
  - add patterns for DiffTime & NominalDiffTime
  - add more Lift instances
  - fix MonthDay and QuarterDay clipping

# 1.9.8

# 1.9.7

- Drop support for GHCs prior 8.6.5

# 1.9.6.1

- Support `time-1.12`.

# 1.9.6

- Move `Hashable` instance here from `hashable-time` package.
  Note: `ZonedTime` instance is dropped, as `ZonedTime` doesn't have `Eq`
  instance.
- Drop GHC-7.0 and GHC-7.2 support.

# 1.9.5

- Support `time-1.11.1`
  - Add `NFData CalandarDiffDays` instance

# 1.9.4

- Support `time-1.11`
  - `Data.Time.Calendar.Month`
  - `Data.Time.Calendar.Quarter`
  - Pattern synonyms
  - `parseTimeMultipleM` is not backported
  - `Month` is missing `ParseTime` instance
  - Compat extras:
    - Add `Ix`, `Enum`, `NFData` instances to `Month`, `Quarter`, `QuarterOfYear`,
      `CalendarDiffTime` and `DayOfWeek`.

# 1.9.3

- Include `pastMidnight` and `sinceMidnight` aliases (backported from `time-1.10`)
- Support `time-1.10`
