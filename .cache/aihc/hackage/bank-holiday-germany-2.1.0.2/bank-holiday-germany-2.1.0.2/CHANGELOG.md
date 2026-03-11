# Changelog for `bank-holiday-germany`

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

Any change that effects the number of holidays in a federal state must
be reflected a minor version increment.

## [2.1.0.0] - 2025-04-18

Increment minor version because version 2.0.1.0 changed the number of
holidays in Sachsen.

## [2.0.1.0] - 2025-04-17

### Fixed

- Fronleichnam is a public holiday in some regions of Sachsen.
  It was missing in this package until now.

## [2.0.0.0] - 2025-04-13

Breaking changes due to extensive refactoring!

### Fixed

- The Weltkindertag on September 20 is a public holiday in Thüringen
  (since 2019). It was missing in this package until now.

### Changed

- Renamed all type constructors to their German names,
  e.g. `Heiligabend` instead of `ChristmasEve`. This was changed to be
  consistent with the former `ExtraHoliday` type.
- Changed `fromDay` to return a `[a]` instead of `Maybe a` because
  there might be different holidays from different federal states on
  the same day in future.
- Combined modules `Data.Time.Calendar.BankHoliday.Germany`
  and `Data.Time.Calendar.BankHoliday.Germany.ExtraHolidays`
  into `Data.Holiday.Germany`.
- Combined types `BankHoliday` and `ExtraHoliday` into new type `Holiday`.
- Adapted all functions for new types.

## [1.3.1.0] - 2025-04-13

### Fixed

- Added missing Weltkindertag for Thüringen (September 20)

## [1.3.0.0] - 2024-04-01

### Added

- Add extra holidays for all remaining federal states.
- Update doc
## [1.2.0.0] - 2024-03-22

### Added

- Add extra holidays for Baden-Württemberg, Nordrhein-Westfalen,
  Hessen, and Niedersachsen
- Update doc

## [1.1.0.0] - 2024-03-19

### Added

- Add `ExtraHoliday` for Bundesland Berlin
- Add and enhance docs

## [1.0.0.2] - 2024-03-18

### Added

- Add module `ExtraHolidays` for additional public holidays
- Export `dayToYear` helper function since it is also used by the new module
- Add tests
- Enhance docs

## [1.0.0.1] - 2024-03-14

### Added

- Added version bounds of dependencies

## [1.0.0.0] - 2024-03-14

Initial release
