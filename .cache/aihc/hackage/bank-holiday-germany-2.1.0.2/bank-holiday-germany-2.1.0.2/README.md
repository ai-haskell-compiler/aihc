# bank-holiday-germany

*See below for a German version.*

This package provides calculation of bank holidays and public holidays
in Germany.

Most bank holidays are also public aka legal holidays
throughout Germany. You can use `isPublicHoliday` to check if a
holiday is also a public holiday.

There are even more public holidays in each federal state.

Public holidays are generally off for all employees. Bank holidays that
are not public holidays are generally only off for bank employees.

See the [module documentation](https://hackage.haskell.org/package/bank-holiday-germany)
on Hackage for more information.


-----

Dieses Modul behandelt deutsche Bankfeiertage und gesetzliche Feiertage.

Bis auf Heilig Abend und Silvester sind alle Bankfeiertage
gleichzeitig gesetzliche Feiertage in allen Bundesländern der
Bundesrepublik Deutschland. Die Funktion `isPublicHoliday` prüft ob
ein Bankfeiertag auch ein gesetzlicher Feiertag ist.

Darüber hinaus gibt es je nach Bundesland weitere gesetzliche Feiertage.

**Für alle 16 Bundesländer sind damit die jeweiligen Feiertage
vollständig implementiert (Stand 2024-03-31).**

Bankfeiertage sind in der Regel für Bankangestellte frei.
Gesetzliche Feiertage sind in der Regel für alle Angestellten frei (im
Bundesland für das sie gelten).

Vorsicht: Manche gesetzliche Feiertage gelten nicht für ein ganzes
Bundesland sondern nur für bestimmte Landkreise, z.B. das Friedensfest
in Augsburg.

Gesetzliche Feiertage sind übrigens Ländersache – abgesehen vom
[Nationalfeiertag](https://www.bmi.bund.de/DE/themen/verfassung/staatliche-symbole/nationale-feiertage/nationale-feiertage-node.html)
*Tag der Deutschen Einheit*.

## A rewrite to version 2

Sorry for the incompatible changes introduced by the rewrite to version 2.
The rational for the big refactoring was to simplify the library's
interface by unifying `BankHoliday` and `ExtraHoliday` types.

### How to migrate to version 2

The following functions haven't changed semantically:

```haskell
toDay :: Year -> Holiday -> Day
fromDay :: Day -> [Holiday]
holidaysBetween :: Day -> Day -> [(Day, Holiday)]
germanHolidayName :: Holiday -> String
```

However, they are now returning more holidays – not only bank holidays
but the union of all kind of holidays of all federal states.

It's now on you to filter for the holidays you're interested in. You
can use the following functions:

```haskell
isBankHoliday :: Holiday -> Bool
isGermanPublicHoliday :: Holiday -> Bool
isFederalPublicHoliday :: FederalState -> Holiday -> Bool
```

Changes:

- `isPublicHoliday` has been renamed to `isGermanPublicHoliday`.
- `isBankHoliday` now takes a `Holiday` instead of a `Day`.
- The namespace changed from `Data.Time.Calendar.BankHoliday.Germany`
  to `Data.Holiday.Germany`.
- `fromDay` now returns a list of holidays instead of a `Maybe`.

### Why didn't you get the design right in the first place?

For version 1, I looked on
[hackage](https://hackage.haskell.org/packages/search?terms=bank%20holiday)
for other holiday packages and there have been two conventions:

1. `bank-holiday-*` with modules like `Data.Time.Calendar.BankHoliday.*`
2. `*-holidays` with modules like `Data.Holiday.*`

I settled with the first scheme because I initially only wanted to
provide bank holidays and had no plans to support all federal
holidays. The library evolved and over time we added support for
public holidays and federal holidays. That was when the library's
interface got complicated.

The module path was long, clumsy and inaccurate because it wasn't only
about bank holidays anymore.

### Why the mix of english function names and german type constructors?

I choose to use german names for type constructors for simplicity and consistency.
It's hard enough to find a common name and notation for German
holidays (e.g. "1. Mai", "1. Maifeiertag", "Tag der Arbeit", etc.).
But it's getting harder to find translations for e.g. "Augsburger
Friedensfest" ("Augsburg High Festival of Peace" – source:
bavarikon.de – doesn't sound right).

Also, many German users of this library will have an easier time when
they don't have to look up words like "Whit Monday", "Ascension Day",
or "Epiphany".

## Sample code for version 2

Rank federal states by number of holidays:

`test2.hs`: Number of holidays per federal state.

```haskell
import Prelude
import Data.List
import Data.Time
import Data.Holiday.Germany

holidays :: Year -> FederalState -> [(Day, Holiday)]
holidays year state = filter (isFederalPublicHoliday state . snd) $ holidaysBetween start end
  where
    start = fromGregorian year 1 1
    end = fromGregorian year 12 31

supportedFederalStates :: [FederalState]
supportedFederalStates = [minBound .. maxBound]

year :: Year
year = 2025

showPadded :: Int -> String
showPadded n | n < 10 = " " ++ show n
             | otherwise = show n

main :: IO ()
main = putStrLn
         $ unlines
         $ map (\(x, n) -> showPadded n ++ "  " ++ show x)
         $ sortOn ((0-) . snd)
         $ map (\x -> (x, length $ holidays year x))
         $ supportedFederalStates
```

```
$ stack runghc --package time test2.hs
14  Bayern
12  BadenWuerttemberg
12  Saarland
12  Thueringen
11  MecklenburgVorpommern
11  NordrheinWestfalen
11  RheinlandPfalz
11  Sachsen
11  SachsenAnhalt
10  Berlin
10  Brandenburg
10  Bremen
10  Hamburg
10  Hessen
10  Niedersachsen
10  SchleswigHolstein
```

`test3.hs`: Number of holidays on week days.

```haskell
import Prelude
import Data.List
import Data.Time
import Data.Holiday.Germany

holidays :: Year -> FederalState -> [(Day, Holiday)]
holidays year state = filter (isFederalPublicHoliday state . snd) $ holidaysBetween start end
  where
    start = fromGregorian year 1 1
    end = fromGregorian year 12 31

year :: Year
year = 2025

main :: IO ()
main = putStrLn
         $ unlines
         $ map (\(x,n) -> show n ++ " " ++ show x)
         $ map (\xs@(x:_) -> (x, length xs))
         $ group
         $ sort
         $ map (dayOfWeek . fst)
         $ filter ((/=Friedensfest) . snd)
         $ holidays year Bayern
```

```
$ stack runghc --package time test3.hs
3 Monday
1 Wednesday
4 Thursday
4 Friday
1 Saturday
```

More examples:

- [Holidays](https://hackage.haskell.org/package/bank-holiday-germany/docs/Data-Holiday-Germany.html)

## Sample code for version 1

Rank federal states by number of holidays:

`test1.hs`:

```haskell
import Prelude
import Data.List
import Data.Time
import qualified Data.Time.Calendar.BankHoliday.Germany as BH
import qualified Data.Time.Calendar.BankHoliday.Germany.ExtraHolidays as EH
import Data.Time.Calendar.BankHoliday.Germany (BankHoliday(..))
import Data.Time.Calendar.BankHoliday.Germany.ExtraHolidays (FederalState(..), ExtraHoliday(..))

holidays :: Year -> FederalState -> [Day]
holidays year state = map fst (filter (BH.isPublicHoliday . snd) $ BH.holidaysBetween start end)
                   ++ map fst (EH.holidaysBetween state start end)
  where
    start = fromGregorian year 1 1
    end = fromGregorian year 12 31


supportedFederalStates :: [FederalState]
supportedFederalStates = [minBound .. maxBound]

year :: Year
year = 2024

showPadded :: Int -> String
showPadded n | n < 10 = " " ++ show n
             | otherwise = show n

main :: IO ()
main = putStrLn
         $ unlines
         $ map (\(x, n) -> showPadded n ++ "  " ++ show x)
         $ sortOn ((0-) . snd)
         $ map (\x -> (x, length $ holidays year x))
         $ supportedFederalStates
```

```
$ stack script --resolver=lts-22.0 --package time --package bank-holiday-germany test1.hs
14  Bayern
12  BadenWuerttemberg
12  Saarland
12  Thueringen
11  MecklenburgVorpommern
11  NordrheinWestfalen
11  RheinlandPfalz
11  Sachsen
11  SachsenAnhalt
10  Berlin
10  Brandenburg
10  Bremen
10  Hamburg
10  Hessen
10  Niedersachsen
10  SchleswigHolstein
```

More examples:

- [Bank holidays](https://hackage.haskell.org/package/bank-holiday-germany-1.3.0.0/docs/Data-Time-Calendar-BankHoliday-Germany.html)
- [Extra holidays](https://hackage.haskell.org/package/bank-holiday-germany-1.3.0.0/docs/Data-Time-Calendar-BankHoliday-Germany-ExtraHolidays.html)
