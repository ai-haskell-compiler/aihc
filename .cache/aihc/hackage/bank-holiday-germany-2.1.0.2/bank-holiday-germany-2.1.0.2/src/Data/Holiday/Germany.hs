
{-|
Description: Calculation of bank holidays and public holidays for Germany.

This module computes bank holidays and public holidays for Germany and
its federal states.

Use 'isBankHoliday' to check if a holiday is also a bank holiday.
Use 'isGermanPublicHoliday' and 'isFederalPublicHoliday' to check if a
holiday is also a public holiday.

You can test this package or just calculate a few holidays with GHCi:

@
$ stack ghci --package time --package bank-holiday-germany
ghci> import Data.Time
ghci> import Data.Holiday.Germany
ghci> fromDay (fromGregorian 2024 5 1)  -- Tag der Arbeit
[ErsterMai]
ghci> isBankHoliday Heiligabend
True
ghci> isGermanPublicHoliday Heiligabend
False
ghci> holidaysBetween (fromGregorian 2024 12 1) (fromGregorian 2024 12 26)
[(2024-12-24,Heiligabend),(2024-12-25,ErsterWeihnachtsfeiertag),(2024-12-26,ZweiterWeihnachtsfeiertag)]
@

Public holidays – except for 'Data.Holiday.Germany.TagDerDeutschenEinheit' –
are under federal obligations in Germany („Ländersache“).

Most bank holidays are also federal public holidays and vice versa.
But there are some additional holidays which may differ between
federal states.

For example, Heilige Drei Könige is not a bank holiday but it is a
public holiday in Bavaria.

The following example prints all public holidays in Bavaria (Landkreis
Miesbach, Oberbayern) in 2025:

@
import Prelude
import Data.Time
import Data.Holiday.Germany

start = fromGregorian 2025 1 1

end = fromGregorian 2025 12 31

main :: IO ()
main = putStrLn
  $ unlines
  $ map (\\(d,x) -> show d ++ " " ++ germanHolidayName x)
  $ filter (\\(_,x) -> isFederalPublicHoliday Bayern x && x /= Friedensfest)
  $ holidaysBetween start end
@

Resources:

Public holidays are implemented for all 16 federal states. For some
states, we couldn't find official sources; That's why this list
only includes some states.

 - Bankfeiertage: https://de.wikipedia.org/wiki/Bankfeiertag
 - Übersicht: https://de.wikipedia.org/wiki/Gesetzliche_Feiertage_in_Deutschland
 - Weitere Übersicht: https://www.arbeitstage.org/
 - Bayern: https://www.stmi.bayern.de/suv/feiertage/
 - Baden-Württemberg: https://im.baden-wuerttemberg.de/de/service/feiertage
 - Niedersachsen: https://service.niedersachsen.de/portaldeeplink/?tsa_leistung_id=8664664&tsa_sprache=de_DE
 - Hessen: https://innen.hessen.de/buerger-staat/feiertage
 - Rheinland-Pfalz: https://mdi.rlp.de/themen/buerger-und-staat/verfassung-und-verwaltung/sonn-und-feiertagsrecht
 - Brandenburg: https://bravors.brandenburg.de/gesetze/ftg_2003/6
 - Sachsen: https://www.revosax.sachsen.de/vorschrift/3997-SaechsSFG
 - Sachsen-Anhalt: https://www.landesrecht.sachsen-anhalt.de/bsst/document/jlr-FeiertGSTrahmen/part/X
 - Thüringen: https://innen.thueringen.de/staats-und-verwaltungsrecht/oeffentliches-recht/feiertagsrecht
 - Hamburg: https://www.hamburg.de/ferien-und-feiertage/
 - Mecklenburg-Vorpommern u. Berlin (Frauentag): https://www.deutsche-rentenversicherung.de/DRV/DE/Ueber-uns-und-Presse/Presse/Meldungen/2024/240306_frauentag_feiertag_frei.html
 - Saarland: https://www.saarland.de/mibs/DE/themen-aufgaben/aufgaben/buerger_und_staat/sonn_u_feiertagsrecht/feiertagsrecht_node.html
 - Bremen: https://www.transparenz.bremen.de/metainformationen/gesetz-ueber-die-sonn-gedenk-und-feiertage-vom-12-november-1954-145882?asl=bremen203_tpgesetz.c.55340.de&template=20_gp_ifg_meta_detail_d
-}

module Data.Holiday.Germany (
    Holiday(..),
    FederalState(..),
    isBankHoliday,
    isGermanPublicHoliday,
    isFederalPublicHoliday,
    calculateEasterSunday,
    holidaysBetween,
    fromDay,
    toDay,
    germanHolidayName,
    yearFromDay
) where

import Prelude
import Data.Time.Calendar

-- | Data type specifying all German holidays.
--
-- Note: This type cannot be an instance of class 'Ord' because due to
-- Easter day calculation the order can change from year to year.
data Holiday
    = Neujahrstag        -- ^ Neujahrstag
    | Karfreitag         -- ^ Karfreitag
    | Ostermontag        -- ^ Ostermontag
    | ErsterMai          -- ^ Tag der Arbeit – 1. Mai
    | ChristiHimmelfahrt -- ^ Christi Himmelfahrt
    | Pfingstmontag      -- ^ Pfingstmontag
    | TagDerDeutschenEinheit -- ^ Tag der Deutschen Einheit
    | Heiligabend        -- ^ Heilig Abend
    | ErsterWeihnachtsfeiertag  -- ^ 1​. Weihnachtsfeiertag
    | ZweiterWeihnachtsfeiertag -- ^ 2​. Weihnachtsfeiertag
    | Silvestertag           -- ^ Silvestertag
    | HeiligeDreiKoenige     -- ^ Heilige Drei Könige (Bayern, Baden-Württemberg, Sachsen-Anhalt)
    | Fronleichnam           -- ^ Fronleichnam (Bayern, Baden-Württemberg, Nordrhein-Westfalen, Hessen, Rheinland-Pfalz, Thüringen*, Saarland, Sachsen*)
    | Friedensfest           -- ^ Friedensfest (Bayern*)
    | MariaeHimmelfahrt      -- ^ Mariä Himmelfahrt (Bayern*, Saarland)
    | Allerheiligen          -- ^ Allerheiligen (Bayern, Baden-Württemberg, Nordrhein-Westfalen, Rheinland-Pfalz, Saarland)
    | Reformationstag        -- ^ Reformationstag (Niedersachsen, Sachsen, Schleswig-Holstein, Brandenburg, Sachsen-Anhalt, Thüringen, Hamburg, Mecklenburg-Vorpommern, Bremen)
    | InternationalerFrauentag -- ^ Internationaler Frauentag (Berlin, Mecklenburg-Vorpommern)
    | BussUndBettag          -- ^ Buß- und Bettag (Sachsen)
    | Weltkindertag          -- ^ Weltkindertag (Thüringen)
    deriving (Enum, Eq, Bounded, Show, Read)

-- | Germany's federal states – Deutsche Bundesländer.
data FederalState
  = BadenWuerttemberg
  | Bayern
  | Berlin
  | Brandenburg
  | Bremen
  | Hamburg
  | Hessen
  | MecklenburgVorpommern
  | Niedersachsen
  | NordrheinWestfalen
  | RheinlandPfalz
  | Saarland
  | Sachsen
  | SachsenAnhalt
  | SchleswigHolstein
  | Thueringen
  deriving (Enum, Eq, Bounded, Show, Read)

-- | Check if a given holiday is a bank holiday.
--
-- >>> isBankHoliday Heiligabend
-- True
isBankHoliday :: Holiday -> Bool
isBankHoliday Heiligabend = True
isBankHoliday Silvestertag = True
isBankHoliday holiday = isGermanPublicHoliday holiday

-- | Helper to extract the year from a date.
--
-- >>> yearFromDay $ fromGregorian 2020 1 1
-- 2020
yearFromDay :: Day -> Year
yearFromDay = (\(y, _, _) -> y) . toGregorian

-- | Calculate Easter Sunday using Spencer's algorithm.
calculateEasterSunday :: Year -> Day
calculateEasterSunday year =
    let
        a = year `rem` 19
        b = year `quot` 100
        c = year `rem` 100
        d = b `quot` 4
        e = b `rem` 4
        f = (b + 8) `quot` 25
        g = (b - f + 1) `quot` 3
        h = (19 * a + b - d - g + 15) `rem` 30
        i = c `quot` 4
        k = c `rem` 4
        l = (32 + 2 * e + 2 * i - h - k) `rem` 7
        m = (a + 11 * h + 22 * l) `quot` 451
        n = (h + l - 7 * m + 114) `quot` 31
        o = (h + l - 7 * m + 114) `rem` 31
    in
        fromGregorian year (fromIntegral n) (fromIntegral o + 1)

-- | Compute the date for a given year and bank holiday.
--
-- >>> toDay 2024 ErsterMai
-- 2024-05-01
toDay :: Year -> Holiday -> Day
toDay year Neujahrstag        = fromGregorian year 1 1
toDay year Karfreitag         = addDays (-2) (calculateEasterSunday year)
toDay year Ostermontag        = addDays 1 (calculateEasterSunday year)
toDay year ErsterMai          = fromGregorian year 5 1
toDay year ChristiHimmelfahrt = addDays 39 (calculateEasterSunday year)
toDay year Pfingstmontag      = addDays 50 (calculateEasterSunday year)
toDay year TagDerDeutschenEinheit = fromGregorian year 10 3
toDay year Heiligabend        = fromGregorian year 12 24
toDay year ErsterWeihnachtsfeiertag  = fromGregorian year 12 25
toDay year ZweiterWeihnachtsfeiertag = fromGregorian year 12 26
toDay year Silvestertag            = fromGregorian year 12 31
toDay year HeiligeDreiKoenige      = fromGregorian year 1 6
toDay year Fronleichnam            = addDays 60 $ calculateEasterSunday year
toDay year Friedensfest            = fromGregorian year 8 8
toDay year MariaeHimmelfahrt       = fromGregorian year 8 15
toDay year Allerheiligen           = fromGregorian year 11 1
toDay year InternationalerFrauentag = fromGregorian year 3 8
toDay year Reformationstag          = fromGregorian year 10 31
toDay year BussUndBettag           = calculateBussUndBettag year
toDay year Weltkindertag           = fromGregorian year 9 20

-- | Compute list of holidays for a given date.
--
-- Note: In some years, two bank holidays can fall on the same
-- day. E.g. 'ErsterMai' and 'ChristiHimmelfahrt' in 2008 are both on
-- 2008-05-01.
--
-- >>> fromDay (fromGregorian 2024 1 1)
-- [Neujahrstag]
--
-- >>> fromDay (fromGregorian 2024 5 5)
-- []
fromDay :: Day -> [Holiday]
fromDay day = filter (\d -> day == toDay (yearFromDay day) d) [minBound..maxBound]

-- | Compute pairs of date and holiday from start to end (inclusive).
--
-- Note: In some years, two holidays can fall on the same
-- day. In such cases both holidays will be in the resulting list.
-- See 'fromDay' for more information.
--
-- >>> map snd $ holidaysBetween (fromGregorian 2024 12 25) (fromGregorian 2024 12 26)
-- [ErsterWeihnachtsfeiertag,ZweiterWeihnachtsfeiertag]
holidaysBetween :: Day -> Day -> [(Day, Holiday)]
holidaysBetween start end = concat $ map (\d -> map (d,) $ fromDay d) [start..end]

-- | Translate the holiday name to German.
germanHolidayName :: Holiday -> String
germanHolidayName d = case d of
  Neujahrstag        -> "Neujahrstag"
  Karfreitag         -> "Karfreitag"
  Ostermontag        -> "Ostermontag"
  ErsterMai          -> "Tag der Arbeit"
  ChristiHimmelfahrt -> "Christi Himmelfahrt"
  Pfingstmontag      -> "Pfingstmontag"
  TagDerDeutschenEinheit -> "Tag der Deutschen Einheit"
  Heiligabend        -> "Heilig Abend"
  ErsterWeihnachtsfeiertag  -> "1. Weihnachtsfeiertag"
  ZweiterWeihnachtsfeiertag -> "2. Weihnachtsfeiertag"
  Silvestertag           -> "Silvestertag"
  HeiligeDreiKoenige     -> "Heilige Drei Könige"
  Fronleichnam           -> "Fronleichnam"
  Friedensfest           -> "Friedensfest"
  MariaeHimmelfahrt      -> "Mariä Himmelfahrt"
  Allerheiligen          -> "Allerheiligen"
  Reformationstag          -> "Reformationstag"
  InternationalerFrauentag -> "Internationaler Frauentag"
  BussUndBettag            -> "Buß- und Bettag"
  Weltkindertag            -> "Weltkindertag"

-- | True only for public holidays that are holidays in all federal states.
-- 'Heiligabend' and 'Silvestertag' are bank holidays but not public holidays.
-- 'Allerheiligen' is only a holiday in some federal states bot not throughout Germany.
--
-- >>> isGermanPublicHoliday ErsterMai
-- True
isGermanPublicHoliday :: Holiday -> Bool
isGermanPublicHoliday Neujahrstag = True
isGermanPublicHoliday Karfreitag = True
isGermanPublicHoliday Ostermontag = True
isGermanPublicHoliday ErsterMai = True
isGermanPublicHoliday ChristiHimmelfahrt = True
isGermanPublicHoliday Pfingstmontag = True
isGermanPublicHoliday TagDerDeutschenEinheit = True
isGermanPublicHoliday ErsterWeihnachtsfeiertag = True
isGermanPublicHoliday ZweiterWeihnachtsfeiertag = True
isGermanPublicHoliday _ = False

-- | Check if 'Holiday' is a holiday in the given federal state.
--
-- Note: Internationaler Frauentag is a holiday in Berlin (since 2019)
-- and Mecklenburg-Vorpommern (since 2023).
-- However this function doesn't take the year into account and hence
-- is incorrect for earlier years.
--
-- >>> isFederalPublicHoliday Bayern Allerheiligen
-- True
--
-- >>> isFederalPublicHoliday Berlin Allerheiligen
-- False
isFederalPublicHoliday :: FederalState -> Holiday -> Bool
isFederalPublicHoliday BadenWuerttemberg HeiligeDreiKoenige = True
isFederalPublicHoliday BadenWuerttemberg Fronleichnam = True
isFederalPublicHoliday BadenWuerttemberg Allerheiligen = True
isFederalPublicHoliday Bayern HeiligeDreiKoenige = True
isFederalPublicHoliday Bayern Fronleichnam = True
isFederalPublicHoliday Bayern Friedensfest = True
isFederalPublicHoliday Bayern MariaeHimmelfahrt = True
isFederalPublicHoliday Bayern Allerheiligen = True
isFederalPublicHoliday Berlin InternationalerFrauentag = True
isFederalPublicHoliday NordrheinWestfalen Fronleichnam = True
isFederalPublicHoliday NordrheinWestfalen Allerheiligen = True
isFederalPublicHoliday Niedersachsen Reformationstag = True
isFederalPublicHoliday Hessen Fronleichnam = True
isFederalPublicHoliday RheinlandPfalz Allerheiligen = True
isFederalPublicHoliday RheinlandPfalz Fronleichnam = True
isFederalPublicHoliday Sachsen Fronleichnam = True
isFederalPublicHoliday Sachsen Reformationstag = True
isFederalPublicHoliday Sachsen BussUndBettag = True
isFederalPublicHoliday SchleswigHolstein Reformationstag = True
isFederalPublicHoliday Brandenburg Reformationstag = True
isFederalPublicHoliday SachsenAnhalt HeiligeDreiKoenige = True
isFederalPublicHoliday SachsenAnhalt Reformationstag = True
isFederalPublicHoliday Thueringen Fronleichnam = True
isFederalPublicHoliday Thueringen Reformationstag = True
isFederalPublicHoliday Thueringen Weltkindertag = True
isFederalPublicHoliday Hamburg Reformationstag = True
isFederalPublicHoliday MecklenburgVorpommern Reformationstag = True
isFederalPublicHoliday MecklenburgVorpommern InternationalerFrauentag = True
isFederalPublicHoliday Saarland Fronleichnam = True
isFederalPublicHoliday Saarland Allerheiligen = True
isFederalPublicHoliday Saarland MariaeHimmelfahrt = True
isFederalPublicHoliday Bremen Reformationstag = True
isFederalPublicHoliday _ holiday = isGermanPublicHoliday holiday

-- | Calculate Buß- und Bettag.
--
-- https://de.wikipedia.org/wiki/Bu%C3%9F-_und_Bettag
calculateBussUndBettag :: Year -> Day
calculateBussUndBettag year =
   let november23 = fromGregorian year 11 23
       weekDay = dayOfWeek november23
    in if weekDay <= Wednesday
         then addDays (toInteger $ fromEnum Wednesday - fromEnum weekDay - 7) november23
         else addDays (toInteger $ fromEnum Wednesday - fromEnum weekDay) november23
