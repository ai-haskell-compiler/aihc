module Main where

import System.Time (Day(Monday,Saturday))

import Text.Html(Html, (!), (+++), (<<), toHtml)
import qualified Text.Html as Html

import Data.Maybe (fromMaybe, )
import Data.List.HT (sliceVertical, )

import qualified Data.Map as Map



type Year = Int

-- cf. HTam.NumberTheory
divides :: Int -> Year -> Bool
divides k n  =  mod n k == 0

isLeapYear :: Year -> Bool
isLeapYear year =
   (divides 4 year && not (divides 100 year)) || divides 400 year

{-
We want to know, which years share the same calendar.
That is we want to know, in which year we can re-use the calendar of this year.
There are 14 types of years, because of two parameters:
The starting weekday of the year and whether there is a leap day or not.
-}
type YearType = (Day, Bool)

succDay :: Day -> Day
succDay d = if d==maxBound then minBound else succ d

futureYearTypes :: [(Year, YearType)]
futureYearTypes =
   iterate
      (\(year,(day,leap)) ->
          let newYear = succ year
              startDay = succDay $
                 if leap
                   then succDay day
                   else day
          in  (newYear, (startDay, isLeapYear newYear)))
      (2001,(Monday,False))


predDay :: Day -> Day
predDay d = if d==minBound then maxBound else pred d

pastYearTypes :: [(Year, YearType)]
pastYearTypes =
   iterate
      (\(year,(day,_)) ->
          let oldYear = pred year
              leap    = isLeapYear oldYear
              startDay = predDay $
                 if leap
                   then predDay day
                   else day
          in  (oldYear, (startDay, leap)))
      (2000,(Saturday,True))


-- compute equivalent years
exampleYearTypeMap :: Map.Map YearType [Year]
exampleYearTypeMap =
   let years = reverse (take 99 futureYearTypes) ++ take 101 pastYearTypes
   in  Map.fromListWith (++) (map (\(year,yearType) -> (yearType,[year])) years)


exampleYearTypeMapStr :: String
exampleYearTypeMapStr =
   let showYear ((day,leap),years) =
          show day ++ " " ++
          (if leap then "(leap)" else "      ") ++ "\t" ++
          show years
   in  unlines $ map showYear $ Map.toList exampleYearTypeMap


weekdays :: [Day]
weekdays = [minBound..maxBound]

yearToHtml :: Year -> Html
yearToHtml = toHtml . show

exampleHtml :: Html
exampleHtml =
   let makeYearTable leap =
          Html.simpleTable [] []
               (map (\d -> (toHtml (show d) :) $
                           map (\y -> Html.anchor (yearToHtml y) !
                                         [Html.name (show y)]) $
                           fromMaybe (error "every weekday must exist") $
                           Map.lookup (d,leap) exampleYearTypeMap) weekdays)
            ! [Html.border 1]
   in  Html.simpleTable [] []
            (sliceVertical 20 $
               map (\y -> Html.anchor (yearToHtml y) ! [Html.href ('#':show y)])
                   [1900..2099::Int])
         ! [Html.border 1]
        +++ Html.h2 << "Normale Jahre" +++ makeYearTable False
        +++ Html.h2 << "Schaltjahre"   +++ makeYearTable True
        +++ foldl1 (+++) (replicate 30 Html.br)



writeHtml :: IO ()
writeHtml = writeFile "CalendarTable.hsc" (show exampleHtml)

main :: IO ()
main = writeHtml
