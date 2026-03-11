{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Time.Calendar.HebrewSpec (spec) where

import Data.Time.Calendar.Hebrew.Internal
import Test.Hspec
import Test.Hspec.QuickCheck

import Control.Applicative ((<$>))
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Calendar (fromGregorian, Day (..))
import Test.QuickCheck

spec :: Spec
spec = do
  prop "join and split chalakim" $ \tc ->
    uncurry4 joinChalakim (splitChalakim tc) `shouldBe` tc
  prop "to/from julian date" $ \yl yt j ->
    uncurry (julianFromDate yl yt) (dateFromJulian yl yt j) `shouldBe` j
  it "first rosh hashana is day 1" $
    roshHashana 1 `shouldBe` 1
  prop "rosh hashana/julianFromDays" $ \y ->
    julianFromDays (roshHashana y) `shouldBe` (y, 1)
  prop "to/from hebrew" $ \d' ->
    let d = ModifiedJulianDay d'
     in fromHebrew (toHebrew d) `shouldBe` d
  it "splitChalakim " $ do
    splitChalakim 1080 `shouldBe` (0, 0, 1, 0)
    splitChalakim (15 * 24 * 1080) `shouldBe` (2, 1, 0, 0)
  it "molad tishrei" $ do
    let testMolad w x y z =
          let (_, d, s, c) = splitChalakim $ moladTishrei w
           in (w, d, s, c) `shouldBe` (w, x, y, z)
    testMolad 5764 5 10 491
    testMolad 1 1 5 204
    testMolad 2 5 14 0
    testMolad 3 2 22 876
    testMolad 4 1 20 385
    testMolad 5 6 5 181
    testMolad 6 3 13 1057
    testMolad 7 2 11 566
    testMolad 8 6 20 362
    testMolad 9 5 17 951
    testMolad 10 3 2 747
    testMolad 11 0 11 543
    testMolad 18 0 15 414
    testMolad 19 5 0 210
    testMolad 20 3 21 799
  prop "months til tishrei" $ \y ->
    monthsTilTishrei y == monthsTilTishreiLong y
  prop "valid year length" $ \y ->
    let l = yearLength y
     in l `elem` [353, 354, 355, 383, 384, 385]
  it "months til tishrei case" $ do
    monthsTilTishrei 1 `shouldBe` 0
    monthsTilTishrei 2 `shouldBe` 12
    monthsTilTishrei 3 `shouldBe` 24
    monthsTilTishrei 4 `shouldBe` 37
    monthsTilTishrei 20 `shouldBe` 235
  prop "rosh hashana valid weekday" $
    (`elem` [1, 2, 4, 6]) . dayOfWeek . roshHashana
  prop "greg/hebrew same weekday" $ \h ->
    let td = totalDaysFromHebrew h
        wd1 = dayOfWeek td
        d = fromHebrew h
        (_, _, wd2) = toWeekDate d
        wd2' = fromIntegral wd2 `mod` 7
     in wd1 == wd2'
  it "integral date spot check" $ do
    (toModifiedJulianDay $ fromGregorian 2009 9 26) `shouldBe`
     (toModifiedJulianDay $ fromHebrew $ HebrewDate 5770 Tishrei 8)
    dayOfWeek (roshHashana 5770) `shouldBe` 6
    roshHashana 5770 `shouldBe` totalDaysFromHebrew (HebrewDate 5770 Tishrei 1)
    dayOfWeek (totalDaysFromHebrew $ HebrewDate 5770 Tishrei 3) `shouldBe` 1
  it "individual date spot checks" $ do
    fromGregorian 1984 9 27 `shouldBe` fromHebrew (HebrewDate 5745 Tishrei 1)
    fromGregorian 1985 1 12 `shouldBe` fromHebrew (HebrewDate 5745 Tevet 19)
    fromGregorian 1986 9 8 `shouldBe` fromHebrew (HebrewDate 5746 Elul 4)
  it "year 3932" $ dayOfWeek (roshHashana 3932) `shouldBe` 2
  it "caseAnniversaryInYear" $ do
    -- Year 5770 is just the current year at time of writing
    -- Year 3 is a chaser leap year
    -- Year 4 is a leap year
    HebrewDate 5770 Tishrei 1 `shouldBe`
        anniversaryInYear 5770 (HebrewDate 1 Tishrei 1)
    HebrewDate 3 Cheshvan 29 `shouldBe`
        anniversaryInYear 3 (HebrewDate 1 Cheshvan 30)
    HebrewDate 3 Kislev 29 `shouldBe`
        anniversaryInYear 3 (HebrewDate 1 Kislev 30)
    HebrewDate 3 Adar2 1 `shouldBe`
        anniversaryInYear 3 (HebrewDate 1 Adar 1)
    HebrewDate 4 Adar 1 `shouldBe`
        anniversaryInYear 4 (HebrewDate 1 Adar1 1)
    HebrewDate 4 Adar 1 `shouldBe`
        anniversaryInYear 4 (HebrewDate 1 Adar2 1)
    HebrewDate 4 Adar 29 `shouldBe`
        anniversaryInYear 4 (HebrewDate 1 Adar1 30)
  it "caseNextAnniversary" $ do
    HebrewDate 5770 Tishrei 2 `shouldBe`
        nextAnniversary (HebrewDate 5770 Tishrei 1) (HebrewDate 1 Tishrei 2)
    HebrewDate 5771 Tishrei 2 `shouldBe`
        nextAnniversary (HebrewDate 5770 Tishrei 3) (HebrewDate 1 Tishrei 2)
    HebrewDate 5775 Adar 1 `shouldBe`
        nextAnniversary (HebrewDate 5775 Tishrei 1) (HebrewDate 5774 Adar2 1)
    HebrewDate 5775 Cheshvan 29 `shouldBe`
        nextAnniversary (HebrewDate 5775 Tishrei 1) (HebrewDate 5774 Cheshvan 30)
  prop "anniversary year works" $ \d (Years y') ->
    let y = fromIntegral y'
     in year (anniversaryInYear y d) `shouldBe` y
  it "anniversary leap year examples" $ do
    let today = toHebrew $ fromGregorian 2019 2 17
        orig = toHebrew $ fromGregorian 1956 3 4
        expected = anniversaryInYear 5779 orig
        actual = nextAnniversary today orig
    expected `shouldBe` HebrewDate 5779 Adar2 21
    actual `shouldBe` expected

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (a, b, c, d) = f a b c d

extraMonthCount :: Years -> Months
extraMonthCount i =
    case i of
        0 -> 0
        1 -> 0
        2 -> 0
        3 -> 1
        4 -> 1
        5 -> 1
        6 -> 2
        7 -> 2
        8 -> 3
        9 -> 3
        10 -> 3
        11 -> 4
        12 -> 4
        13 -> 4
        14 -> 5
        15 -> 5
        16 -> 5
        17 -> 6
        18 -> 6
        _ -> error $ "extraMonthCount: " ++ show i

monthsTilTishreiLong :: Years -> Months
monthsTilTishreiLong (Years y') =
    let (machzorim, y) = (y' - 1) `divMod` 19
        base = Months $ (y' - 1) * 12 + machzorim * 7
        extra = extraMonthCount $ Years y
     in base + extra


julianFromDate :: YearLeap -> YearType -> Month -> Date -> Julian
julianFromDate yl yt m d =
    let ml = monthLength yl yt
        months = case m of
            Tishrei -> []
            _ -> enumFromTo Tishrei (pred m)
     in d + sum (map ml months)

dayOfWeek :: TotalDays -> Weekday
dayOfWeek t =
    let (_, w) = weeksFromDays t
     in w

enumAll :: Enum e => [e]
enumAll = enumFrom $ toEnum 1

-- orphan arbitrary instances
instance Arbitrary Chalakim where
    arbitrary = fromIntegral <$> (arbitrary :: Gen Int)

instance Arbitrary Days where
    arbitrary = fromIntegral . (+ 1) . (`mod` 353)
            <$> (arbitrary :: Gen Int)

instance Arbitrary Years where
    arbitrary = fromIntegral . (+ 1) . (`mod` 6000)
            <$> (arbitrary :: Gen Int)

instance Arbitrary YearLeap where
    arbitrary = elements enumAll

instance Arbitrary YearType where
    arbitrary = elements enumAll

instance Arbitrary HebrewDate where
    arbitrary = do
        m <- elements [Tishrei, Cheshvan, Kislev, Tevet, Shevat,
                       Nissan, Iyar, Sivan, Tammuz, Av, Elul]
        y <- (+ 1) . (`mod` 6000) <$> arbitrary
        day <- (+ 1) . (`mod` 29) <$> arbitrary
        return $! HebrewDate y m day

