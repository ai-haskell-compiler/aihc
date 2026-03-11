-- | Locale representing Vietnamese usage. For use with "Data.Time.Format".
--
-- Besides a locale similar to other implementation, this module also provides
-- an alternate locale perfered by native speakers.
--
-- Some common date time expressions are also included.

module Data.Time.Locale.Vietnamese (
    vietnameseTimeLocale
  , vietnameseNativeTimeLocale
  , documentDateFormat
  , longDateFormat
  , shortDateFormat
  , shortDateFormatAlt
  , fullHourFormat
  , hourFormat
) where

import Data.Time

-- | Locale representing Vietnames usage.
--
-- Similar to how it is commonly implemented in other libraries and languages.
--
-- References:
--
-- * http://www.localeplanet.com/java/vi-VN/index.html
--
-- * https://github.com/moment/moment/blob/develop/locale/vi.js
vietnameseTimeLocale :: TimeLocale
vietnameseTimeLocale = TimeLocale
  { wDays = [ ("Chủ Nhật", "CN"), ("Thứ Hai", "Th 2")
            , ("Thứ Ba", "Th 3"), ("Thứ Tư", "Th 4")
            , ("Thứ Năm", "Th 5"),  ("Thứ Sáu", "Th 6")
            , ("Thứ Bảy", "Th 7")
            ]
  , months = [ ("Tháng một", "thg 1"), ("Tháng hai", "thg 2")
             , ("Tháng ba", "thg 3"), ("Tháng tư", "thg 4")
             , ("Tháng năm", "thg 5"), ("Tháng sáu", "thg 6")
             , ("Tháng bảy", "thg 7"), ("Tháng tám", "thg 8")
             , ("Tháng chín", "thg 9"), ("Tháng mười", "thg 10")
             , ("Tháng mười một", "thg 11"), ("Tháng mười hai", "thg 12")
             ]
  , amPm = ("SA", "CH")
  , dateTimeFmt = "%a %b %e %H:%M:%S %Z %Y"
  , dateFmt = "%d/%m/%y"
  , timeFmt = "%H:%M:%S"
  , time12Fmt = "%I:%M:%S %p"
  , knownTimeZones = [ TimeZone 0 False "GMT"
                     , TimeZone (7 * 60) False "ICT"
                     ]
  }

-- | Locale representing Vietnames usage.
--
-- Native speaker perference.
--
-- References:
--
-- * https://github.com/moment/moment/blob/develop/locale/vi.js
vietnameseNativeTimeLocale :: TimeLocale
vietnameseNativeTimeLocale = vietnameseTimeLocale
  { wDays = [ ("Chủ Nhật", "CN"), ("Thứ Hai", "T2")
            , ("Thứ Ba", "T3"), ("Thứ Tư", "T4")
            , ("Thứ Năm", "T5"),  ("Thứ Sáu", "T6")
            , ("Thứ Bảy", "T7")
            ]
  , months = [ ("Tháng 1", "1"), ("Tháng 2", "2")
             , ("Tháng 3", "3"), ("Tháng 4", "4")
             , ("Tháng 5", "5"), ("Tháng 6", "6")
             , ("Tháng 7", "7"), ("Tháng 8", "8")
             , ("Tháng 9", "9"), ("Tháng 10", "10")
             , ("Tháng 11", "11"), ("Tháng 12", "12")
             ]
  }

-- | Date format used in formal documents, example: 'Ngày 20 tháng 11 năm 2017'.
documentDateFormat :: String
documentDateFormat = "Ngày %e %#B năm %_Y"

-- | Full date format, example: 'Thứ sáu, ngày 20 tháng 11 năm 2017'.
longDateFormat :: String
longDateFormat = "%A, ngày %e %#B năm %_Y"

-- | Short date format, example: '20/11/2017'.
shortDateFormat :: String
shortDateFormat = "%d/%m/%_y"

-- | Alternative short date format, example: '20-11-2017'.
shortDateFormatAlt :: String
shortDateFormatAlt = "%d-%m-%_y"

-- | Common time format, example: '2 giờ 30 phút chiều'.
fullHourFormat :: String
fullHourFormat = "%l giờ %-M phút %p"

-- | Common time format, example: '2 giờ chiều'.
hourFormat :: String
hourFormat = "%l giờ %p"
