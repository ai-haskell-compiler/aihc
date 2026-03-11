{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ROC.ID.Location
  ( Location (..)
  , parseLocation
  , printLocation
  , randomLocation
  ) where

import Control.Monad.Random.Class
    ( MonadRandom (..) )
import Data.Text
    ( Text )
import GHC.Generics
    ( Generic )
import ROC.ID.Language
    ( Language (..) )
import ROC.ID.Utilities

-- | A location, encodable within an ROC identification number.
--
-- To generate the name of a 'Location', use the 'printLocation' function.
--
-- To parse a 'Location', use the 'parseLocation' function.
--
-- To generate a random 'Location', use the 'randomLocation' function.
--
data Location
  = A -- ^ 臺北市 Taipei City
  | B -- ^ 臺中市 Taichung City
  | C -- ^ 基隆市 Keelung City
  | D -- ^ 臺南市 Tainan City
  | E -- ^ 高雄市 Kaohsiung City
  | F -- ^ 新北市 New Taipei City
  | G -- ^ 宜蘭縣 Yilan County
  | H -- ^ 桃園市 Taoyuan City
  | I -- ^ 嘉義市 Chiayi City
  | J -- ^ 新竹縣 Hsinchu County
  | K -- ^ 苗栗縣 Miaoli County
  | L -- ^ 臺中縣 Taichung County
  | M -- ^ 南投縣 Nantou County
  | N -- ^ 彰化縣 Changhua County
  | O -- ^ 新竹市 Hsinchu City
  | P -- ^ 雲林縣 Yunlin County
  | Q -- ^ 嘉義縣 Chiayi County
  | R -- ^ 臺南縣 Pingtung County
  | S -- ^ 高雄縣 Kaohsiung County
  | T -- ^ 屏東縣 Pingtung County
  | U -- ^ 花蓮縣 Hualien County
  | V -- ^ 臺東縣 Taitung County
  | W -- ^ 金門縣 Kinmen County
  | X -- ^ 澎湖縣 Penghu County
  | Y -- ^ 陽明山 Yangmingshan
  | Z -- ^ 連江縣 Lienchiang County
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

-- | Parse the specified uppercase alphabetic character as a 'Location'.
--
-- Returns 'Nothing' if the specified character is not an uppercase alphabetic
-- character.
--
parseLocation :: Char -> Maybe Location
parseLocation c = maybeRead [c]

-- | Pretty-print the specified 'Location'.
printLocation :: Language -> Location -> Text
printLocation = \case
  English -> printLocationEnglish
  Chinese -> printLocationChinese

printLocationChinese :: Location -> Text
printLocationChinese = \case
  A -> "臺北市"
  B -> "臺中市"
  C -> "基隆市"
  D -> "臺南市"
  E -> "高雄市"
  F -> "新北市"
  G -> "宜蘭縣"
  H -> "桃園市"
  I -> "嘉義市"
  J -> "新竹縣"
  K -> "苗栗縣"
  L -> "臺中縣"
  M -> "南投縣"
  N -> "彰化縣"
  O -> "新竹市"
  P -> "雲林縣"
  Q -> "嘉義縣"
  R -> "臺南縣"
  S -> "高雄縣"
  T -> "屏東縣"
  U -> "花蓮縣"
  V -> "臺東縣"
  W -> "金門縣"
  X -> "澎湖縣"
  Y -> "陽明山"
  Z -> "連江縣"

printLocationEnglish :: Location -> Text
printLocationEnglish = \case
  A -> "Taipei City"
  B -> "Taichung City"
  C -> "Keelung City"
  D -> "Tainan City"
  E -> "Kaohsiung City"
  F -> "New Taipei City"
  G -> "Yilan County"
  H -> "Taoyuan City"
  I -> "Chiayi City"
  J -> "Hsinchu County"
  K -> "Miaoli County"
  L -> "Taichung County"
  M -> "Nantou County"
  N -> "Changhua County"
  O -> "Hsinchu City"
  P -> "Yunlin County"
  Q -> "Chiayi County"
  R -> "Pingtung County"
  S -> "Kaohsiung County"
  T -> "Pingtung County"
  U -> "Hualien County"
  V -> "Taitung County"
  W -> "Kinmen County"
  X -> "Penghu County"
  Y -> "Yangmingshan"
  Z -> "Lienchiang County"

-- | Generate a random 'Location'.
--
randomLocation :: MonadRandom m => m Location
randomLocation = randomBoundedEnum

