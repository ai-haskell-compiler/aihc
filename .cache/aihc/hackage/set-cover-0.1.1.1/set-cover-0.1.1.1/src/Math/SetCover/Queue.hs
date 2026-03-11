module Math.SetCover.Queue (SetId(..), Methods(..), findMin) where

import Data.EnumMap (EnumMap)
import Data.EnumSet (EnumSet)


newtype SetId = SetId Int

instance Enum SetId where
   fromEnum (SetId n) = n
   toEnum = SetId


data Methods queue set =
   Methods {
      fromEnumMap :: EnumMap SetId set -> queue,
      partition :: queue -> set -> (EnumSet SetId, queue),
      difference :: queue -> EnumMap SetId set -> queue,
      findMinValue :: queue -> Maybe (set, EnumSet SetId),
      null :: queue -> Bool
   }

findMin :: Methods queue set -> queue -> Maybe (EnumSet SetId)
findMin dict = fmap snd . findMinValue dict
