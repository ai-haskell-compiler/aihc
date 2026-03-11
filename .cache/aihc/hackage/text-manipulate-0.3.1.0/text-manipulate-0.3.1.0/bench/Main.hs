{-# LANGUAGE OverloadedStrings #-}

-- Module      : Main
-- Copyright   : (c) 2014-2020 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Main (main) where

import Criterion
import Criterion.Main
import Data.List (intersperse)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Manipulate

main :: IO ()
main =
  defaultMain
    [ bgroup
        "Data.Text"
        [ bench "takeWord" $
            whnf (Text.takeWhile (not . isWordBoundary)) phrase,
          bench "toCamel" $
            whnf (lowerHead . mconcat . group Text.toTitle) phrase,
          bench "toPascal" $
            whnf (upperHead . mconcat . group Text.toTitle) phrase,
          bench "toSnake" $
            whnf (mconcat . intersperse "_" . group Text.toLower) phrase,
          bench "toSpinal" $
            whnf (mconcat . intersperse "-" . group Text.toLower) phrase,
          bench "toTrain" $
            whnf (mconcat . intersperse "-" . group Text.toTitle) phrase
        ],
      bgroup
        "Data.Text.Case"
        [ bench "takeWord" $ whnf takeWord phrase,
          bench "toCamel" $ whnf toCamel phrase,
          bench "toPacal" $ whnf toPascal phrase,
          bench "toSnake" $ whnf toSnake phrase,
          bench "toSpinal" $ whnf toSpinal phrase,
          bench "toTrain" $ whnf toTrain phrase
        ]
    ]

phrase :: Text
phrase = "Supercalifragilistic, world! This-is  A multipleDelimiter_String"

group :: (Text -> Text) -> Text -> [Text]
group f =
  map (f . Text.dropWhile isBoundary)
    . Text.groupBy (const (not . isWordBoundary))
