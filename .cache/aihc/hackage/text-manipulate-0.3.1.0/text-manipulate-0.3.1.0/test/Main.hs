{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

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

import Data.Text (Text)
import Data.Text.Manipulate
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup
      "tests"
      [ exampleGroup
          "lowerHead"
          lowerHead
          [ "",
            "title cased phrase",
            "camelCasedPhrase",
            "pascalCasedPhrase",
            "snake_cased_phrase",
            "spinal-cased-phrase",
            "train-Cased-Phrase",
            "1-Mixed_string AOK",
            "a  double--stop__phrase",
            "hTML5",
            "είναιΥπάρχουν πολλές-Αντίθετα",
            "je_obecněÚvodní-Španěl"
          ],
        exampleGroup
          "upperHead"
          upperHead
          [ "",
            "Title cased phrase",
            "CamelCasedPhrase",
            "PascalCasedPhrase",
            "Snake_cased_phrase",
            "Spinal-cased-phrase",
            "Train-Cased-Phrase",
            "1-Mixed_string AOK",
            "A  double--stop__phrase",
            "HTML5",
            "ΕίναιΥπάρχουν πολλές-Αντίθετα",
            "Je_obecněÚvodní-Španěl"
          ],
        exampleGroup
          "takeWord"
          takeWord
          [ "",
            "Title",
            "camel",
            "Pascal",
            "snake",
            "spinal",
            "Train",
            "1",
            "a",
            "HTML5",
            "Είναι",
            "Je"
          ],
        exampleGroup
          "dropWord"
          dropWord
          [ "",
            "cased phrase",
            "CasedPhrase",
            "CasedPhrase",
            "cased_phrase",
            "cased-phrase",
            "Cased-Phrase",
            "Mixed_string AOK",
            "double-stop_phrase",
            "",
            "Υπάρχουν πολλές-Αντίθετα",
            "obecněÚvodní-Španěl"
          ],
        exampleGroup
          "stripWord"
          stripWord
          [ Nothing,
            Just "cased phrase",
            Just "CasedPhrase",
            Just "CasedPhrase",
            Just "cased_phrase",
            Just "cased-phrase",
            Just "Cased-Phrase",
            Just "Mixed_string AOK",
            Just "double-stop_phrase",
            Just "",
            Just "Υπάρχουν πολλές-Αντίθετα",
            Just "obecněÚvodní-Španěl"
          ],
        testGroup
          "splitWords"
          [],
        testGroup
          "indentLines"
          [],
        testGroup
          "toAcronym"
          [],
        testGroup
          "toOrdinal"
          [ testCase "1st" ("1st" @=? toOrdinal 1),
            testCase "2nd" ("2nd" @=? toOrdinal 2),
            testCase "3rd" ("3rd" @=? toOrdinal 3),
            testCase "4th" ("4th" @=? toOrdinal 4),
            testCase "5th" ("5th" @=? toOrdinal 5),
            testCase "21st" ("21st" @=? toOrdinal 21),
            testCase "33rd" ("33rd" @=? toOrdinal 33),
            testCase "102nd" ("102nd" @=? toOrdinal 102),
            testCase "203rd" ("203rd" @=? toOrdinal 203)
          ],
        exampleGroup
          "toCamel"
          toCamel
          [ "",
            "titleCasedPhrase",
            "camelCasedPhrase",
            "pascalCasedPhrase",
            "snakeCasedPhrase",
            "spinalCasedPhrase",
            "trainCasedPhrase",
            "1MixedStringAOK",
            "aDoubleStopPhrase",
            "hTML5",
            "είναιΥπάρχουνΠολλέςΑντίθετα",
            "jeObecněÚvodníŠpaněl"
          ],
        exampleGroup
          "toPascal"
          toPascal
          [ "",
            "TitleCasedPhrase",
            "CamelCasedPhrase",
            "PascalCasedPhrase",
            "SnakeCasedPhrase",
            "SpinalCasedPhrase",
            "TrainCasedPhrase",
            "1MixedStringAOK",
            "ADoubleStopPhrase",
            "HTML5",
            "ΕίναιΥπάρχουνΠολλέςΑντίθετα",
            "JeObecněÚvodníŠpaněl"
          ],
        exampleGroup
          "toSnake"
          toSnake
          [ "",
            "title_cased_phrase",
            "camel_cased_phrase",
            "pascal_cased_phrase",
            "snake_cased_phrase",
            "spinal_cased_phrase",
            "train_cased_phrase",
            "1_mixed_string_aok",
            "a_double_stop_phrase",
            "html5",
            "είναι_υπάρχουν_πολλές_αντίθετα",
            "je_obecně_úvodní_španěl"
          ],
        exampleGroup
          "toSpinal"
          toSpinal
          [ "",
            "title-cased-phrase",
            "camel-cased-phrase",
            "pascal-cased-phrase",
            "snake-cased-phrase",
            "spinal-cased-phrase",
            "train-cased-phrase",
            "1-mixed-string-aok",
            "a-double-stop-phrase",
            "html5",
            "είναι-υπάρχουν-πολλές-αντίθετα",
            "je-obecně-úvodní-španěl"
          ],
        exampleGroup
          "toTrain"
          toTrain
          [ "",
            "Title-Cased-Phrase",
            "Camel-Cased-Phrase",
            "Pascal-Cased-Phrase",
            "Snake-Cased-Phrase",
            "Spinal-Cased-Phrase",
            "Train-Cased-Phrase",
            "1-Mixed-String-AOK",
            "A-Double-Stop-Phrase",
            "HTML5",
            "Είναι-Υπάρχουν-Πολλές-Αντίθετα",
            "Je-Obecně-Úvodní-Španěl"
          ],
        exampleGroup
          "toTitle"
          toTitle
          [ "",
            "Title Cased Phrase",
            "Camel Cased Phrase",
            "Pascal Cased Phrase",
            "Snake Cased Phrase",
            "Spinal Cased Phrase",
            "Train Cased Phrase",
            "1 Mixed String AOK",
            "A Double Stop Phrase",
            "HTML5",
            "Είναι Υπάρχουν Πολλές Αντίθετα",
            "Je Obecně Úvodní Španěl"
          ]
      ]

exampleGroup :: (Show a, Eq a) => TestName -> (Text -> a) -> [a] -> TestTree
exampleGroup n f = testGroup n . zipWith run reference
  where
    run (c, x) y = testCase c (f x @=? y)

    reference =
      [ ("Empty", ""),
        ("Title", "Title cased phrase"),
        ("Camel", "camelCasedPhrase"),
        ("Pascal", "PascalCasedPhrase"),
        ("Snake", "snake_cased_phrase"),
        ("Spinal", "spinal-cased-phrase"),
        ("Train", "Train-Cased-Phrase"),
        ("Mixed", "1-Mixed_string AOK"),
        ("Double Stop", "a  double--stop__phrase"),
        ("Acronym", "HTML5"),
        ("Greek", "ΕίναιΥπάρχουν πολλές-Αντίθετα"),
        ("Czech", "Je_obecněÚvodní-Španěl")
      ]
