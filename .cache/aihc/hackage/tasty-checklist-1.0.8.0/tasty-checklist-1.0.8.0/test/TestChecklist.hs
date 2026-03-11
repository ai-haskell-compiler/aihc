{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Data.Parameterized.Context ( pattern Empty, pattern (:>) )
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.Checklist
import Test.Tasty.ExpectedFailure
import Test.Tasty.HUnit


main :: IO ()
main = defaultMain $ testGroup "Checklist testing"
       [
         expectFailBecause "3 failed checks" $
         testCase "simple checklist" $
         withChecklist "simple" $ do
           let tst :: Int -> Bool
               tst = (> 3)
           check "one" tst 1
           check "two" tst 2
           check "five" tst 5
           check "three" tst 3
           check "four" tst 4

       , expectFailBecause "2 failed checks" $
         testCase "simple checklist with retraction" $
         withChecklist "simple retracted" $ do
           let tst :: Int -> Bool
               tst = (> 3)
           check "one" tst 1
           check "two" tst 2
           check "five" tst 5
           check "three" tst 3
           check "four" tst 4
           discardCheck "two"

       , expectFailBecause "3 failed checks and assert" $
         testCase "simple checklist and assert" $
         withChecklist "simple" $ do
           let tst :: Int -> Bool
               tst = (> 3)
           check "one" tst 1
           check "two" tst 2
           check "five" tst 5
           check "three" tst 3
           check "four" tst 4
           3 @=? (4 :: Int)

       , testCase "someFun 7 result is good" $
         -- everything should pass, no check failures
         withChecklist "someFun 7" $
         someFun 7 `checkValues`
         (Empty
          :> Val "foo" foo 42
          :> Val "baz" baz "The answer to the universe"
          :> Val "shown" show "The answer to the universe is 42!"
          :> Val "odd answer" oddAnswer False
         )

       , expectFailBecause "2 values don't match" $
         testCase "someFun 3 result" $
         withChecklist "someFun" $
         someFun 3 `checkValues`
         (Empty
          :> Val "foo" foo 42
          :> Val "baz" baz "The answer to the universe"
          :> Val "shown" show "The answer to the universe is 42!"
          :> Val "odd answer" oddAnswer False
         )

       , expectFailBecause "assertion" $
         testCase "normal assert failure" $
         withChecklist "asserts" $ do
           3 @=? (5 :: Int)

       , expectFailBecause "2 values don't match and assertion" $
         testCase "someFun 3 result and assert" $
         withChecklist "someFun" $ do
           someFun 3 `checkValues`
             (Empty
              :> Val "foo" foo 42
              :> Val "baz" baz "The answer to the universe"
              :> Val "shown" show "The answer to the universe is 42!"
              :> Val "odd answer" oddAnswer False
              :> Got "even answer" (not . oddAnswer)
             )
           3 @=? (5 :: Int)

       , testCase "object w/o Show is OK" $
         -- The test object has a TestShow instance but no Show
         -- instance.  The test should pass, no checks or failures
         withChecklist "opaque object" $
         genOpaque `checkValues`
         (Empty
          :> Val "displayed" display "[[19]]"
          :> Val "answer" answer 19
          :> Val "revealed" reveal 19
          :> Val "the answer" answer 19
         )

       , expectFailBecause "revealed test check fails" $
         -- The test object has a TestShow but no Show
         testCase "object w/o Show bad comparison" $
         withChecklist "opaque object bad expected" $
         genOpaque `checkValues`
         (Empty
          :> Val "displayed" display "[[19]]"
          :> Val "answer" answer 19
          :> Val "revealed" reveal 18
          :> Val "the answer" answer 19
         )

       ----------------------------------------

       , testCase "Compare identical multi-line results" $
         withChecklist "multi-line" $
         let result = fmap (T.replace " " " ") soliloquy
         in result `checkValues`
            (Empty
             :> Val "length" length (length soliloquy)
             :> Val "word count" (length . T.words . T.unlines) 114
             :> Observe "unchanged" id soliloquy
              (\expect actual ->
                 multiLineDiff (T.unlines expect) (T.unlines actual))
            )

       , expectFailBecause "there are changes" $
         testCase "Compare changed multi-line results" $
         withChecklist "multi-line" $
         let result = fmap (T.replace "  " " ") soliloquy
         in result `checkValues`
            (Empty
             :> Val "length" length (length soliloquy)
             :> Val "word count" (length . T.words . T.unlines) 113
             :> Observe "unchanged" id soliloquy
              (\expect actual ->
                 multiLineDiff (T.unlines expect) (T.unlines actual))
            )
       ]

----------------------------------------------------------------------

data Struct = MyStruct { foo :: Int
                       , bar :: Char
                       , baz :: String }

instance Show Struct where
   show s = baz s <> " is " <> (show $ foo s) <> (bar s : [])

instance TestShow Struct  -- uses the Show instance

someFun :: Int -> Struct
someFun n = MyStruct (n * 6)
              (if n * 6 == 42 then '!' else '?')
              "The answer to the universe"

oddAnswer :: Struct -> Bool
oddAnswer = odd . foo

----------------------------------------------------------------------

data Opaque = Hidden { answer :: Int }

genOpaque :: Opaque
genOpaque = Hidden 19

reveal :: Opaque -> Int
reveal = answer

display :: Opaque -> String
display o = "[[" <> show (answer o) <> "]]"

-- Note that Opaque doesn't have a standard Show instance, but a
-- TestShow can be provided to suffice for testing.

instance TestShow Opaque where
  testShow = display


----------------------------------------------------------------------

soliloquy :: [T.Text]
soliloquy =
  [ "To be, or not to be--that is the question:"
  , "Whether 'tis nobler in the mind to suffer"
  , "The slings and arrows of outrageous fortune"
  , "Or to take arms against a sea of troubles"
  , "And by opposing end them.  To die, to sleep--"
  , "No more--and by a sleep to say we end"
  , "The heartache, and the thousand natural shocks"
  , "That flesh is heir to. 'Tis a consummation"
  , "Devoutly to be wished.  To die, to sleep--"
  , "To sleep--perchance to dream: ay, there's the rub,"
  , "For in that sleep of death what dreams may come"
  , "When we have shuffled off this mortal coil,"
  , "Must give us pause.  There's the respect"
  , "That makes calamity of so long life."
  ]

instance TestShow [T.Text] where testShow = show . T.unlines
