module Main where

import qualified Data.Text as Text
import qualified DeferredFolds.Unfoldr as Unfoldr
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.QuickCheck
import Prelude

main :: IO ()
main =
  defaultMain
    $ testGroup "All"
    $ [ testProperty "List roundtrip" $ \(list :: [Int]) ->
          list === toList (Unfoldr.foldable list),
        testProperty "take" $ \(list :: [Int], amount) ->
          take amount list
            === toList (Unfoldr.take amount (Unfoldr.foldable list)),
        testProperty "takeWhile odd" $ \(list :: [Int]) ->
          takeWhile odd list
            === toList (Unfoldr.takeWhile odd (Unfoldr.foldable list)),
        testProperty "intersperse" $ \(list :: [Char]) ->
          intersperse ',' list
            === toList (Unfoldr.intersperse ',' (Unfoldr.foldable list)),
        testProperty "textChars" $ \(text :: Text) ->
          Text.unpack text
            === toList (Unfoldr.textChars text),
        testProperty "textWords" $ \(text :: Text) ->
          Text.words text
            === toList (Unfoldr.textWords text),
        testProperty "trimWhitespace 1" $ \(text :: Text) ->
          let words =
                Text.words text
              run =
                fromString . toList . Unfoldr.trimWhitespace . Unfoldr.textChars
              spacedInput =
                Text.map (\c -> if isSpace c then ' ' else c) text
              newlinedInput =
                Text.map (\c -> if isSpace c then '\n' else c) text
           in Text.unwords words === run spacedInput
                .&&. Text.intercalate "\n" words === run newlinedInput,
        testProperty "trimWhitespace 2" $ \(text :: Text) ->
          let isNewline c =
                c == '\n' || c == '\r'
              isSpaceButNotNewline c =
                isSpace c && not (isNewline c)
              expected =
                text
                  & Text.split isNewline
                  & fmap Text.strip
                  & filter (not . Text.null)
                  & Text.intercalate "\n"
                  & Text.split isSpaceButNotNewline
                  & filter (not . Text.null)
                  & Text.intercalate " "
              run =
                fromString . toList . Unfoldr.trimWhitespace . Unfoldr.textChars
           in expected === run text
      ]
