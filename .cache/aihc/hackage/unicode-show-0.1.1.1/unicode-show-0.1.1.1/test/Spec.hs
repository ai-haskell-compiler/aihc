{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Monad (when)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Text.Read             (readMaybe)

import           Text.Show.Unicode

newtype T試6験 = Å4 { すけろく :: String } deriving (Eq, Ord, Show, Read)
data T試7験 = String :@\& String deriving (Eq, Ord, Show, Read)
data T試8験 = String :＠\& String deriving (Eq, Ord, Show, Read)
data T試9験 = String :\&＠\& String deriving (Eq, Ord, Show, Read)
data T試10験 = String :\&\& String deriving (Eq, Ord, Show, Read)

ushowTo :: Show a => a -> String -> Spec
ushowTo f t = it ("ushow " ++ show f ++ " == " ++ t) $ ushow f `shouldBe` t

-- | check `read . ushow == id` when `read . show == id`.
-- The reason why we don't test if the show fails is that older versions may fail to read the result of the show,
-- which cannot be handled at the library level, so we exclude it.
-- ==> is not used because it will cause an error if there is no test case that can be executed.
readUShowIsIdWhenOkPrelude :: (Eq a, Show a, Read a) => a -> Expectation
readUShowIsIdWhenOkPrelude v =
  when preludeOk ushowOk
  where preludeOk = readMaybe (show v) == Just v
        ushowOk = read (ushow v) `shouldBe` v

spec :: Spec
spec = do
  describe "individual representations test" $ do
    "صباح الخير" `ushowTo` "\"صباح الخير\""
    "😆💕>λ\\=🐘" `ushowTo`  "\"😆💕>λ\\\\=🐘\""
    "漢6" `ushowTo` "\"漢6\""
    "\32\&7" `ushowTo` "\" 7\""
    "改\n行" `ushowTo` "\"改\\n行\""
    "下一站\na\ri\ta国际机场" `ushowTo` "\"下一站\\na\\ri\\ta国际机场\""
    "\SOH\SO\&H" `ushowTo` "\"\\SOH\\SO\\&H\""
    "\"\"" `ushowTo` "\"\\\"\\\"\""
    "\"" `ushowTo` "\"\\\"\""
    "\"unterminated" `ushowTo` "\"\\\"unterminated\""

  describe "read . ushow == id" $ do
    prop "read . ushow == id, for String" $
      \str -> read (ushow str) `shouldBe` (str :: String)

    prop "read . ushow == id, for Char" $
      \x -> read (ushow x) `shouldBe` (x :: Char)

    prop "read . ushow == id, for [(Char,())]" $
      \x -> read (ushow x) `shouldBe` (x :: [(Char,())])

    prop "read . read . ushow . ushow == id, for String" $
      \str -> read (read $ ushow $ ushow str) `shouldBe` (str :: String)

    prop "read . ushow == id, for some crazy Unicode type: T試6験" $
      \str -> readUShowIsIdWhenOkPrelude $ Å4 str

    prop "read . ushow == id, for some crazy Unicode type: T試7験" $
      \a b -> readUShowIsIdWhenOkPrelude $ a :@\& b

    prop "read . ushow == id, for some crazy Unicode type: T試8験" $
      \a b -> readUShowIsIdWhenOkPrelude $ a :＠\& b

    prop "read . ushow == id, for some crazy Unicode type: T試9験" $
      \a b -> readUShowIsIdWhenOkPrelude $ a :\&＠\& b

    prop "read . ushow == id, for some crazy Unicode type: T試10験" $
      \a b -> readUShowIsIdWhenOkPrelude $ a :\&\& b

    prop "read . ushow == id, for compound type" $
      \str -> read (ushow str) `shouldBe` (str :: Either [String] (String,String))

main :: IO ()
main = hspec spec
