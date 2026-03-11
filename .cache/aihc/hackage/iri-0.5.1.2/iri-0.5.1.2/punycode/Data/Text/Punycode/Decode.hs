{-# LANGUAGE DeriveDataTypeable #-}

module Data.Text.Punycode.Decode (PunycodeDecodeException (..), decode) where

import Control.Exception.Base
import qualified Data.ByteString as BS
import Data.Char
import Data.Serialize hiding (decode)
import qualified Data.Text as T
import Data.Text.Punycode.Shared
import Data.Typeable
import Data.Word

data PunycodeDecodeException
  = GenericDecodeException
  | InternalStringTooShort
  | InputTooShort
  | RightOfHyphenShouldBeAlphanumeric
  | LeftOfHyphenShouldBeBasic
  | CantStartWithDash
  | InvalidCodePoint
  deriving (Eq, Show, Typeable)

instance Exception PunycodeDecodeException

-- | Decode a string into its unicode form
decode :: BS.ByteString -> Either PunycodeDecodeException T.Text
decode input
  | input == BS.pack [45, 45] = Right $ T.pack "-"
  | not (BS.null input) && BS.length (BS.filter (== 45) input) == 1 && BS.head input == 45 = Left CantStartWithDash
  | T.any (not . isExtendedBasic) before = Left LeftOfHyphenShouldBeBasic
  | otherwise = case runGet (inner2 initial_n 0 initial_bias before) after of
      Right out -> out
      Left _ -> Left InputTooShort
  where
    (before, after)
      | BS.any f input = (T.pack $ map (chr . fromIntegral) $ BS.unpack $ BS.init b1, a1)
      | otherwise = (T.empty, input)
    f = (== (fromIntegral $ ord '-'))
    (b1, a1) = BS.breakEnd f input

inner2 :: Int -> Int -> Int -> T.Text -> Get (Either PunycodeDecodeException T.Text)
inner2 n oldi bias output = do
  b <- isEmpty
  helper b
  where
    helper False = do
      i <- inner base 1 oldi bias
      helper' i
      where
        helper' Nothing = return $ Left RightOfHyphenShouldBeAlphanumeric
        helper' (Just i) = case output' of
          Right output'' -> inner2 n' (i' + 1) bias' output''
          Left err -> return $ Left err
          where
            bias' = adapt (i - oldi) (T.length output + 1) (oldi == 0)
            n' = n + i `div` (T.length output + 1)
            i' = i `mod` (T.length output + 1)
            output' = insertInto output n' i'
    helper True = return $ Right output

inner :: Int -> Int -> Int -> Int -> Get (Maybe Int)
inner k w i bias = do
  word8 <- getWord8
  helper $ word8ToDigit word8
  where
    helper Nothing = return Nothing
    helper (Just digit)
      | digit < t = return $ Just i'
      | otherwise = inner (k + base) w' i' bias
      where
        w' = w * (base - t)
        i' = i + digit * w
        t
          | k <= bias + tmin = tmin
          | k >= bias + tmax = tmax
          | otherwise = k - bias

insertInto :: T.Text -> Int -> Int -> Either PunycodeDecodeException T.Text
insertInto input n i
  | T.length input < i = Left InternalStringTooShort
  | otherwise = case n' of
      Just n'' -> Right $ T.concat [T.take i input, T.singleton n'', T.drop i input]
      Nothing -> Left InvalidCodePoint
  where
    n' = safeChr n

safeChr :: Int -> Maybe Char
safeChr x
  | x >= 0 && x <= fromEnum (maxBound :: Char) = Just $ chr x
  | otherwise = Nothing

word8ToDigit :: Word8 -> Maybe Int
word8ToDigit = helper . fromIntegral
  where
    helper word8
      | word8 >= ord 'a' && word8 <= ord 'z' = Just $ word8 - (ord 'a')
      | word8 >= ord 'A' && word8 <= ord 'Z' = Just $ word8 - (ord 'A')
      | word8 >= ord '0' && word8 <= ord '9' = Just $ 26 + word8 - (ord '0')
      | otherwise = Nothing

isExtendedBasic :: Char -> Bool
isExtendedBasic x
  | isBasic x = True
  | ord x == 128 = True
  | otherwise = False
