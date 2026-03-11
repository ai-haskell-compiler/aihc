{-# LANGUAGE OverloadedStrings #-}

-- | Validate hostnames.

module Text.Hostname
  (validHostname)
  where

import Control.Applicative
import Data.Attoparsec hiding (Parser)
import Data.Attoparsec.Combinator
import Data.Attoparsec.Types (Parser)
import Data.ByteString (ByteString)
import GHC.Word

--------------------------------------------------------------------------------
-- Exported

-- | Is the input a valid host name?
validHostname :: ByteString -> Bool
validHostname = test (host >> endOfInput)

--------------------------------------------------------------------------------
-- Parser

-- | Test the given parser on the given input.
test :: Parser ByteString b -> ByteString -> Bool
test p x = either (const False) (const True) (parseOnly p x)

-- | A host name.
host :: Parser ByteString [[[Word8]]]
host = labelStart >> many label

-- | A name part.
name :: Parser ByteString [Word8]
name = (many1 (char '-') >> many1 diglet) <|> many1 diglet

-- | A host part.
label :: Parser ByteString [[Word8]]
label = char '.' >> diglet >> many name

-- | Start of a host part.
labelStart :: Parser ByteString [[Word8]]
labelStart = diglet >> many name

-- | Match the character.
char :: Char -> Parser ByteString Word8
char c = word8 (fromIntegral (fromEnum c))

-- | ASCII letters and digits.
diglet :: Parser ByteString Word8
diglet = satisfy (flip elem (['a'..'z'] ++ ['0'..'9']) . toEnum . fromIntegral)

--------------------------------------------------------------------------------
-- Unit tests

-- | Do all tests pass?
testsPass :: Bool
testsPass = all validHostname correctTests && not (any validHostname incorrectTests)

-- | Tests that should pass.
correctTests :: [ByteString]
correctTests =
  ["a"
  ,"a.com"
  ,"a-c"
  ,"a--b"
  ,"64"
  ,"54.com"
  -- Non-alpha languages use this encoding
  ,"aaa-bbb-ccc.dooo-bar--zot"
  ,"xn--mgbh0fb.xn--kgbechtv"
  ,"xn--fsqu00a.xn--0zwm56d"
  ,"xn--fsqu00a.xn--g6w251d"
  ,"xn--hxajbheg2az3al.xn--jxalpdlp"
  ,"xn--p1b6ci4b4b3a.xn--11b5bs3a9aj6g"
  ,"xn--r8jz45g.xn--zckzah"
  ,"xn--9n2bp8q.xn--9t4b11yi5a"
  ,"xn--mgbh0fb.xn--hgbk6aj7f53bba"
  ,"xn--e1afmkfd.xn--80akhbyknj4f"
  ,"xn--zkc6cc5bi7f6e.xn--hlcj6aya9esc7a"
  ,"xn--6dbbec0c.xn--deba0ad"
  ,"xn--fdbk5d8ap9b8a8d.xn--deba0ad"]

-- | Tests that should passfail.
incorrectTests :: [ByteString]
incorrectTests =
  [""
  ,"a-"
  ,"-"
  ,"-a"
  ,"a--"
  ,"a.-"
  ,".a"
  ,".a-z"]
