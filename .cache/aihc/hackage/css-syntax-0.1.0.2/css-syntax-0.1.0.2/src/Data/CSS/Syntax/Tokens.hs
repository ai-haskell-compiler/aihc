{-# LANGUAGE OverloadedStrings, RankNTypes, PatternSynonyms, ViewPatterns,
             BangPatterns, MagicHash #-}

module Data.CSS.Syntax.Tokens
    ( Token(..)
    , NumericValue(..)
    , HashFlag(..)
    , Unit

    , tokenize
    , serialize
    ) where


import           Control.Applicative
import           Control.Monad

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import           Data.Monoid
import           Data.Char
import           Data.Scientific
import           Numeric

import           Prelude

import           Data.Text.Internal (Text(..))
import           Data.Text.Unsafe (inlineInterleaveST)
import qualified Data.Text.Array as A
import           Control.Monad.ST (ST)
import           GHC.Exts
import           GHC.Base (unsafeChr)
import           GHC.Word (Word8(..))
import           Data.Bits


data Token
    = Whitespace

    | CDO -- CommentDelimiterOpen
    | CDC -- CommentDelimiterClose

    | Comma
    | Colon
    | Semicolon

    | LeftParen
    | RightParen
    | LeftSquareBracket
    | RightSquareBracket
    | LeftCurlyBracket
    | RightCurlyBracket

    | SuffixMatch
    | SubstringMatch
    | PrefixMatch
    | DashMatch
    | IncludeMatch

    | Column

    | String !Text
    | BadString

    | Number !Text !NumericValue
    | Percentage !Text !NumericValue
    | Dimension !Text !NumericValue !Unit

    | Url !Text
    | BadUrl

    | Ident !Text

    | AtKeyword !Text

    | Function !Text

    | Hash !HashFlag !Text

    | Delim !Char

    deriving (Show, Eq)


data NumericValue
    = NVInteger !Integer   -- ^ number without dot '.' or exponent 'e'
    | NVNumber !Scientific -- ^ number with dot '.' or exponent 'e'
    deriving (Show, Eq)

data HashFlag = HId | HUnrestricted
    deriving (Show, Eq)

type Unit = Text



-- Tokenization
-------------------------------------------------------------------------------


-- | Parse a 'Text' into a list of 'Token's.
--
-- https://drafts.csswg.org/css-syntax/#tokenization

tokenize :: Text -> [Token]
tokenize = parseTokens . preprocessInputStream



-- | Before sending the input stream to the tokenizer, implementations must
-- make the following code point substitutions: (see spec)
--
-- https://drafts.csswg.org/css-syntax/#input-preprocessing

preprocessInputStream :: Text -> Text
preprocessInputStream t0@(Text _ _ len) = withNewA (len*3) $ \ dst -> do
    let go t d = case t of
            '\x0D' :. '\x0A' :. t' ->
                put '\x0A' t'
            '\x0D' :. t' ->
                put '\x0A' t'
            '\x0C' :. t' ->
                put '\x0A' t'
            '\x00' :. t' -> do
                d' <- writeFFFD dst d
                go t' d'
            c :. t' ->
                put c t'
            _ ->
                return d
            where put x t' = do
                      write dst d x
                      go t' (d + 1)
    go t0 0


-- Low level utilities
-------------------------------------------------------------------------------

pattern (:.) :: Char -> Text -> Text
pattern x :. xs <- (uncons -> Just (x, xs))

infixr 5 :.

-- | uncons first Word8 from Text without trying to decode UTF-8 sequence
uncons :: Text -> Maybe (Char, Text)
uncons (Text src offs len)
    | len <= 0 = Nothing
    | otherwise =
      Just (w2c (A.unsafeIndex src offs), Text src (offs+1) (len-1))
{-# INLINE uncons #-}

-- | write replacement character
writeFFFD :: A.MArray s -> Int -> ST s Int
writeFFFD dst d = writeChar dst d '\xFFFD'

-- | write 8bit character
write :: A.MArray s -> Int -> Char -> ST s ()
write dst d x = A.unsafeWrite dst d (c2w x)
{-# INLINE write #-}

-- | write character that could have more than 8bit
-- code from Data.Text.Internal.Unsafe.Char.unsafeWrite
writeChar :: A.MArray s -> Int -> Char -> ST s Int
writeChar marr i c = case utf8Length c of
    1 -> do
        let n0 = intToWord8 (ord c)
        A.unsafeWrite marr i n0
        return (i+1)
    2 -> do
        let (n0, n1) = ord2 c
        A.unsafeWrite marr i     n0
        A.unsafeWrite marr (i+1) n1
        return (i+2)
    3 -> do
        let (n0, n1, n2) = ord3 c
        A.unsafeWrite marr i     n0
        A.unsafeWrite marr (i+1) n1
        A.unsafeWrite marr (i+2) n2
        return (i+3)
    _ -> do
        let (n0, n1, n2, n3) = ord4 c
        A.unsafeWrite marr i     n0
        A.unsafeWrite marr (i+1) n1
        A.unsafeWrite marr (i+2) n2
        A.unsafeWrite marr (i+3) n3
        return (i+4)
{-# INLINE writeChar #-}

utf8Length :: Char -> Int
utf8Length (C# c) = I# ((1# +# geChar# c (chr# 0x80#)) +# (geChar# c (chr# 0x800#) +# geChar# c (chr# 0x10000#)))
{-# INLINE utf8Length #-}

ord2 :: Char -> (Word8,Word8)
ord2 c = (x1,x2)
    where
      n  = ord c
      x1 = intToWord8 $ (n `shiftR` 6) + 0xC0
      x2 = intToWord8 $ (n .&. 0x3F)   + 0x80
{-# INLINE ord2 #-}

ord3 :: Char -> (Word8,Word8,Word8)
ord3 c = (x1,x2,x3)
    where
      n  = ord c
      x1 = intToWord8 $ (n `shiftR` 12) + 0xE0
      x2 = intToWord8 $ ((n `shiftR` 6) .&. 0x3F) + 0x80
      x3 = intToWord8 $ (n .&. 0x3F) + 0x80
{-# INLINE ord3 #-}

ord4 :: Char -> (Word8,Word8,Word8,Word8)
ord4 c = (x1,x2,x3,x4)
    where
      n  = ord c
      x1 = intToWord8 $ (n `shiftR` 18) + 0xF0
      x2 = intToWord8 $ ((n `shiftR` 12) .&. 0x3F) + 0x80
      x3 = intToWord8 $ ((n `shiftR` 6) .&. 0x3F) + 0x80
      x4 = intToWord8 $ (n .&. 0x3F) + 0x80
{-# INLINE ord4 #-}

intToWord8 :: Int -> Word8
intToWord8 = fromIntegral


type Writer' s = (A.MArray s -> Int -> ST s Int, Text)
type Writer s = A.MArray s -> Int -> ST s (Int, Text)

-- | no-op for convenient pattern matching
w2c :: Word8 -> Char
w2c = unsafeChr . fromIntegral
{-# INLINE w2c #-}

c2w :: Char -> Word8
c2w = fromIntegral . ord
{-# INLINE c2w #-}

withNewA :: Int -> (forall s . A.MArray s -> ST s Int) -> Text
withNewA len act = Text a 0 l
    where (a, l) = A.run2 $ do
              dst <- A.new len
              dLen <- act dst
              return (dst, dLen)


-- Serialization
-------------------------------------------------------------------------------


-- | Serialize a list of 'Token's back into 'Text'.
--
-- Serialization "round-trips" with parsing:
--
--   tokenize (serialize (tokenize s)) == tokenize s
--
-- https://drafts.csswg.org/css-syntax/#serialization


serialize :: [Token] -> Text
serialize = TL.toStrict . TLB.toLazyText . go
    where go [] = ""
          go [Delim '\\'] = "\\" -- do not add newline in last token
          go [x] = renderToken x
          go (x:xs@(y:_))
              | needComment x y = renderToken x <> "/**/" <> go xs
              | otherwise = renderToken x <> go xs

{-# INLINE renderToken #-}
{-# INLINE needComment #-}

needComment :: Token -> Token -> Bool
needComment a CDC = case a of
    -- Can't be parsed that way but may exists in generated `Token` list.
    -- It's also possible to make Delim 'a' which will be parsed as Ident
    -- but we can't do much in this case since it's impossible to
    -- create Delim 'a' tokens in parser.
    Delim '!' -> True
    Delim '@' -> True
    Delim '#' -> True
    Delim '-' -> True
    Number {} -> True
    Dimension {} -> True
    Ident _ -> True
    AtKeyword _ -> True
    Function _ -> True
    Hash {} -> True
    _ -> False
needComment a b = case a of
    Whitespace    -> b == Whitespace
    Ident "--"    -> b == Delim '>' -- Looks like a CDC
    Ident _       -> idn || b == CDC || b == LeftParen
    AtKeyword _   -> idn || b == CDC
    Hash {}       -> idn || b == CDC
    Dimension {}  -> idn || b == CDC
    Delim '#'     -> idn
    Delim '-'     -> idn
    Number {}     -> i || num || b == Delim '%'
    Delim '@'     -> i || b == Delim '-'
    Delim '.'     -> num
    Delim '+'     -> num
    Delim '/'     -> b == Delim '*' || b == SubstringMatch
    Delim '|'     -> b == Delim '='
        || b == Delim '|' ||  b == Column || b == DashMatch
    Delim '$'     -> b == Delim '='
    Delim '*'     -> b == Delim '='
    Delim '^'     -> b == Delim '='
    Delim '~'     -> b == Delim '='
    _             -> False
    where idn = i || b == Delim '-' || num
          i = case b of
              Ident _ -> True
              Function _ -> True
              Url _ -> True
              BadUrl -> True
              _ -> False
          num = case b of
              Number {} -> True
              Percentage {} -> True
              Dimension {} -> True
              _ -> False


renderToken :: Token -> TLB.Builder
renderToken token = case token of
    Whitespace         -> c ' '

    CDO                -> "<!--"
    CDC                -> "-->"

    Comma              -> c ','
    Colon              -> c ':'
    Semicolon          -> c ';'

    LeftParen          -> c '('
    RightParen         -> c ')'
    LeftSquareBracket  -> c '['
    RightSquareBracket -> c ']'
    LeftCurlyBracket   -> c '{'
    RightCurlyBracket  -> c '}'

    SuffixMatch        -> "$="
    SubstringMatch     -> "*="
    PrefixMatch        -> "^="
    DashMatch          -> "|="
    IncludeMatch       -> "~="

    Column             -> "||"

    String x           -> string x
    BadString          -> "\"\n"

    Number x _         -> t x
    Percentage x _     -> t x <> c '%'
    Dimension x _ u    -> t x <> t (renderDimensionUnit x u)

    Url x              -> "url(" <> t (renderUrl x) <> c ')'
    BadUrl             -> "url(()"

    Ident x            -> ident x

    AtKeyword x        -> c '@' <> ident x

    Function x         -> ident x <> c '('

    Hash HId x           -> c '#' <> ident x
    Hash HUnrestricted x -> c '#' <> t (renderUnrestrictedHash x)

    Delim '\\'         -> "\\\n"
    Delim x            -> c x
    where c = TLB.singleton
          t = TLB.fromText
          q = c '"'
          string x = q <> t (renderString x) <> q
          ident = t . renderIdent

-- https://www.w3.org/TR/cssom-1/#serialize-a-string

renderString :: Text -> Text
renderString t0@(Text _ _ l)
    | T.any needEscape t0 = withNewA (l*8) $ go t0 0
    | otherwise = t0
  where
    needEscape c = c <= '\x1F' || c == '\x7F' || c == '"' || c == '\\'
    go t d dst = case T.uncons t of
        Nothing -> return d
        Just (c, t')
            | c == '\x0' -> do
                d' <- writeFFFD dst d
                -- spec says it should be escaped, but we loose
                -- serialize->tokenize->serialize roundtrip that way
                go t' d' dst
            | (c >= '\x1' && c <= '\x1F') || c == '\x7F' -> do
                d' <- escapeAsCodePoint dst d c
                go t' d' dst
            | c == '"' || c == '\\' -> do
                -- strings are always in double quotes, so '\'' aren't escaped
                write dst d '\\'
                write dst (d+1) c
                go t' (d+2) dst
            | otherwise -> do
                d' <- writeChar dst d c
                go t' d' dst

renderUrl :: Text -> Text
renderUrl t0@(Text _ _ l)
    | T.any needEscape t0 = withNewA (l*8) $ go t0 0
    | otherwise = t0
  where
    needEscape c = c <= '\x1F' || c == '\x7F' || isWhitespace c
        || c == '\\' || c == ')' || c == '"' || c == '\'' || c == '('
    go t d dst = case T.uncons t of
        Nothing -> return d
        Just (c, t')
            | c == '\x0' -> do
                d' <- writeFFFD dst d
                go t' d' dst
            | needEscape c -> do
                d' <- escapeAsCodePoint dst d c
                go t' d' dst
            | otherwise -> do
                d' <- writeChar dst d c
                go t' d' dst

renderDimensionUnit :: Text -> Text -> Text
renderDimensionUnit num t0@(Text _ _ l)
    | not (T.any isExponent num)
    , c :. t' <- t0
    , isExponent c && validExp t' =
        withNewA (l*8) $ \ dst -> do
            d' <- escapeAsCodePoint dst 0 c
            renderUnrestrictedHash' t' d' dst
    | otherwise =
        renderIdent t0
    where validExp (s :. d :. _) | (s == '+' || s == '-') = isDigit d
          validExp (d :. _) = isDigit d
          validExp _ = False

renderIdent :: Text -> Text
renderIdent "-" = "\\-"
renderIdent t0@(Text _ _ l) = case t0 of
    c :. t'
        | isDigit c -> withNewA (l*8) $ \ dst -> do
            d' <- escapeAsCodePoint dst 0 c
            renderUnrestrictedHash' t' d' dst
    '-' :. c :. t'
        | isDigit c -> withNewA (l*8) $ \ dst -> do
            write dst 0 '-'
            d' <- escapeAsCodePoint dst 1 c
            renderUnrestrictedHash' t' d' dst
    _ -> renderUnrestrictedHash t0

renderUnrestrictedHash :: Text -> Text
renderUnrestrictedHash t0@(Text _ _ l)
    | T.any (not . nameCodePoint) t0 =
        withNewA (l*8) $ renderUnrestrictedHash' t0 0
    | otherwise = t0

renderUnrestrictedHash' :: Text -> Int -> A.MArray s -> ST s Int
renderUnrestrictedHash' = go
    where go t d dst = case T.uncons t of
            Nothing -> return d
            Just (c, t')
                | c == '\x0' -> do
                    d' <- writeFFFD dst d
                    go t' d' dst
                | (c >= '\x1' && c <= '\x1F') || c == '\x7F' -> do
                    d' <- escapeAsCodePoint dst d c
                    go t' d' dst
                | nameCodePoint c -> do
                    d' <- writeChar dst d c
                    go t' d' dst
                | otherwise -> do
                    write dst d '\\'
                    d' <- writeChar dst (d+1) c
                    go t' d' dst

escapeAsCodePoint :: A.MArray s -> Int -> Char -> ST s Int
escapeAsCodePoint dst d c = do
    write dst d '\\'
    d' <- foldM (\ o x -> write dst o x >> return (o+1))
        (d+1) (showHex (ord c) [])
    write dst d' ' '
    return (d' + 1)


-- | verify valid escape and consume escaped code point
escapedCodePoint :: Text -> Maybe (Writer' s)
escapedCodePoint t = case t of
    (hex -> Just d) :. ts -> go 5 d ts
    '\n' :. _ -> Nothing
    c :. ts -> Just (\ dst d -> write dst d c >> return (d+1), ts)
    _ -> Nothing
    where go :: Int -> Int -> Text -> Maybe (Writer' s)
          go 0 acc ts = ret acc ts
          go n acc ts = case ts of
              (hex -> Just d) :. ts' -> go (n-1) (acc*16 + d) ts'
              c :. ts' | isWhitespace c -> ret acc ts'
              _ -> ret acc ts
          ret c ts = Just
              (\ dst d ->
                  if safe c
                  then writeChar dst d (unsafeChr c)
                  else writeFFFD dst d
              ,ts)

safe :: Int -> Bool
safe x
    | x == 0 || x > 0x10FFFF   = False
    | x .&. 0x1ff800 /= 0xd800 = True
    | otherwise                = False -- UTF-16 surrogate code point

hex :: Char -> Maybe Int
hex c
    | c >= '0' && c <= '9' = Just (ord c - ord '0')
    | c >= 'a' && c <= 'f' = Just (ord c - ord 'a' + 10)
    | c >= 'A' && c <= 'F' = Just (ord c - ord 'A' + 10)
    | otherwise            = Nothing

{-# INLINE safe #-}
{-# INLINE hex #-}

escapedCodePoint' :: Text -> Maybe (Writer' s)
escapedCodePoint' ('\\' :. ts) = escapedCodePoint ts
escapedCodePoint' _ = Nothing

nameStartCodePoint :: Char -> Bool
nameStartCodePoint c =
    isAsciiLower c || isAsciiUpper c || c >= '\x0080' || c == '_'

nameCodePoint :: Char -> Bool
nameCodePoint c = nameStartCodePoint c || isDigit c || c == '-'

satisfyOrEscaped :: (Char -> Bool) -> Text -> Maybe (Writer' s)
satisfyOrEscaped p (c :. ts)
    | p c = Just (\ dst d -> write dst d c >> return (d+1), ts)
    | c == '\\' = escapedCodePoint ts
satisfyOrEscaped _ _ = Nothing

-- | Check if three code points would start an identifier and consume name
parseName :: Text -> Maybe (Writer s)
parseName t = case t of
    '-' :. ts -> consumeName' <$> satisfyOrEscaped (\ c -> nameStartCodePoint c || c == '-') ts
    ts -> consumeName <$> satisfyOrEscaped nameStartCodePoint ts
    where consumeName' n dst d = do
              write dst d '-'
              consumeName n dst (d + 1)


consumeName :: Writer' s -> Writer s
consumeName (w0, ts0) dst d0 = do
    d' <- w0 dst d0
    loop ts0 d'
    where loop ts d = case satisfyOrEscaped nameCodePoint ts of
              Just (w, ts') -> do
                  d' <- w dst d
                  loop ts' d'
              Nothing -> return (d, ts)

{-# INLINE parseName #-}
{-# INLINE consumeName #-}
{-# INLINE satisfyOrEscaped #-}
{-# INLINE escapedCodePoint #-}
{-# INLINE escapedCodePoint' #-}

parseNumericValue :: Text -> Maybe (Text, NumericValue, Text)
parseNumericValue t0@(Text a offs1 _) = case withSign start t0 of
    Just (nv, ts@(Text _ offs2 _)) ->
        Just (Text a offs1 (offs2 - offs1), nv, ts)
    Nothing -> Nothing
    where start sign t = case t of
              '.' :. (digit -> Just d) :. ts -> dot sign (startIR d) (-1) ts
              (digit -> Just d) :. ts        -> digits sign (startIR d) ts
              _ -> Nothing
          digits sign !c t = case t of
              '.' :. (digit -> Just d) :. ts -> dot sign (accIR c d) (-1) ts
              (digit -> Just d) :. ts        -> digits sign (accIR c d) ts
              _ -> Just $ expn True (sign $ readIR c) 0 t
          dot sign !c !e t = case t of
              (digit -> Just d) :. ts        -> dot sign (accIR c d) (e-1) ts
              _ -> Just $ expn False (sign $ readIR c) e t
          expn int c e0 t = case t of
              x :. ts
                  | isExponent x
                  , Just r <- withSign (expStart c e0 0) ts -> r
              _   | int -> (NVInteger c, t)
                  | otherwise -> (NVNumber $ scientific c e0, t)
          expStart c e0 e sign t = case t of
              (digit -> Just d) :. ts -> expDigits c e0 (e*10 + d) sign ts
              _ -> Nothing
          expDigits c e0 !e sign t = case t of
              (digit -> Just d) :. ts -> expDigits c e0 (e*10 + d) sign ts
              _ -> Just (NVNumber $ scientific c (sign e + e0), t)
          digit :: Enum a => Char -> Maybe a
          digit c
              | isDigit c = Just (toEnum $ ord c - ord '0')
              | otherwise = Nothing
          withSign :: Num a => ((a -> a) -> Text -> Maybe (b, Text))
                   -> Text -> Maybe (b, Text)
          withSign f t = case t of
              '+' :. ts -> f id ts
              '-' :. ts -> f negate ts
              _ -> f id t

-- Idea stolen from GHC implementation of `instance Read Integer`
-- http://hackage.haskell.org/package/base-4.11.1.0/docs/src/Text.Read.Lex.html#valInteger
-- A sub-quadratic algorithm for converting digits to Integer.
-- First we collect blocks of `blockDigits`-digit Integers
-- (so we don't do anything besides simple (acc*10+digit) on most inputs).
-- Then we combine them:
-- Pairs of adjacent radix b digits are combined into a single radix b^2 digit.
-- This process is repeated until we are left with a single digit.

blockDigits :: Int
blockDigits = 40

startBase :: Integer
startBase = 10^blockDigits

-- | (num digits in current block, blocks, current block's value)
type IntegerReader = (Int, [Integer], Integer)

startIR :: Integer -> IntegerReader
startIR d = (1, [], d)

{-# INLINE startIR #-}
{-# INLINE accIR #-}
{-# INLINE readIR #-}

accIR :: IntegerReader -> Integer -> IntegerReader
accIR (n, blocks, !cd) d
    | n < blockDigits = (n+1, blocks, cd*10 + d)
    | otherwise = (1, cd:blocks, d)

readIR :: IntegerReader -> Integer
readIR (_, [], cd) = cd
readIR (n, blocks, cd) =
    go startBase ((cd * padding):blocks) `div` padding
    where padding = 10^(blockDigits-n)
          go :: Integer -> [Integer] -> Integer
          go _ [] = 0
          go _ [x] = x
          go b xs = go (b*b) (combine b xs)
          combine :: Integer -> [Integer] -> [Integer]
          combine _ [] = []
          combine _ [x] = [x]
          combine b (x0:x1:xs) = x' : combine b xs
              where !x' = x0 + x1*b

skipComment :: Text -> Text
skipComment t = case t of
    '*' :. '/' :. ts -> ts
    _ :. ts -> skipComment ts
    ts -> ts

skipWhitespace :: Text -> Text
skipWhitespace t = case t of
    c :. ts
        | isWhitespace c -> skipWhitespace ts
        | otherwise -> t
    ts -> ts

parseTokens :: Text -> [Token]
parseTokens t0@(Text _ _ len) = snd $ A.run2 $ do
    dst <- A.new len
    dsta <- A.unsafeFreeze dst
    let go' !t d tgo = do
            ts <- inlineInterleaveST $ go d tgo
            return (t : ts)
        go d tgo = case tgo of
            c :. ts | isWhitespace c ->
                 go' Whitespace d (skipWhitespace ts)
            '/' :. '*' :. ts -> go d (skipComment ts)

            '<' :. '!' :. '-' :. '-' :. ts -> token CDO ts
            '-' :. '-' :. '>' :. ts ->        token CDC ts

            ',' :. ts -> token Comma ts
            ':' :. ts -> token Colon ts
            ';' :. ts -> token Semicolon ts
            '(' :. ts -> token LeftParen ts
            ')' :. ts -> token RightParen ts
            '[' :. ts -> token LeftSquareBracket ts
            ']' :. ts -> token RightSquareBracket ts
            '{' :. ts -> token LeftCurlyBracket ts
            '}' :. ts -> token RightCurlyBracket ts

            '$' :. '=' :. ts -> token SuffixMatch ts
            '*' :. '=' :. ts -> token SubstringMatch ts
            '^' :. '=' :. ts -> token PrefixMatch ts
            '|' :. '=' :. ts -> token DashMatch ts
            '~' :. '=' :. ts -> token IncludeMatch ts

            '|' :. '|' :. ts -> token Column ts

            (parseNumericValue -> Just (repr, nv, ts))
                | '%' :. ts' <- ts ->
                    go' (Percentage repr nv) d ts'
                | Just u <- parseName ts -> do
                    (unit, d', ts') <- mkText dst d u
                    go' (Dimension repr nv unit) d' ts'
                | otherwise ->
                    go' (Number repr nv) d ts

            -- ident like
            (parseName -> Just n) -> do
                (name, d', ts) <- mkText dst d n
                if isUrl name then
                    -- Special handling of url() functions (they are not really
                    -- functions, they have their own Token type).
                    case ts of
                        '(' :. (skipWhitespace -> ts') ->
                            case ts' of
                                '"'  :. _ -> go' (Function name) d' ts'
                                '\'' :. _ -> go' (Function name) d' ts'
                                _ -> parseUrl d' ts'
                        _ -> go' (Ident name) d' ts
                else
                    case ts of
                        '(' :. ts' -> go' (Function name) d' ts'
                        _ -> go' (Ident name) d' ts

            '"' :. ts -> parseString '"' d ts
            '\'' :. ts -> parseString '\'' d ts

            '@' :. (parseName -> Just n) -> do
                (name, d', ts) <- mkText dst d n
                go' (AtKeyword name) d' ts

            '#' :. (parseName -> Just n) -> do
                (name, d', ts) <- mkText dst d n
                go' (Hash HId name) d' ts

            '#' :. (satisfyOrEscaped nameCodePoint -> Just n) -> do
                (name, d', ts) <- mkText dst d (consumeName n)
                go' (Hash HUnrestricted name) d' ts

            c :. ts ->
                token (Delim c) ts
            _ -> return []

            where token t ts = go' t d ts

        isUrl t@(Text _ _ 3)
            | u :. r :. l :. _ <- t =
                (u == 'u' || u == 'U') &&
                (r == 'r' || r == 'R') &&
                (l == 'l' || l == 'L')
        isUrl _ = False

        -- https://drafts.csswg.org/css-syntax-3/#consume-string-token
        parseString endingCodePoint d0 = string d0
            where string d t = case t of
                      c :. ts | c == endingCodePoint -> ret d ts
                      '\\' :. ts
                          | Just (p, ts') <- escapedCodePoint ts -> do
                              d' <- p dst d
                              string d' ts'
                          | '\n' :. ts' <- ts ->
                              string d ts'
                          | Text _ _ 0 <- ts ->
                              string d ts
                      '\n' :. _ -> go' BadString d t
                      c :. ts -> do
                          write dst d c
                          string (d+1) ts
                      _ -> ret d t
                  ret d t = go' (String $ Text dsta d0 (d-d0)) d t

        -- https://drafts.csswg.org/css-syntax/#consume-url-token
        parseUrl d0 tUrl = url d0 (skipWhitespace tUrl)
            where ret d ts = go' (Url (Text dsta d0 (d-d0))) d ts
                  url d t = case t of
                      ')' :. ts -> ret d ts
                      c :. ts
                          | c == '"' || c == '\'' || c == '('
                            || nonPrintableCodePoint c -> do
                              badUrl d ts
                          | isWhitespace c ->
                              whitespace d ts
                      '\\' :. ts
                          | Just (p, ts') <- escapedCodePoint ts -> do
                              d' <- p dst d
                              url d' ts'
                          | otherwise ->
                              badUrl d ts
                      c :. ts -> do
                          write dst d c
                          url (d+1) ts
                      _ ->
                          ret d t
                  whitespace d t = case t of
                      c :. ts -> do
                          if isWhitespace c then
                              whitespace d ts
                          else if c == ')' then
                              ret d ts
                          else
                              badUrl d ts
                      _ ->
                          ret d t
                  badUrl d t = case t of
                      ')' :. ts -> go' BadUrl d ts
                      (escapedCodePoint' -> Just (_, ts)) -> do
                          badUrl d ts
                      _ :. ts ->
                          badUrl d ts
                      _ -> go' BadUrl d t
        mkText :: A.MArray s -> Int -> Writer s -> ST s (Text, Int, Text)
        mkText dest d w = do
            (d', ts) <- w dest d
            return (Text dsta d (d' - d), d', ts)

    r <- go 0 t0
    return (dst, r)


isWhitespace :: Char -> Bool
isWhitespace '\x0009' = True
isWhitespace '\x000A' = True
isWhitespace '\x0020' = True
isWhitespace _        = False

nonPrintableCodePoint :: Char -> Bool
nonPrintableCodePoint c
    | c >= '\x0000' && c <= '\x0008' = True -- NULL through BACKSPACE
    | c == '\x000B'                  = True -- LINE TABULATION
    | c >= '\x000E' && c <= '\x001F' = True -- SHIFT OUT through INFORMATION SEPARATOR ONE
    | c == '\x007F'                  = True -- DELETE
    | otherwise                      = False

isExponent :: Char -> Bool
isExponent c = c == 'e' || c == 'E'
