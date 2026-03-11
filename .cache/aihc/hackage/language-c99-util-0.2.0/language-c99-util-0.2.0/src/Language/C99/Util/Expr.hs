module Language.C99.Util.Expr
  ( digit
  , nonzerodigit
  , nondigit
  , ident
  , litbool
  , litint
  , litdouble
  , litfloat
  , litstring
  , identdeclr
  ) where

import Data.Char (isDigit)

import Language.C99.AST
import Language.C99.Util.Wrap

-- A digit in Haskell, not C
type HSDigit = Int

digit :: Int -> Digit
digit i = case i of
  0 -> DZero
  1 -> DOne
  2 -> DTwo
  3 -> DThree
  4 -> DFour
  5 -> DFive
  6 -> DSix
  7 -> DSeven
  8 -> DEight
  9 -> DNine
  _ -> error $ show i ++ " is not a digit"

nonzerodigit :: Int -> NonZeroDigit
nonzerodigit i = case i of
  1 -> NZOne
  2 -> NZTwo
  3 -> NZThree
  4 -> NZFour
  5 -> NZFive
  6 -> NZSix
  7 -> NZSeven
  8 -> NZEight
  9 -> NZNine
  _ -> error $ show i ++ " is not a non-zero digit"

nondigit :: Char -> IdentNonDigit
nondigit c = IdentNonDigit $ case c of
  '_' -> NDUnderscore
  'a' -> NDa ;      'A' -> NDA
  'b' -> NDb ;      'B' -> NDB
  'c' -> NDc ;      'C' -> NDC
  'd' -> NDd ;      'D' -> NDD
  'e' -> NDe ;      'E' -> NDE
  'f' -> NDf ;      'F' -> NDF
  'g' -> NDg ;      'G' -> NDG
  'h' -> NDh ;      'H' -> NDH
  'i' -> NDi ;      'I' -> NDI
  'j' -> NDj ;      'J' -> NDJ
  'k' -> NDk ;      'K' -> NDK
  'l' -> NDl ;      'L' -> NDL
  'm' -> NDm ;      'M' -> NDM
  'n' -> NDn ;      'N' -> NDN
  'o' -> NDo ;      'O' -> NDO
  'p' -> NDp ;      'P' -> NDP
  'q' -> NDq ;      'Q' -> NDQ
  'r' -> NDr ;      'R' -> NDR
  's' -> NDs ;      'S' -> NDS
  't' -> NDt ;      'T' -> NDT
  'u' -> NDu ;      'U' -> NDU
  'v' -> NDv ;      'V' -> NDV
  'w' -> NDw ;      'W' -> NDW
  'x' -> NDx ;      'X' -> NDX
  'y' -> NDy ;      'Y' -> NDY
  'z' -> NDz ;      'Z' -> NDZ
  _   -> error $ show c ++ " is not a nondigit"

ident :: String -> Ident
ident (c:cs) = foldl char (IdentBase $ nondigit c) cs where
  char cs c | isDigit c = IdentCons         cs (digit (read [c]))
            | otherwise = IdentConsNonDigit cs (nondigit c)

litbool :: Bool -> PrimExpr
litbool False = PrimConst $ ConstEnum $ Enum (ident "false")
litbool True  = PrimConst $ ConstEnum $ Enum (ident "true")

litint :: Integer -> UnaryExpr
litint i | i == 0 = UnaryPostfix $ PostfixPrim $ constzero
         | i >  0 = UnaryPostfix $ PostfixPrim $ constint i
         | i <  0 = UnaryOp UOMin (CastUnary $ litint (abs i))

litfp :: (RealFloat a, Show a) => Maybe FloatSuffix -> a -> UnaryExpr
litfp mbFS n
  | isNaN n = UnaryPostfix $ PostfixPrim $ PrimIdent $ ident "NAN"
  | isInfinite n = case n > 0 of
    True  -> UnaryPostfix $ PostfixPrim $ PrimIdent $ ident "INFINITY"
    False -> UnaryOp UOMin (CastUnary $ UnaryPostfix $ PostfixPrim $ PrimIdent $ ident "INFINITY")
  | otherwise = litfpFinite mbFS n
  where
    litfpFinite :: (RealFloat a, Show a) => Maybe FloatSuffix -> a -> UnaryExpr
    litfpFinite mbFS = parse . lex where
      lex :: (RealFloat a, Show a) => a -> (String, String, String)
      lex d = (nat, dec, exp) where
        ds = show d
        e = dropWhile (/='e') ds
        nat = takeWhile (/='.') ds
        dec = takeWhile (/='e') $ tail $ dropWhile (/='.') ds
        exp = case length e of
          0 -> ""
          _ -> tail e

    parse :: (String, String, String) -> UnaryExpr
    parse (nat, dec, exp) = op $ PostfixPrim $ PrimConst $ ConstFloat $ FloatDec $ DecFloatFrac (FracZero (Just nat') dec') exp' mbFS where
      op = case head nat of
        '-' -> UnaryOp UOMin . CastUnary . UnaryPostfix
        _   -> UnaryPostfix
      nat' = case head nat of
        '-' -> digitseq $ digitsc (tail nat)
        _   -> digitseq $ digitsc nat
      dec' = digitseq $ digitsc dec
      exp' = case exp of
        "" -> Nothing
        (e:es)  -> case e of
          '-' -> Just $ E (Just SMinus) (digitseq $ digitsc es)
          _   -> Just $ E Nothing (digitseq $ digitsc (e:es))

litdouble :: Double -> UnaryExpr
litdouble = litfp Nothing

litfloat :: Float -> UnaryExpr
litfloat = litfp (Just FF)

litstring :: String -> UnaryExpr
litstring ss = wrap $ PrimString $ StringLit $ (sl ss) where
  sl :: String -> Maybe SCharSeq
  sl [] = Nothing
  sl cs = Just $ readschar cs

  readschar :: String -> SCharSeq
  readschar (c:cs) = foldl SCharCons (SCharBase $ f c) (map f cs)

  f :: Char -> SChar
  f c | isEscseq c = SCharEsc $ litescseq c
      | otherwise  = SChar c

litescseq :: Char -> EscSeq
litescseq c = case c of
  '\'' -> EscSimple SEQuote
  '\"' -> EscSimple SEDQuote
  -- '\?' -> EscSimple SEQuestion
  '\\' -> EscSimple SEBackSlash
  '\a' -> EscSimple SEa
  '\b' -> EscSimple SEb
  '\f' -> EscSimple SEf
  '\n' -> EscSimple SEn
  '\r' -> EscSimple SEr
  '\t' -> EscSimple SEt
  '\v' -> EscSimple SEv
  otherwise -> error $ show c ++ " is not an escape sequence."

isEscseq :: Char -> Bool
isEscseq c = c `elem` "\'\"\\\a\b\f\n\n\r\t\v"


identdeclr :: String -> Declr
identdeclr name = Declr Nothing (DirectDeclrIdent $ ident name)


intdigits :: Integer -> [HSDigit]
intdigits = map (read.return).show

constint :: Integer -> PrimExpr
constint i = PrimConst $ ConstInt $ IntDec (decconst $ intdigits i) Nothing

constzero :: PrimExpr
constzero = PrimConst $ ConstInt $ IntOc Oc0 Nothing

decconst :: [HSDigit] -> DecConst
decconst (d:ds) = foldl step base ds where
  base      = DecBase $ nonzerodigit d
  step xs x = DecCons xs (digit x)

digitseq :: [Int] -> DigitSeq
digitseq (x:xs) = foldl DigitCons (DigitBase (digit x)) (map digit xs) where

digitsc :: [Char] -> [Int]
digitsc cs = map (\x -> read [x]) cs
