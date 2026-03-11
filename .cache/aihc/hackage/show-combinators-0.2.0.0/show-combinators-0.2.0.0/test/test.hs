import Text.Show.Combinators

data MyType a
  = C a a                   -- a regular constructor
  | a :+: a                 -- an infix constructor
  | R { f1 :: a, f2 :: a }  -- a record
  deriving Show

infixl 4 :+:

showsMyType :: (a -> PrecShowS) -> MyType a -> PrecShowS
showsMyType showA (C a b) = showCon "C" `showApp` showA a `showApp` showA b
showsMyType showA (c :+: d) = showInfix ":+:" 4 (showA c) (showA d)
showsMyType showA (R {f1 = e, f2 = f}) =
  showRecord "R" ("f1" `showField` showA e &| "f2" `showField` showA f)

-- Just making sure this typechecks
_showsMyType' :: Show a => MyType a -> PrecShowS
_showsMyType' (C a b) = showCon "C" @| a @| b
_showsMyType' (c :+: d) = showInfix' ":+:" 4 c d
_showsMyType' (R {f1 = e, f2 = f}) =
  showRecord "R" ("f1" .=. e &| "f2" .=. f)

showR :: [Int] -> PrecShowS
showR [] = showCon "[]"
showR (x : xs) = showInfixr ":" 5 (flip showsPrec x) (showR xs)

-- snoc lists
showL :: [Int] -> PrecShowS
showL [] = showCon "[]"
showL (x : xs) = showInfixl ":" 5 (showL xs) (flip showsPrec x)

check :: Show a => (a -> PrecShowS) -> Int -> a -> IO ()
check show' d x = assertEqual s s'
  where
    s = showsPrec d x ""
    s' = show' x d ""

assertEqual :: (Eq a, Show a) => a -> a -> IO ()
assertEqual s s' =
  if s == s' then
    return ()
  else
    fail $ show (s, s')

unPS :: (a -> PrecShowS) -> a -> String
unPS p x = p x 0 ""

main :: IO ()
main = do
  check smt1  0 (C () ())
  check smt2  0 (C (C () ()) (() :+: ()))
  check smt2  0 ((() :+: ()) :+: (() :+: ()))
  check smt2 11 (R (C () ()) (C () ()))
  assertEqual (unPS showR [1,2,3]) "1 : 2 : 3 : []"
  assertEqual (unPS showL [1,2,3]) "[] : 3 : 2 : 1"
  where
    smt1 = showsMyType (flip showsPrec)
    smt2 = showsMyType smt1
