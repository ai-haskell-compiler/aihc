module Countdown where

-----------------------------------------------------------------------------
--
--                           The Countdown Problem
--
--                               Graham Hutton
--                         University of Nottingham
--
--                               November 2001
--
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- Formally specifying the problem
-----------------------------------------------------------------------------

data Op               = Add | Sub | Mul | Div
  deriving Eq

valid                :: Op -> Int -> Int -> Bool
valid Add _ _         = True
valid Sub x y         = x > y
valid Mul _ _         = True
valid Div x y         = x `mod` y == 0

apply                :: Op -> Int -> Int -> Int
apply Add x y         = x + y
apply Sub x y         = x - y
apply Mul x y         = x * y
apply Div x y         = x `div` y

data Expr             = Val Int | App Op Expr Expr
  deriving Eq

values               :: Expr -> [Int]
values (Val n)        = [n]
values (App _ l r)    = values l ++ values r

eval                 :: Expr -> [Int]
eval (Val n)          = [n | n > 0]
eval (App o l r)      = [apply o x y | x <- eval l, y <- eval r, valid o x y]

subbags              :: [a] -> [[a]]
subbags xs            = [zs | ys <- subs xs, zs <- perms ys]

subs                 :: [a] -> [[a]]
subs []               = [[]]
subs (x:xs)           = ys ++ map (x:) ys
                        where
                           ys = subs xs

perms                :: [a] -> [[a]]
perms []              = [[]]
perms (x:xs)          = concat (map (interleave x) (perms xs))

interleave           :: a -> [a] -> [[a]]
interleave x []       = [[x]]
interleave x (y:ys)   = (x:y:ys) : map (y:) (interleave x ys)

solution             :: Expr -> [Int] -> Int -> Bool
solution e ns n       = elem (values e) (subbags ns) && eval e == [n]

-----------------------------------------------------------------------------
-- Brute force implementation
-----------------------------------------------------------------------------

split                :: [a] -> [([a],[a])]
split []              = [([],[])]
split (x:xs)          = ([],x:xs) : [(x:ls,rs) | (ls,rs) <- split xs]

nesplit              :: [a] -> [([a],[a])]
nesplit               = filter ne . split

ne                   :: ([a],[b]) -> Bool
ne (xs,ys)            = not (null xs || null ys)

exprs                :: [Int] -> [Expr]
exprs []              = []
exprs [n]             = [Val n]
exprs ns              = [e | (ls,rs) <- nesplit ns
                           , l       <- exprs ls
                           , r       <- exprs rs
                           , e       <- combine l r]

combine              :: Expr -> Expr -> [Expr]
combine l r           = [App o l r | o <- ops]

ops                  :: [Op]
ops                   = [Add,Sub,Mul,Div]

solutions            :: [Int] -> Int -> [Expr]
solutions ns n        = [e | ns' <- subbags ns, e <- exprs ns', eval e == [n]]

-----------------------------------------------------------------------------
-- Fusing generation and evaluation
-----------------------------------------------------------------------------

type Result           = (Expr,Int)

results              :: [Int] -> [Result]
results []            = []
results [n]           = [(Val n,n) | n > 0]
results ns            = [res | (ls,rs) <- nesplit ns
                             , lx      <- results ls
                             , ry      <- results rs
                             , res     <- combine' lx ry]

combine'             :: Result -> Result -> [Result]
combine' (l,x) (r,y)  = [(App o l r, apply o x y) | o <- ops, valid o x y]

solutions'           :: [Int] -> Int -> [Expr]
solutions' ns n       = [e | ns' <- subbags ns, (e,m) <- results ns', m == n]

-----------------------------------------------------------------------------
-- Exploiting arithmetic properties
-----------------------------------------------------------------------------

valid'               :: Op -> Int -> Int -> Bool
valid' Add x y        = x <= y
valid' Sub x y        = x > y
valid' Mul x y        = x /= 1 && y /= 1 && x <= y
valid' Div x y        = y /= 1 && x `mod` y == 0

eval'                :: Expr -> [Int]
eval' (Val n)         = [n | n > 0]
eval' (App o l r)     = [apply o x y | x <- eval' l, y <- eval' r, valid' o x y]

solution'            :: Expr -> [Int] -> Int -> Bool
solution' e ns n      = elem (values e) (subbags ns) && eval' e == [n]

results'             :: [Int] -> [Result]
results' []           = []
results' [n]          = [(Val n,n) | n > 0]
results' ns           = [res | (ls,rs) <- nesplit ns
                             , lx      <- results' ls
                             , ry      <- results' rs
                             , res     <- combine'' lx ry]

combine''            :: Result -> Result -> [Result]
combine'' (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid' o x y]

solutions''          :: [Int] -> Int -> [Expr]
solutions'' ns n      = [e | ns' <- subbags ns, (e,m) <- results' ns', m == n]

-----------------------------------------------------------------------------
-- Interactive version for testing
-----------------------------------------------------------------------------

instance Show Op where
   show Add           = "+"
   show Sub           = "-"
   show Mul           = "*"
   show Div           = "/"

instance Show Expr where
   show (Val n)       = show n
   show (App o l r)   = bracket l ++ show o ++ bracket r
                        where
                           bracket (Val n) = show n
                           bracket e       = "(" ++ show e ++ ")"

display              :: [Expr] -> IO ()
display []            = putStr "\nThere are no solutions.\n\n"
display (e:es)        = do putStr "\nOne possible solution is "
                           putStr (show e)
	                   putStr ".\n\nPress return to continue searching..."
                           getLine
                           putStr "\n"
                           if null es then
                               putStr "There are no more solutions.\n\n"
                            else
                               do sequence [print e | e <- es]
                                  putStr "\nThere were "
                                  putStr (show (length (e:es)))
                                  putStr " solutions in total.\n\n"

-- Properties

infixr 0 -->
False --> _ = True
True --> x = x

prop_lemma1 :: ([Int], [Int], [Int]) -> Bool
prop_lemma1 (xs, ys, zs) = ((xs,ys) `elem` split zs) == (xs ++ ys == zs)

prop_lemma3 :: ([Int], [Int], [Int]) -> Bool
prop_lemma3 (xs, ys, zs) = ((xs, ys) `elem` nesplit zs)
                             == (xs ++ ys == zs && ne (xs, ys))

prop_lemma4 :: ([Int], [Int], [Int]) -> Bool
prop_lemma4 (xs, ys, zs) = ((xs, ys) `elem` nesplit zs) -->
                             (length xs < length zs && length ys < length zs)

prop_solutions (ns, m) = solutions ns m == solutions' ns m
