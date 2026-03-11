-- | For documentation, see the paper "SmallCheck and Lazy SmallCheck:
-- automatic exhaustive testing for small values" available at
-- <http://www.cs.york.ac.uk/fp/smallcheck/>.  Several examples are
-- also included in the package.

module Test.LazySmallCheck
  ( Serial(series) -- :: class
  , Series         -- :: type Series a = Int -> Cons a
  , Cons           -- :: *
  , cons           -- :: a -> Series a
  , (><)           -- :: Series (a -> b) -> Series a -> Series b
  , empty          -- :: Series a
  , (\/)           -- :: Series a -> Series a -> Series a
  , drawnFrom      -- :: [a] -> Cons a
  , cons0          -- :: a -> Series a
  , cons1          -- :: Serial a => (a -> b) -> Series b
  , cons2          -- :: (Serial a, Serial b) => (a -> b -> c) -> Series c
  , cons3          -- :: ...
  , cons4          -- :: ...
  , cons5          -- :: ...
  , Testable       -- :: class
  , depthCheck     -- :: Testable a => Int -> a -> IO ()
  , smallCheck     -- :: Testable a => Int -> a -> IO ()
  , test           -- :: Testable a => a -> IO ()
  , (==>)          -- :: Bool -> Bool -> Bool
  , Property       -- :: *
  , lift           -- :: Bool -> Property
  , neg            -- :: Property -> Property
  , (*&*)          -- :: Property -> Property -> Property
  , (*|*)          -- :: Property -> Property -> Property
  , (*=>*)         -- :: Property -> Property -> Property
  , (*=*)          -- :: Property -> Property -> Property
  )
  where

import Control.Monad
import Control.Exception
import System.Exit

infixr 0 ==>, *=>*
infixr 3 \/, *|*
infixl 4 ><, *&*

type Pos = [Int]

data Term = Var Pos Type | Ctr Int [Term]

data Type = SumOfProd [[Type]]

type Series a = Int -> Cons a

data Cons a = C Type ([[Term] -> a])

class Serial a where
  series :: Series a

-- Series constructors

cons :: a -> Series a
cons a d = C (SumOfProd [[]]) [const a]

empty :: Series a
empty d = C (SumOfProd []) []

(><) :: Series (a -> b) -> Series a -> Series b
(f >< a) d = C (SumOfProd [ta:p | shallow, p <- ps]) cs
  where
    C (SumOfProd ps) cfs = f d
    C ta cas = a (d-1)
    cs = [\(x:xs) -> cf xs (conv cas x) | shallow, cf <- cfs]
    shallow = d > 0 && nonEmpty ta

nonEmpty :: Type -> Bool
nonEmpty (SumOfProd ps) = not (null ps)

(\/) :: Series a -> Series a -> Series a
(a \/ b) d = C (SumOfProd (ssa ++ ssb)) (ca ++ cb)
  where
    C (SumOfProd ssa) ca = a d
    C (SumOfProd ssb) cb = b d

conv :: [[Term] -> a] -> Term -> a
conv cs (Var p _) = error ('\0':map toEnum p)
conv cs (Ctr i xs) = (cs !! i) xs

drawnFrom :: [a] -> Cons a
drawnFrom xs = C (SumOfProd (map (const []) xs)) (map const xs)

-- Helpers, a la SmallCheck

cons0 :: a -> Series a
cons0 f = cons f

cons1 :: Serial a => (a -> b) -> Series b
cons1 f = cons f >< series

cons2 :: (Serial a, Serial b) => (a -> b -> c) -> Series c
cons2 f = cons f >< series >< series

cons3 :: (Serial a, Serial b, Serial c) => (a -> b -> c -> d) -> Series d
cons3 f = cons f >< series >< series >< series

cons4 :: (Serial a, Serial b, Serial c, Serial d) =>
  (a -> b -> c -> d -> e) -> Series e
cons4 f = cons f >< series >< series >< series >< series

cons5 :: (Serial a, Serial b, Serial c, Serial d, Serial e) =>
  (a -> b -> c -> d -> e -> f) -> Series f
cons5 f = cons f >< series >< series >< series >< series >< series

-- Standard instances

instance Serial () where
  series = cons0 ()

instance Serial Bool where
  series = cons0 False \/ cons0 True

instance Serial a => Serial (Maybe a) where
  series = cons0 Nothing \/ cons1 Just

instance (Serial a, Serial b) => Serial (Either a b) where
  series = cons1 Left \/ cons1 Right

instance Serial a => Serial [a] where
  series = cons0 [] \/ cons2 (:)

instance (Serial a, Serial b) => Serial (a, b) where
  series = cons2 (,) . (+1)

instance (Serial a, Serial b, Serial c) => Serial (a, b, c) where
  series = cons3 (,,) . (+1)

instance (Serial a, Serial b, Serial c, Serial d) =>
    Serial (a, b, c, d) where
  series = cons4 (,,,) . (+1)

instance (Serial a, Serial b, Serial c, Serial d, Serial e) =>
    Serial (a, b, c, d, e) where
  series = cons5 (,,,,) . (+1)

instance Serial Int where
  series d = drawnFrom [-d..d]

instance Serial Integer where
  series d = drawnFrom (map toInteger [-d..d])

instance Serial Char where
  series d = drawnFrom (take (d+1) ['a'..])

instance Serial Float where
  series d = drawnFrom (floats d)

instance Serial Double where
  series d = drawnFrom (floats d)

floats :: RealFloat a => Int -> [a]
floats d = [ encodeFloat sig exp
           | sig <- map toInteger [-d..d]
           , exp <- [-d..d]
           , odd sig || sig == 0 && exp == 0
           ]

-- Term refinement

refine :: Term -> Pos -> [Term]
refine (Var p (SumOfProd ss)) [] = new p ss
refine (Ctr c xs) p = map (Ctr c) (refineList xs p)

refineList :: [Term] -> Pos -> [[Term]]
refineList xs (i:is) = [ls ++ y:rs | y <- refine x is]
  where (ls, x:rs) = splitAt i xs

new :: Pos -> [[Type]] -> [Term]
new p ps = [ Ctr c (zipWith (\i t -> Var (p++[i]) t) [0..] ts)
           | (c, ts) <- zip [0..] ps ]

-- Find total instantiations of a partial value

total :: Term -> [Term] 
total val = tot val
  where
    tot (Ctr c xs) = [Ctr c ys | ys <- mapM tot xs] 
    tot (Var p (SumOfProd ss)) = [y | x <- new p ss, y <- tot x]

-- Answers

answer :: a -> (a -> IO b) -> (Pos -> IO b) -> IO b
answer a known unknown =
  do res <- try (evaluate a)
     case res of
       Right b -> known b
       Left (ErrorCall ('\0':p)) -> unknown (map fromEnum p)
       Left e -> throw e

-- Refute

refute :: Result -> IO Int
refute r = ref (args r)
  where
    ref xs = eval (apply r xs) known unknown
      where
        known True = return 1
        known False = report
        unknown p = sumMapM ref 1 (refineList xs p)

        report =
          do putStrLn "Counter example found:"
             mapM_ putStrLn $ zipWith ($) (showArgs r)
                            $ head [ys | ys <- mapM total xs]
             exitWith ExitSuccess

sumMapM :: (a -> IO Int) -> Int -> [a] -> IO Int
sumMapM f n [] = return n
sumMapM f n (a:as) = seq n (do m <- f a ; sumMapM f (n+m) as)

-- Properties with parallel conjunction (Lindblad TFP'07)

data Property =
    Bool Bool
  | Neg Property
  | And Property Property
  | ParAnd Property Property
  | Eq Property Property

eval :: Property -> (Bool -> IO a) -> (Pos -> IO a) -> IO a
eval p k u = answer p (\p -> eval' p k u) u

eval' (Bool b) k u = answer b k u
eval' (Neg p) k u = eval p (k . not) u
eval' (And p q) k u = eval p (\b-> if b then eval q k u else k b) u
eval' (Eq p q) k u = eval p (\b-> if b then eval q k u else eval (Neg q) k u) u
eval' (ParAnd p q) k u = eval p (\b-> if b then eval q k u else k b) unknown
  where
    unknown pos = eval q (\b-> if b then u pos else k b) (\_-> u pos)

lift :: Bool -> Property
lift b = Bool b

neg :: Property -> Property
neg p = Neg p

(*&*), (*|*), (*=>*), (*=*) :: Property -> Property -> Property
p *&* q = ParAnd p q
p *|* q = neg (neg p *&* neg q)
p *=>* q = neg (p *&* neg q)
p *=* q = Eq p q

-- Boolean implication

(==>) :: Bool -> Bool -> Bool
False ==> _ = True
True ==> x = x

-- Testable

data Result =
  Result { args     :: [Term]
         , showArgs :: [Term -> String]
         , apply    :: [Term] -> Property
         }

data P = P (Int -> Int -> Result)

run :: Testable a => ([Term] -> a) -> Int -> Int -> Result
run a = f where P f = property a

class Testable a where
  property :: ([Term] -> a) -> P

instance Testable Bool where
  property apply = P $ \n d -> Result [] [] (Bool . apply . reverse)

instance Testable Property where
  property apply = P $ \n d -> Result [] [] (apply . reverse)

instance (Show a, Serial a, Testable b) => Testable (a -> b) where
  property f = P $ \n d ->
    let C t c = series d
        c' = conv c
        r = run (\(x:xs) -> f xs (c' x)) (n+1) d
    in  r { args = Var [n] t : args r, showArgs = (show . c') : showArgs r }

-- Top-level interface

depthCheck :: Testable a => Int -> a -> IO ()
depthCheck d p =
  do n <- refute $ run (const p) 0 d
     putStrLn $ "OK, required " ++ show n ++ " tests at depth " ++ show d

smallCheck :: Testable a => Int -> a -> IO ()
smallCheck d p = mapM_ (`depthCheck` p) [0..d]

test :: Testable a => a -> IO ()
test p = mapM_ (`depthCheck` p) [0..]
