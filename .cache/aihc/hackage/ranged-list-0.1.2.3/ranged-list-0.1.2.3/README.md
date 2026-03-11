# ranged-list

## What's this

This package provides lists whose lengths are determined by the type and
lists whose ranges of lengths are determined by the type.

```haskell
sample1 :: LengthL 3 Integer
sample1 = 1 :. 2 :. 3 :. NilL

sample2 :: LengthR 3 Integer
sample2 = NilR :+ 1 :+ 2 :+ 3

sample3 :: RangeL 2 5 Integer
sample3 = 1 :. 2 :. 3 :.. 4 :.. NilL

sample4 :: RangeR 2 5 Integer
sample4 = NilR :++ 1 :++ 2 :+ 3 :+ 4
```

`LengthL 3 Integer` and `LengthR 3 Integer` are lists who have just 3 `Integer`.
`RangeL 2 5 Integer` and `RangeR 2 5 Integer` are lists whose element numbers
are 2 at minimum and 5 at maximum.
`LengthL 3 Integer` and `RangeL 2 5 Integer` are
pushed or poped a element from left.
`LengthR 3 Integer` and `RangeR 2 5 Integer` are
pushed or poped a element from right.

## Motivation

Suppose you want to take elements from list. You can use `take` like following.

```
xs = take 3 "Hello, world!"
```

The length of `xs` is lesser or equal `3`.
But you cannot use this knowledge when you write next code.
You should check the argument of a next function.

```haskell
fun :: [Char] -> ...
fun [] = ...
fun [x] = ...
fun [x, y] = ...
fun [x, y, z] = ...
fun _ = error "bad argument"
```
If you use `LengthL 3 Char`,
you don't need to mind the argument has more than 3 elements.

```haskell
fun :: LengthL 3 Char -> ...
fun (x :. y :. z :. NilL) = ...
```

## LengthL

### To make rectangles from a number list

Suppose you want to make a value which represent a rectangle.
You have a number list.
The numbers are a left border, a top border, a width and a height of
a rectangle in order.
The numbers of the first rectangle are followed by
the numbers of a second rectangle,
and the numbers of the second rectangle are followed by
the numbers of a third rectangle,
and so on.

```
[left1, top1, width1, height1, left2, top2, width2, height2, left3, ...]
```

The list of numbers defined above are covert to a following list.

```
[Rect left1 top1 width1 height1, Rect left2 top2 width2 height2, Rect left3 ...]
```

The code is following. (View `sample/rectangle.hs`)

```haskell:sample/rectangle.hs
import Data.Length.Length

data Rect = Rect {
	left :: Double, top :: Double,
	width :: Double, height :: Double } derivins Show

makeRect :: Length 4 Double -> Rect
makeRect (l :. t :. w :. h :. NilL) = Rect l t w h

main :: IO ()
main = print $ map makeRect . fst $ chunksL [3, 5, 15, 2, 8, 4, 1, 9, 3, 5]
```

The function `chunksL` return a value of type `([LengthL n a], RangeL 0 (n - 1) a)`.
The first value of this tuple is a list of `n` elements of type `a`.
And the second value of this tuple is rest elements.
The number of the rest elements is `0` at minimum and `n - 1` at maximum.

Try running.

```
% stack ghc sample/rectangle.hs
% ./sample/rectangle
[Rect {left = 3.0, top = 5.0, width = 15.0, height = 2.0},
Rect {left = 8.0, top = 4.0, width = 1.0, height = 9.0)}
```

### To take Word64 from bit list

Let's define function to take a 64 bit word from bit list. (View `sample/word64.hs`)
The language extensions and the import list are following.

```haskell:sample/word64.hs
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DAtaKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import GHC.TypeNats
import Data.Foldable
import Data.List.Length
import Data.List.Range
import Data.Bits
import Data.Word
import Numeric
```

You define function `takeL` to take `n` elements from list.

```haskell:sample/word64.hs
takeL :: (LoosenLMax 0 (n - 1) n, Unfoldr 0 n n, ListToLengthL n) =>
	a -> [a] -> LengthL n a
takeL d = either ((`fillL` d) . loosenLMax) fst . splitL
```

The function `splitL` split a list and get n element lengthed list (`LengthL n a`) and a rest of the list.
If the list does not contain enough elements, then it returns a left value. It is a list of type `RangeL 0 (n - 1) a`.
The function `loosenLMax` convert the type `RangeL 0 (n - 1)` into `RangeL 0 n`.
And the function `fillL` fill the list with default value `d` to get a list `LengthL n a`.
Try it.

```
% stack ghci sample/word64.hs
> :set -XDataKinds
> takeL '@' "Hello, world!" :: LengthL 5 Char
'H' :. ('e' :. ('l' :. ('l' :. ('o' :. NilL))))
> takeL 'W' "Hi!" :: LengthL 5 Char
'H' :. ('i' :. ('!' :. ('@' :. ('@' :. NilL))))
```

You define data type which represent a bit as follow.

```haskell:sample/word64.hs
data Bit = O | I deriving Show

boolToBit :: Bool -> Bit
boolToBit = \case False -> O; True -> I

bitToNum63 :: (Num n, Bits n) => Bit -> n
bitToNum63 = \case O -> 0; I -> 1 `shiftL` 63
```

`O` is 0 and `I` is 1.
Function `boolToBit` converts a value of `Bool` into a value of `Bit`.
Function `bitToNum63` converts a value of `Bit` into a number.
It converte the bit as a 63rd bit.

You define the function which convert a bit list into 64 bit word.

```haskell:sample/word64.hs
bitsToWord64 :: LengthL 64 Bit -> Word64
bitsToWord64 = foldl' (\w b -> w `shiftR` 1 .|. bitToNum63 b) 0
```

It gets a bit from the left end.
It put the bit on a 63rd position of a 64 bit word.
Then it gets a next bit.
It shifts 64 bit word to the right.
And it put the bit on a 63rd position of a 64 bit word.
It continue in the same way.

You define the function which take 64 bit word from a bit list expressed
as string.

```haskell:sample/word64.hs
takeWord64 :: String -> Word64
takeWord64 = bitsToWord64 . takeL O . (boolToBit . (== '*') <$>)
```

The argument of this function is a string.
The string represent a bit sequence.
Character \'\*\' is 1 and character \'.\' is 0.

You define sample string and try it in function `main`.

```haskell:sample/word64.hs
sample1, sample2 :: String
sample1 = "...*..*..*...........*...**********...*************............******"
sample2 = "...*..*..*...........*.."

main :: IO ()
main = do
	putStrLn $ takeWord64 sample1 `showHex` ""
	putStrLn $ takeWord64 sample2 `showHex` ""
```

Try it.

```
% stack ghc sample/word64.hs
% ./sample/word64
8007ffc7fe200248
200248
```

## LengthR

### To push and pop from right

A value of the type `LengthR n a` is a list of values of the type `a`.
The length of the list is `n`.
And you can push and pop an element from right.
Try it. (view `sample/LengthR.hs`)

```haskell:sample/LengthR.hs
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module LengthR where

import Data.List.Length

hello :: LengthR 5 Char
hello = NilR :+ 'h' :+ 'e' :+ 'l' :+ 'l' :+ 'o'
```

The value `hello` is a list of characters which length is `5`.
Let\'s push the character `'!'` from right.

```
% stack ghci sample/LengthR.hs
> hello
((((NilR :+ 'h') :+ 'e') :+ 'l') :+ 'l') :+ 'o'
> hello :+ '!'
(((((NilR :+ 'h') :+ 'e') :+ 'l') :+ 'l') :+ 'o') :+ '!'
```

### To show 4 points of rectangles

#### function `fourPoints` and headers

You want to calculate four points of rectangle
from the left-top point, width and height of the rectangle.
You define function `fourPoints`. (View `sample/fourPointsOfRect.hs`)

```haskell:sample/fourPointsOfRect.hs
fourPoints :: LengthR 4 Double -> LengthR 4 (Double, Double)
fourPoints (NilR :+ l :+ t :+ w :+ h) =
	NilR :+ (l, t) :+ (l + w, t) :+ (l, t + h) :+ (l + w, t + h)
```

You add language extensions and modules to import.

```haskell:sample/fourPointsOfRect.hs
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
	UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=Plugin.TypeCheck.Nat.Simple #-}

import GHC.TypeNats
import Control.Monad.Fix
import Control.Monad.Catch
import Data.List.Length
import Text.Read
```

Try it.

```
% stack ghci sample/fourPointsOfRect.hs
> fourPoints $ NilR :+ 300 :+ 200 :+ 50 :+ 30
(((NilR :+ (300.0,200.0)) :+ (350.0,200.0)) :+ (300.0,230.0)) :+ (350.0,230.0)
```

#### to input values interactively

You want to input values of a left bound, a top bound, a width and a height
interactively.
You want to delete the last value and reinput a new value.
First of all, you define two data type,
`DeleteOr a` and `NothingToDeleteException`.

```haskell:sample/fourPointsOfRect.hs
data DeleteOr a = Delete | Value a deriving Show
data NothingToDeleteException = NothingToDeleteException deriving Show
instance Exception NothingToDeleteException
```

And you define the function `getElems` as a class function.

```haskell:sample/fourPointsOfRect.hs
class GetElems n v where
	getElems :: MonadThrow m =>
		LengthR n a -> m (Maybe (DeleteOr a)) -> m (LengthR (n + v) a)

instance GetElems 0 0 where getElems NilR _ = pure NilR

instance {-# OVERLAPPABLE #-} 1 <= n => GetElems n 0 where
	getElems xs@(_ :+ _) _ = pure xs

instance {-# OVERLAPPABLE #-} GetElems 1 (v - 1) => GetElems 0 v where
	getElems NilR gt = gt >>= \case
		Nothing -> getElems NilR gt
		Just Delete -> throwM NothingToDeleteException
		Just (Value x) -> getElem @1 @(v - 1) (NilR :+ x) gt

instance {-# OVERLAPPABLE #-}
	(1 <= n, GetElems (n - 1) (v + 1), GetElems (n + 1) (v - 1)) =>
	GetElems n v where
	getElems xa@(xs :+ _) gt = gt >>= \case
		Nothing -> getElems xa gt
		Just Delete -> getElems @(n - 1) @(v + 1) xs gt
		Just (Value x) -> getElems @(n + 1) @(v - 1) (xa :+ x) gt
```

##### class GetElems n v

The class function `getElems` has two arguments.
The first argument is a list of values which are already inputed.
The second argument is a monad which returns 3 kinds of values,
a value which represents to delete, a new value to push to the list
or a value which represents to do nothing.

##### instance GetElems 0 0

`n == 0` and `v == 0` means that the function `getElems` get
 a list of no elements and return a list of no elements.

##### instance GetElems n 0

`v == 0` means that the function `getElems` get a list and
return the list as it is.

##### instance GetElems 0 v

`n == 0` means that there are no already inputed elements.
The monad returns 3 kind of values.
If it returns `Nothing`, then it rerun the whole as `getElems NilR gt`.
If it returns `Just Delete`, then `NothingToDeleteException` occurs.
If it returns `Just (Value x)`,
then it set the already-inputed elements to `NilR :+ x` and rerun the whole.

##### instance GetElems n v

The monad `gt` returns 3 kind of values.
If it returns `Nothing`, then rerun the whole as `getElems xa gt`.
If it returns `Just Delete`,
then it remove an element from the already-inputed list
and rerun the whole.
If it returns `Just (Value x)`,
then it set the already-inputed elements to `xa :+ x` and rerun the whole.

##### to try it

Try it.

```
% stack ghci sample/fourPointsOfRect.hs
> :set -XDataKinds -XBlockArguments -XLambdaCase
> getElems NilR (Just . Value <$> getLine) :: IO (LengthR 3 String)
foo
bar
baz
((NilR :+ "foo") :+ "bar") :+ "baz"
> gt = (<$> getLine) \case "" -> Nothing; "d' -> Just Delete; s -> Just (Value s)
> getElems NilR gt :: IO (LengthR 3 String)
foo
bar
d
boo

baz
((NilR :+ "foo") :+ "boo") :+ "baz"
> getElems NilR gt :: IO (LengthR 3 String)
foo
bar
d
d
hoge
piyo
baz
((NilR :+ "hoge") :+ "piyo") :+ "baz"
> getElems NilR gt :: IO (LengthR 3 String)
foo
bar
d
d
d
*** Exception: NothingToDeleteException
```

### function `titles`

You define the function `titles` which show values as string with title.

```haskell:sample/fourPointsOfRect.hs
titles :: (Show a, Applicative (LengthR n)) =>
	Int -> LengthR n String -> LengthR n a -> LengthR n String
titles n ts xs = (\t x -> t ++ replicate (n - length t) ' ' ++ ": " ++ show x)
	<$> ts <*> xs
```

Try it.

```
% stack ghci sample/fourPointsOfRect.hs
> titles 5 (NilR :+ "foo" :+ "bar" :+ "baz") (NilR :+ 123 :+ 456 :+ 789)
((NilR :+ "foo  : 123") :+ "bar  : 456") :+ "baz  : 789"
```

### function `printResult`

You define the function `printResult` which show values expressing a rectangle
and 4 points of rectangle.

```haskell:sample/fourPointsOfRect.hs
printResult :: LengthR 4 Double -> IO ()
printResult r = do
	putStrLn ""
	putStrLn `mapM_` titles 6 t r; putStrLn ""
	putStrLn `mapM_` titles 12 u (fourPoints r); putStrLn ""
	where
	t = NilR :+ "left :+ "top" :+ "width" :+ "height"
	u = NIlR :+ "left-top" :+ "right-top" :+ "left-bottom" :+ "right-bottom"
```

Try it.

```
% stack ghci sample/fourPointsOfRect.hs
> printResult $ NilR :+ 300 :+ 200 :+ 70 :+ 50

left  : 300.0
top   : 200.0
width : 70.0
height: 50.0

left-top    : (300.0,200.0)
right-top   : (370.0,200.0)
left-bottom : (300.0,250.0)
right-bottom: (370.0,250.0)
```

### function `getRect`

You define the function `getRect` which gets user input to make rectangle.

```haskell:sample/fourPointsOfRect.hs
getRect :: forall n . GetElems n (4 - n) =>
	LengthR n Double -> IO (LengthR 4 Double)
getRect xs = (<$) <$> id <*> printRect =<<
	getElems @n @(4 - n) xs ((<$> getLine) \case
		"d" -> Just Delete; l -> Value <*> readMaybe l)
	`catch`
	\(_ :: NothingToDeleteException) ->
		putStrLn *** Nothing to delete." >> getRect @0 NilR
```

It gets a user input with `getLine`.
If it is `"d"`, then it deletes the last input.
If there are nothing to delete, then `NothingToDeleteException` occur.
It catches this exception and shows error message and rerun `getRect`.

### function `main`

You define function `main`.

```haskell:sample/fourPointsOfRect.hs
main :: IO ()
main = getRect NilR >>= fix \go xa@(xs :+ _) -> getLine >>= \case
	"q" -> pure ()
	"d" -> go =<< getRect xs
	_ -> putStrLn "q or d" >> go xa
```

It call function `getRect` with list of `0` elements (`NilR`).
And it repeats function `getRect` with list of `4 - 1` elements (`xs`)
if you input `"d"`.

```
% stack ghc sample/fourPointsOfRect.hs
% ./sample/fourPointsOfRect
500
300
75
50

left  : 500.0
top   : 300.0
width : 75.0
height: 50.0

left-top    : (500.0,300.0)
right-top   : (575.0,300.0)
left-bottom : (500.0,350.0)
right-bottom: (575.0,350.0)

d
d
125
100

left  : 500.0
top   : 300.0
width : 125.0
height: 100.0

left-top    : (500.0,300.0)
right-top   : (625.0,300.0)
left-bottom : (500.0,400.0)
right-bottom: (625.0,400.0)

d
d
d
d
d
*** Nothing to delete.
2000
1500
90
50

left  : 2000.0
top   : 1500.0
width : 90.0
height: 50.0

left-top    : (2000.0,1500.0)
right-top   : (2090.0,1500.0)
left-bottom : (2000.0,1550.0)
right-bottom: (2090.0,1550.0)

q
```

## RangeL and RangeR

### To specify the range of a number of elements of a list

You can specify the range of a number of elements of a list.
There is a data type `RangeL n m a`.
It represents a list which have a type `a` element.
And its length is `n` at minimum and `m` at maximum.

```
% stack ghci
> :module Data.List.Range
> :set -XDataKinds
> 'h' :. 'e' :. 'l' :. 'l' :.. 'o' :.. NilL :: RangeL 3 8 Char
'h' :. ('e' :. ('l' :. ('l' :.. ('o' :.. NilL))))
```

### To get passwords

Suppose you want to get a password
whose length is 8 at minimum and 127 at maximum.
First of all, you define headers.

```haskell:sample/password.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Data.List.Range
import System.IO

import qualified Data.ByteString.Char8 as BSC
```

You define `type Password`.

```haskell:sample/password.hs
type Password = RangeL 8 127 Char
```

It is a list of `Char`.
Its length is 8 at minimum and 127 at maximum.

You define a function `getRangedString`.
It recieves a user input.
It return a just value if the length of the input is within range.
It return a nothing value if the length of the input is out of range.

```haskell:sample/password.hs
getRangedPassword :: Unfoldr 0 n m => IO (Maybe (RangeL n m Char))
getRangedPassword = do
	e <- hGetEcho stdin
	hSetEcho stdin False
	unfoldrMRangeMaybe ((/= '\n') <$> hLookAhead stdin) getChar
		<* hSetEcho stdin e
```

It makes echo of stdin off.
It gets characters until you input `'\n'`.
And it makes echo of stdin on.

```
% stack ghci sample/password.hs
> :set -XDataKinds
> getRangedPassword :: IO (Maybe Password)
(Input "foobarbaz")
Just ('f' :. ('o' :. ('o' :. ('b' :. ('a' :. ('r' :. ('b' :. ('a' :. ('z' :..NilL)))))))))
> getRangedPassword :: IO (Maybe Password)
(Input "foo")
Nothing
> getRangedPassword :: IO (Maybe (RangeL 2 5 Char))
(Input "foobar")
Nothing
> r
```

You want to convert a value of type `Password` into a value of `ByteString`.
You can use other packages if you get password as a value of `ByteString`.

```haskell:sample/password.hs
passwordToByteString :: Password -> BSC.ByteString
passwordToByteString = foldr BSC.cons ""
```

You define function `main` to try it.

```haskell:sample/password.hs
main :: IO ()
main = do
	p <- getRangedPassword
	print p
	maybe (eror "bad password length") BSC.putStrLn $ passwordToByteString <$> p
```

Try it.

```
% stack ghc sample/password.hs
% ./sample/password
(Input "foobarbaz")
Just ('f' :. ('o' :. ('o' :. ('b' :. ('a' :. ('r' :. ('b' :. ('a' :. ('z' :.. NilL)))))))))
foobarbaz
```

### Finger Tree

The next example is Finger Tree.

[Finger Trees: A Simple General-purpose Data Structure](https://www.staff.city.ac.uk/~ross/papers/FingerTree.html)

#### Language Extension and Import List

Let's make headers.

```haskell:sample/fingertree.hs
{-# LANGUAGE ScopedTypeVariables, TypeApplications, InstanceSigs #-}
{-# LANGUAGE DataKinds, TypeOperators #-}You
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
	UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=Plugin.TypeCheck.Nat.Simple #-}

import GHC.TypeNats
import Data.List.Range
```

#### Types

You can describe Finger Tree as follows.

```haskell:sample/fingertree.hs
data FingerTree a
	= Enpty | Single a
	| Deep (DigitL a) (FingerTree (Node a)) (DigitR a)
	deriving Show

type Node = RangeL 2 3
type DigitL = RangeL 1 4
type DigitR = RangeR 1 4
```

A list of type `Node a` contains two or three elements of type `a`.
A list of type `DigitL a` contains one elements at minimum and
four elements at maximum.
A list of type `DigitR a` contains the same number of elements as `DigitL a`.
But you can push and pop a element from right.

#### To push from left

You define the function which Add a new element to the left of the sequence.
First of all you define the function to push an element to a list of type `DigitL a`.

```haskell:sample/fingertree.hs
infixr 5 <||

(<||) :: a -> DigitL a -> Either (DigitL a) (DigitL a, Node a)
a <|| b :. NilL = Left $ a :. b :.. NilL
a <|| b :. c :.. NilL = Left $ a :. b :.. c :.. NilL
a <|| b :. c :.. d :.. NilL = Left $ a :. b :.. c :.. d :.. NilL
a <|| b :. c :.. d :.. e :.. NilL =
	Right (a :. b :.. NilL, c :. d :. e :.. NilL)
```

If the original list has fewer elements than four,
then it return a left value list which contains the added value.
If the original list has just four elements,
then it returns a right value tuple which contain the value of type `DigitL a`
and the value of type `Node a`.

You can define the function which add a new element to the left of the sequence.

```haskell:sample/fingertree.hs
infixr 5 <|

(<|) :: a -> FingerTree a -> FingerTree a
a <| Empty = Single a
a <| Single a = Deep (a :. NilL) Empty (NilR :+ b)
a <| Deep pr m sf = case a <|| pr of
	Left pr' -> Deep pr' m sf
	Right (pr', n3) -> Deep pr' (n3 <| m) sf
```

It pushes three of the elements as a `Node`, leaving two behind.

You also require the liftings of `<|`.

```haskell:sample/fingertree.hs
infixr 5 <|.

(<|.) :: Foldable t => t a -> FingerTree a -> FingerTree a
(<|.) = flip $ foldr (<|)
```

To make finger tree from a list or other foldable structure,
you define a function `toTree`.

```haskell:sample/fingertree.hs
toTree :: Foldable t => t a -> FingerTree a
toTree = (<|. Empty)
```

#### To push from right

Adding to the right end of the sequence is the mirror image of the above.

```haskell:sample/fingertree.hs
infixl 5 ||>, |>, |>.

(||>) :: DigitR a -> a -> Either (DigitR a) (Node a, DigitR a)
NilR :+ a ||> b = Left $ NilR :++ a :+ b
NilR :++ a :+ b ||> c = Left $ NilR :++ a :++ b :+ c
NIlR :++ a :++ b :+ c ||> d = Left $ NilR :++ a :++ b :++ c :+ d
NilR :++ a :++ b :++ c :+ d ||> e =
	Right (a :. b :. c :.. NilL, NilR :++ d :+ e)

(|>) :: FingerTree a -> a -> FingerTree a
Empty |> a = Single a
Single a |> b = Deep (a :. NilL) Empty (NilR :+ b)
Deep pr m sf |> a = case sf ||> a of
	Left sf' -> Deep pr m sf'
	Right (n3, sf') -> Deep pr (m |> n3) sf'

(|>.) :: Foldable t => FingerTree a -> t a -> FingerTree a
(|>.) = foldl (|>)
```

#### To pop from left

To deconstruct a sequence, you define a function `uncons`.

```haskell:sample/fingertree.hs
uncons :: FingerTree a -> Maybe (a, FingerTree a)
uncons Empty = Nothing
uncons (Single x) = Just (x, Empty)
uncons (Deep (a :. pr') m sf) = Just (a, deepL pr' m sf)

deepL :: RangeL 0 3 a -> FingerTree (Node a) -> DigitR a -> FingerTree a
deepL NilL m sf = case uncons m of
	Nothing -> toTree sf
	Just (n, m') -> Deep (loosenL n) m' sf
deepL (a :.. pr) m sf = Deep (loosenL $ a :. pr) m sf
```

Since the prefix `pr` of a `Deep` tree contains at least one element,
you can get its head.
However, the tail of the prefix may be empty,
and thus unsuitable as a first argument to the Deep constructor.
Hence you define a smart constructor that differs from `Deep` by allowing the
prefix to contain zero to three elements,
and in the empty case uses a `uncons` of the middle tree to construct a tree of
the correct shape.

#### Concatenation

First of all you define a function which devide a list into a list of `Node`.
The original list has 3 elements at minimum and 12 elements at maximum.
The returned list has 1 node at minimum and 4 nodes at maximum.
The function has a type like the following.

```haskell
fun :: RangeL 3 12 a -> RangeL 1 4 (Node a)
```

You can define a more general function like the following.

```haskell
fun :: RangeL 3 m a -> RangeL 1 w (Node a)
```

`m` is 3 times `w`.

You define a class.

```haskell:sample/fingertree.hs
class Nodes m w where nodes :: RangeL 3 m a -> RangeL 1 w (Node a)
```

And you define instance when `m` is 3 and `w` is 1.

```haskell:sample/fingertree.hs
instance Nodes 3 1 where nodes = (:. NilL) . loosenL	
```

And you define instance of general case.

```haskell:sample/fingertree.hs
instance {-# OVERLAPPABLE #-} (2 <= w, Nodes (m - 3) (w - 1)) => Nodes m w where
	nodes :: forall a . RangeL 3 m a -> RangeL 1 w (Node a)
	nodes (a :. b :. c :. NilL) = (a :. b :. c :.. NilL) :. NilL
	nodes (a :. b :. c :. d :.. NilL) =
		(a :. b :. NilL) :. (c :. d :. NilL) :.. NilL
	nodes (a :. b :. c :. d :.. e :.. NilL) =
		(a :. b :. c :.. NilL) :. (d :. e :. NilL) :.. NilL
	nodes (a :. b :. c :. d :.. e :.. f :.. xs) =
		(a :. b :. c :.. NilL) .:..
			nodes @(m - 3) @(w - 1) (d :. e :. f :. xs)
```

Try it.

```
% stack ghci sample/fingertree.hs
> :set -XTypeApplications -XDataKinds
> xs = 1 :. 2 :. 3 :. 4 :.. 5 :.. 6 :.. 7 :.. 8 :.. NilL :: RangeL 3 12 Integer
> nodes @12 @4 xs
(1 :. (2 :. (3 :.. NilL))) :. ((4 :. (5 :. (6 :.. NilL))) :.. ((7 :. (8 :. NilL)) :.. NilL))
> :type it
it :: Num a => RangeL 1 4 (Node a)
```

You can combine the two digit argument into a list of Nodes
with the function `nodes`.
You can obtain a recursive function by
generalizing the concatenation function to take an additional list of elements.

```haskell:sample/fingertree.hs
app3 :: FingerTree a -> RangeL 1 4 a -> FingerTree a -> FingerTree a
app3 Empty m xs = m <|. xs
app3 xs m Empty = xs |>. m
app3 (Single x) m xs = x <| m <|. xs
app3 xs m (Single x) = xs |>. m |> x
app3 (Deep pr1 m1 sf1) m (Deep pr2 m2 sf2) =
	Deep pr1 (app3 m1 (nodes $ sf1 ++.. m ++. pr2) m2) sf2
```

To concatenate two finger trees, you take a head element from a second sequence.

```haskell:sample/fingertree.hs
(><) :: FingerTree a -> FingerTree a -> FingerTree a
l >< r = case uncons r of Nothing -> l; Just (x, r') -> app3 l (x :. NilL) r'
```
