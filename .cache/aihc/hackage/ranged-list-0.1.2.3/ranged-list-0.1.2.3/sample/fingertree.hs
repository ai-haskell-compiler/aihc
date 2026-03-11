{-# LANGUAGE ScopedTypeVariables, TypeApplications, InstanceSigs #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
	UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=Plugin.TypeCheck.Nat.Simple #-}

import GHC.TypeNats
import Data.List.Range

data FingerTree a
	= Empty | Single a
	| Deep (DigitL a) (FingerTree (Node a)) (DigitR a)
	deriving Show

type Node = RangeL 2 3
type DigitL = RangeL 1 4
type DigitR = RangeR 1 4

infixr 5 <||

(<||) :: a -> DigitL a -> Either (DigitL a) (DigitL a, Node a)
a <|| b :. NilL = Left $ a :. b :.. NilL
a <|| b :. c :.. NilL = Left $ a :. b :.. c :.. NilL
a <|| b :. c :.. d :.. NilL = Left $ a :. b :.. c :.. d :.. NilL
a <|| b :. c :.. d :.. e :.. NilL =
	Right (a :. b :.. NilL, c :. d :. e :.. NilL)

infixr 5 <|

(<|) :: a -> FingerTree a -> FingerTree a
a <| Empty = Single a
a <| Single b = Deep (a :. NilL) Empty (NilR :+ b)
a <| Deep pr m sf = case a <|| pr of
	Left pr' -> Deep pr' m sf
	Right (pr', n3) -> Deep pr' (n3 <| m) sf

infixr 5 <|.

(<|.) :: Foldable t => t a -> FingerTree a -> FingerTree a
(<|.) = flip $ foldr (<|)

toTree :: Foldable t => t a -> FingerTree a
toTree = (<|. Empty)

infixl 5 ||>, |>, |>.

(||>) :: DigitR a -> a -> Either (DigitR a) (Node a, DigitR a)
NilR :+ a ||> b = Left $ NilR :++ a :+ b
NilR :++ a :+ b ||> c = Left $ NilR :++ a :++ b :+ c
NilR :++ a :++ b :+ c ||> d = Left $ NilR :++ a :++ b :++ c :+ d
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

uncons :: FingerTree a -> Maybe (a, FingerTree a)
uncons Empty = Nothing
uncons (Single x) = Just (x, Empty)
uncons (Deep (a :. pr') m sf) = Just (a, deepL pr' m sf)

deepL :: RangeL 0 3 a -> FingerTree (Node a) -> DigitR a -> FingerTree a
deepL NilL m sf = case uncons m of
	Nothing -> toTree sf
	Just (n, m') -> Deep (loosenL n) m' sf
deepL (a :.. pr) m sf = Deep (loosenL $ a :. pr) m sf

class Nodes m w where nodes :: RangeL 3 m a -> RangeL 1 w (Node a)

instance Nodes 3 1 where nodes = (:. NilL) . loosenL

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

app3 :: FingerTree a -> RangeL 1 4 a -> FingerTree a -> FingerTree a
app3 Empty m xs = m <|. xs
app3 xs m Empty = xs |>. m
app3 (Single x) m xs = x <| m <|. xs
app3 xs m (Single x) = xs |>. m |> x
app3 (Deep pr1 m1 sf1) m (Deep pr2 m2 sf2) =
	Deep pr1 (app3 m1 (nodes $ sf1 ++.. m ++. pr2) m2) sf2

(><) :: FingerTree a -> FingerTree a -> FingerTree a
l >< r = case uncons r of Nothing -> l; Just (x, r') -> app3 l (x :. NilL) r'

main :: IO ()
main = do
	let	h = toTree "Hello, "
		w = toTree "world!"
	print h
	print $ uncons h
	print $ h >< w
