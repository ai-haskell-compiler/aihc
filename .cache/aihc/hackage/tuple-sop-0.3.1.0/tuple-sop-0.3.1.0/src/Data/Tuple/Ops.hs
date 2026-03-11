{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE RankNTypes             #-}
{-|
This module exports various functions that work with n-ary tuples.

Generally speaking all functions can be applied to tuples of different sizes.

However, to improve type inference we have limited our implementation to only only work on tuples of at most size 10.

For functions working with indexes we have also added some convenience functions to avoid usage of Proxy's.
-}
module Data.Tuple.Ops
  ( -- * Selection
    sel
  , selN
    -- ** convenience functions 
  , sel1
  , sel2
  , sel3
  , sel4
  , sel5
  , sel6
  , sel7
  , sel8
  , sel9
  , sel10
  , lastT
  -- * Application
  , app
  , appPoly
  , appN
  , appF
  , mapT
  , mapPolyT
  -- ** convenience functions 
  , app1
  , app2
  , app3
  , app4
  , app5
  , app6
  , app7
  , app8
  , app9
  , app10
  -- * Constructing tuples
  , consT
  , snocT
  , appendT
  , reverseT
  , initT
  , tailT
  -- * Insertion
  , ins
  , insN
  , ins1
  , ins2
  , ins3
  , ins4
  , ins5
  , ins6
  , ins7
  , ins8
  , ins9
  , ins10
  -- * Deletion
  , del
  , delN
  -- ** convenience functions
  , del1
  , del2
  , del3
  , del4
  , del5
  , del6
  , del7
  , del8
  , del9
  , del10
  -- * Folding
  , foldlT
  , foldrT
  -- * Currying
  , uncurryT
  , curryT) where

import Generics.SOP
import GHC.Exts
import GHC.TypeLits

class Select s t where
  -- | Takes an n-ary tuple and returns the first element of the requested type
  --
  -- >>> sel (1,'d','c') :: Char
  -- 'd'
  sel :: s -> t

instance (GenericNP s rep_s, GSelect (RepNP s) t) => Select s t where
  sel s = gsel (from_np s)

class GSelect s t where
  gsel :: s -> t

instance {-# OVERLAPPING #-} GSelect (NP I (t ': xs)) t where
  gsel = unI . hd

instance {-# OVERLAPPING #-} GSelect (NP I xs) t => GSelect (NP I (a ': xs)) t where
  gsel = gsel . tl

class SelectN s (n :: Nat) t | s n -> t where
  -- | Takes an n-ary tuple and a `Proxy` carrying a `Nat`, returns the element with the index specified by the @Nat@
  --
  -- >>> selN (1,'d',False,'c') (Proxy :: Proxy 2)
  -- False
  selN :: s -> Proxy n -> t

instance (GenericNP s rep_s, GSelectN (RepNP s) (Lit n) t) => SelectN s n t where
  selN s Proxy = gselN (from_np s) (Proxy :: Proxy (Lit n))

class GSelectN s (n :: Nat') t | s n -> t where
  gselN :: s -> Proxy n -> t

instance GSelectN (NP I (t ': xs)) Z' t where
  gselN np _ = unI $ hd np

instance GSelectN (NP I xs) n t => GSelectN (NP I (a ': xs)) (S' n) t where
  gselN np _ = gselN (tl np) (Proxy :: Proxy n)


-- | Selects the first element in an n-ary tuple
--
-- >>> sel1 (0,'d','c')
-- 0
sel1 :: SelectN s 0 t => s -> t
sel1 s = selN s (Proxy :: Proxy 0)
sel2 :: SelectN s 1 t => s -> t
sel2 s = selN s (Proxy :: Proxy 1)
sel3 :: SelectN s 2 t => s -> t
sel3 s = selN s (Proxy :: Proxy 2)
sel4 :: SelectN s 3 t => s -> t
sel4 s = selN s (Proxy :: Proxy 3)
sel5 :: SelectN s 4 t => s -> t
sel5 s = selN s (Proxy :: Proxy 4)
sel6 :: SelectN s 5 t => s -> t
sel6 s = selN s (Proxy :: Proxy 5)
sel7 :: SelectN s 6 t => s -> t
sel7 s = selN s (Proxy :: Proxy 6)
sel8 :: SelectN s 7 t => s -> t
sel8 s = selN s (Proxy :: Proxy 7)
sel9 :: SelectN s 8 t => s -> t
sel9 s = selN s (Proxy :: Proxy 8)
sel10 :: SelectN s 9 t => s -> t
sel10 s = selN s (Proxy :: Proxy 9)

-- | Selects the last element of any n-ary tuple
--
-- >>> lastT (1,2,3,4)
-- 4
--
-- >>> lastT (1,2,3)
-- 3
lastT :: forall s n t. (LengthT s ~ n, SelectN s (n - 1) t) => s -> t
lastT s = selN s (Proxy :: Proxy (n - 1))

class TailT s t | s -> t where
  -- | Takes an n-ary tuple and returns the same tuple minus the first element.
  --
  -- >>> tailT (1,2,3,4)
  -- (2,3,4)
  --
  -- Works only only tuples of size at least 3
  --
  -- >>> tailT (1,2)
  -- Couldn't match type `2 ':<= 3' with `2 ':>= 3'
  tailT :: s -> t

instance (GenericNP s rep_s, GenericNP t rep_t, LEQ (LengthT s) 3, GTailT rep_s rep_t) => TailT s t where
  tailT = to_np . gtailT . from_np
  
class GTailT s t | s -> t where
  gtailT :: s -> t

instance GTailT (NP I (a ': xs)) (NP I xs) where
  gtailT (_ :* xs) = xs

class InitT s t | s -> t where
  -- | Takes an n-ary tuple and returns the same tuple minus the first element.
  --
  -- >>> initT (1,2,3,4)
  -- (1,2,3)
  --
  -- Works only only tuples of size at least 3
  --
  -- >>> initT (1,2)
  -- Couldn't match type `2 ':<= 3' with `2 ':>= 3'
  initT :: s -> t

instance (GenericNP s rep_s, GenericNP t rep_t, LEQ (LengthT s) 3, GInitT rep_s rep_t) => InitT s t where
  initT = to_np . ginitT . from_np

class GInitT s t | s -> t where
  ginitT :: s -> t

instance GInitT (NP I '[c]) (NP I '[]) where
  ginitT _ = Nil :: NP I '[]

instance GInitT (NP I (b ': xs)) (NP I xs') => GInitT (NP I (a ': b ': xs)) (NP I (a ': xs')) where
  ginitT (a :* b :* xs) = a :* ginitT (b :* xs)

class App f s t where
  -- | Applies a monomorphic function to the first element of an n-ary tuple that matches the type of the argument of the function.
  --
  -- >>> app not ('d',True)
  -- ('d',False)
  --
  -- Sometimes it is necessary to specify the result type, such that the function becomes monomorphic
  -- >>> app (+1) (True,5) :: (Bool,Integer)
  -- (True,6)
  --
  -- One may also use `appPoly`, which doesn't require specifying the result type. However it can only apply functions
  -- to the first element of an n-ary tuple.
  app :: f -> s -> t

instance (GenericNP s rep_s, GenericNP t rep_t, Applicable f rep_s ~ app, GApp f app rep_s rep_t) => App f s t where
  app f s = to_np $ gapp f (Proxy :: Proxy app) (from_np s)

class GApp f (app :: [Bool]) s t | f s app -> t where
  gapp :: f -> Proxy app -> s -> t

instance (a ~ a', b ~ b') => GApp (Poly a b) ('True ': app) (NP I (a' ': xs)) (NP I (b' ': xs)) where
  gapp (Poly f) _ (I t :* xs) = I (f t) :* xs

instance GApp (a -> b) ('True ': app) (NP I (a ': xs)) (NP I (b ': xs)) where
  gapp f _ (I t :* xs) = I (f t) :* xs

instance GApp f app (NP I xs) (NP I xs') => GApp f ('False ': app) (NP I (c ': xs)) (NP I (c ': xs')) where
  gapp f _ (c :* xs) = c :* gapp f (Proxy :: Proxy app) xs

-- | Applies a polymorphic function to the first element of an n-ary tuple. Since the function is polymorphic in its argument it can always be applied to the first element of a tuple.
--
-- >>> appPoly show (5,False)
-- ("5",False)
appPoly :: App (Poly a b) s t => (a -> b) -> s -> t
appPoly f s = app (poly f) s
  
class AppN f s (n :: Nat) t | f s n -> t where
  -- | Applies a function to the element at index @n@ in an n-ary tuple.
  --
  -- >>> appN not (Proxy 2) (False,True,False)
  -- (False,True,True)
  --
  -- `appN` also works for polymorphic functions
  --
  -- >>> appN show (5,'c',False) (Proxy :: Proxy 2)
  -- (5,'c',"False")
  appN :: f -> s -> Proxy n -> t

instance (GenericNP s rep_s, GenericNP t rep_t, GAppN f rep_s (Lit n) rep_t) => AppN f s n t where
  appN f s Proxy = to_np $ gappN f (from_np s) (Proxy :: Proxy (Lit n))

class GAppN f s (n :: Nat') t | f s n -> t where
  gappN :: f -> s -> Proxy n -> t

instance (a ~ a', b ~ b') => GAppN (a -> b) (NP I (a' ': xs)) Z' (NP I (b' ': xs)) where
  gappN f (I a :* xs) _ = I (f a) :* xs

instance GAppN f (NP I xs) n (NP I xs') => GAppN f (NP I (c ': xs)) (S' n) (NP I (c ': xs')) where
  gappN f (c :* xs) _ = c :* gappN f xs (Proxy :: Proxy n)

-- | Applies a function to the first element of an n-ary tuple
--
-- >>> app1 (+1) (5,6,7)
-- (6,6,7)
app1 :: AppN f s 0 t => f -> s -> t
app1 f s = appN f s (Proxy :: Proxy 0)
app2 :: AppN f s 1 t => f -> s -> t
app2 f s = appN f s (Proxy :: Proxy 1)
app3 :: AppN f s 2 t => f -> s -> t
app3 f s = appN f s (Proxy :: Proxy 2)
app4 :: AppN f s 3 t => f -> s -> t
app4 f s = appN f s (Proxy :: Proxy 3)
app5 :: AppN f s 4 t => f -> s -> t
app5 f s = appN f s (Proxy :: Proxy 4)
app6 :: AppN f s 5 t => f -> s -> t
app6 f s = appN f s (Proxy :: Proxy 5)
app7 :: AppN f s 6 t => f -> s -> t
app7 f s = appN f s (Proxy :: Proxy 6)
app8 :: AppN f s 7 t => f -> s -> t
app8 f s = appN f s (Proxy :: Proxy 7)
app9 :: AppN f s 8 t => f -> s -> t
app9 f s = appN f s (Proxy :: Proxy 8)
app10 :: AppN f s 9 t => f -> s -> t
app10 f s = appN f s (Proxy :: Proxy 9)

class AppF f s t | f s -> t where
  -- | Apply an n-ary function to an n-ary tuple. The function takes an argument for each component of the tuple in left-to-right order.
  --
  -- >>> appF (\a b c -> if a then b else c) (False,1,2)
  -- 2
  appF :: f -> s -> t

instance (GenericNP s rep_s, GAppF f rep_s t) => AppF f s t where
  appF f = gappF f . from_np

class GAppF f s t | f s -> t where
  gappF :: f -> s -> t

instance b ~ b' => GAppF b (NP I '[]) b' where
  gappF f Nil = f

instance (a ~ a', GAppF b (NP I xs) c) => GAppF (a -> b) (NP I (a' ': xs)) c where
  gappF f (I a :* xs) = gappF (f a) xs

class MapT f s t | f s -> t where
  -- | Maps a monomorphic function over each element in an n-ary tuple that matches the type of the argument of the function
  --
  -- >>> mapT not (True,'c',False)
  -- (False,'c',True)
  --
  -- Sometimes it is necessary to specify the result type.
  --
  -- >>> mapT (+1) (5,6,7,False) :: (Integer,Integer,Integer,Bool)
  -- (6,7,8,False)
  --
  -- Using `mapPolyT` this is not necessary. However, to use `mapPolyT` the tuple may only contains elements of a single type.
  mapT :: f -> s -> t

instance (GenericNP s rep_s, GenericNP t rep_t, Applicable f rep_s ~ app, GMapT f app rep_s rep_t) => MapT f s t where
  mapT f s = to_np $ gmapT f (Proxy :: Proxy app) (from_np s)

class GMapT f (app :: [Bool]) s t | f app s -> t where
  gmapT :: f -> Proxy app -> s -> t

instance GMapT f '[] (NP I '[]) (NP I '[]) where
  gmapT _ _ = id

instance GMapT (a -> b) apps (NP I xs) (NP I xs') => GMapT (a -> b) ('True ': apps) (NP I (a ': xs)) (NP I (b ': xs')) where
  gmapT f _ (I a :* xs) = I (f a) :* gmapT f (Proxy :: Proxy apps) xs

instance (a ~ a', b ~ b', GMapT (Poly a b) apps (NP I xs) (NP I xs')) => GMapT (Poly a b) ('True ': apps) (NP I (a' ': xs)) (NP I (b' ': xs')) where
  gmapT p@(Poly f) _ (I a :* xs) = I (f a) :* gmapT p (Proxy :: Proxy apps) xs

instance GMapT f apps (NP I xs) (NP I xs') => GMapT f ('False ': apps) (NP I (c ': xs)) (NP I (c ': xs')) where
  gmapT f _ (c :* xs) = c :* gmapT f (Proxy :: Proxy apps) xs

-- | Applies a polymorphic function to each element in an n-ary tuple. Requires all elements in the tuple to be of the same type.
--
-- >>> mapPolyT (+1) (5,6,7,8)
-- (6,7,8,9)
--
-- >>> mapPolyT (+1) (5,6,7,False)
-- No instance for (Num Bool) arising from the literal `5'
mapPolyT :: MapT (Poly a b) s t => (a -> b) -> s -> t
mapPolyT f s = mapT (poly f) s

class ConsT a s t | a s -> t where
  -- | Adds an element to the head of an n-ary tuple
  --
  -- >>> consT 5 (True,'c')
  -- (5,True,'c')
  consT :: a -> s -> t

instance (GenericNP s rep_s, GenericNP t rep_t, GConsT a rep_s rep_t) => ConsT a s t where
  consT a s = to_np $ gconsT a (from_np s)

class GConsT a s t | a s -> t where
  gconsT :: a -> s -> t

instance GConsT a (NP I xs) (NP I (a ': xs)) where
  gconsT a xs = I a :* xs

class SnocT a s t | a s -> t where
  -- | Adds an element to the back of an n-ary tuple
  --
  -- >>> snocT 5 (True,'c')
  -- (True,'c',5)
  snocT :: a -> s -> t

instance (GenericNP s rep_s, GenericNP t rep_t, GSnocT a rep_s rep_t) => SnocT a s t where
  snocT a s = to_np $ gsnocT a (from_np s)

class GSnocT a s t | a s -> t where
  gsnocT :: a -> s -> t

instance GSnocT a (NP I '[]) (NP I '[a]) where
  gsnocT a Nil = I a :* Nil

instance GSnocT a (NP I xs) (NP I xs') => GSnocT a (NP I (b ': xs)) (NP I (b ': xs')) where
  gsnocT a (b :* xs) = b :* gsnocT a xs

class Insert a s t where
  -- | Inserts an element into an n-ary tuple. Its position is determined by the target type
  --
  -- >>> ins 5 ('c', False) :: (Char, Integer, Bool)
  -- ('c',5,False)
  --
  -- An element will only be inserted at a specific position if:
  --
  -- * The element type matches the component type
  -- * The tail of the tuple remains well-typed
  --
  -- >>> ins 5 ('c',1,False) :: (Char, Integer, Bool, Integer)
  -- ('c',1,False,5)
  --
  -- In the above example, inserting 5 after 'c' would make the tail of the tuple have type (Integer, Bool) when the target type asks for (Bool,Integer)
  --
  -- When attempting to insert an element before or after a component of the same type the element is always inserted in front
  --
  -- >>> ins 5 ('c',1,False) :: (Char, Integer, Integer, Bool)
  -- ('c',5,1,False)
  ins :: a -> s -> t

instance (GenericNP s rep_s, GenericNP t rep_t, GInsert (InsLoc a rep_s rep_t) a rep_s rep_t) => Insert a s t where
  ins a = to_np . gins (Proxy :: Proxy (InsLoc a rep_s rep_t)) a . from_np

class GInsert loc a s t where
  gins :: Proxy loc -> a -> s -> t

instance {-# OVERLAPPING #-} GInsert Z' a (NP I xs) (NP I (a ': xs)) where
  gins _ a s = I a :* s

instance {-# OVERLAPPABLE #-} (b ~ c, GInsert n a (NP I xs) (NP I xs')) => GInsert (S' n) a (NP I (b ': xs)) (NP I (c ': xs')) where
  gins _ a (b :* xs) = b :* gins (Proxy :: Proxy n) a xs


class InsertN a s n t | a s n -> t where
  -- | Inserts an element at an index specified position into an n-ary tuple
  --
  -- >>> insN 5 ('c',1,False) (Proxy :: Proxy 1)
  -- ('c',5,1,False)
  insN :: a -> s -> Proxy n -> t

instance (GenericNP s rep_s, GenericNP t rep_t, GInsertN a rep_s (Lit n) rep_t) => InsertN a s n t where
  insN a s p = to_np $ ginsN a (from_np s) (Proxy :: Proxy (Lit n))

class GInsertN a s n t | a s n -> t where
  ginsN :: a -> s -> Proxy n -> t

instance GInsertN a (NP I xs) Z' (NP I (a ': xs)) where
  ginsN a s _ = I a :* s

instance GInsertN a (NP I xs) n (NP I xs') => GInsertN a (NP I (b ': xs)) (S' n) (NP I (b ': xs')) where
  ginsN a (b :* s) _ = b :* ginsN a s (Proxy :: Proxy n)

-- | Inserts an element in head position into an n-ary tuple
--
-- >>> ins1 5 ('c',1,False)
-- (5,'c',1,False)
ins1 :: InsertN a s 0 t => a -> s -> t
ins1 a s = insN a s (Proxy :: Proxy 0)
ins2 :: InsertN a s 1 t => a -> s -> t
ins2 a s = insN a s (Proxy :: Proxy 1) 
ins3 :: InsertN a s 2 t => a -> s -> t
ins3 a s = insN a s (Proxy :: Proxy 2) 
ins4 :: InsertN a s 3 t => a -> s -> t
ins4 a s = insN a s (Proxy :: Proxy 3) 
ins5 :: InsertN a s 4 t => a -> s -> t
ins5 a s = insN a s (Proxy :: Proxy 4) 
ins6 :: InsertN a s 5 t => a -> s -> t
ins6 a s = insN a s (Proxy :: Proxy 5) 
ins7 :: InsertN a s 6 t => a -> s -> t
ins7 a s = insN a s (Proxy :: Proxy 6) 
ins8 :: InsertN a s 7 t => a -> s -> t
ins8 a s = insN a s (Proxy :: Proxy 7) 
ins9 :: InsertN a s 8 t => a -> s -> t
ins9 a s = insN a s (Proxy :: Proxy 8) 
ins10 :: InsertN a s 9 t => a -> s -> t
ins10 a s = insN a s (Proxy :: Proxy 9) 

class Delete s t where
  -- | Deletes the first element in an n-ary tuple whose type does not exist in the target type
  --
  -- >>> del ('c',False,5) :: (Char,Bool)
  -- ('c',False)
  del :: s -> t

instance (GenericNP s rep_s, GenericNP t rep_t, LEQ (LengthT s) 3, GDelete rep_s rep_t) => Delete s t where
  del = to_np . gdel . from_np

class GDelete s t where
  gdel :: s -> t

instance GDelete (NP I (a ': xs)) (NP I xs) where
  gdel (a :* xs) = xs

instance GDelete (NP I xs) (NP I xs') => GDelete (NP I (a ': xs)) (NP I (a ': xs')) where
  gdel (a :* xs) = a :* gdel xs

class DeleteN s (n :: Nat) t | s n -> t where
  -- | Deletes an element specified by an index in an n-ary tuple
  --
  -- >>> delN ('c',False,5) (Proxy :: Proxy 1)
  -- ('c',5)
  delN :: s -> Proxy n -> t

instance (GenericNP s rep_s, GenericNP t rep_t, LEQ (LengthT s) 3, GDeleteN rep_s (Lit n) rep_t) => DeleteN s n t where
  delN s Proxy = to_np $ gdelN (from_np s) (Proxy :: Proxy (Lit n))

class GDeleteN s (n :: Nat') t | s n -> t where
  gdelN :: s -> Proxy n -> t

instance GDeleteN (NP I (t ': xs)) Z' (NP I xs) where
  gdelN (_ :* xs) _ = xs

instance GDeleteN (NP I xs) n (NP I xs') => GDeleteN (NP I (a ': xs)) (S' n) (NP I (a ': xs')) where
  gdelN (a :* xs) _ = a :* gdelN xs (Proxy :: Proxy n) 

-- | Deletes the first element of an n-ary tuple
-- 
-- >>> del1 ('c',False,5)
-- (False,5)
del1 :: DeleteN s 0 t => s -> t
del1 s = delN s (Proxy :: Proxy 0)
del2 :: DeleteN s 1 t => s -> t
del2 s = delN s (Proxy :: Proxy 1)
del3 :: DeleteN s 2 t => s -> t
del3 s = delN s (Proxy :: Proxy 2)
del4 :: DeleteN s 3 t => s -> t
del4 s = delN s (Proxy :: Proxy 3)
del5 :: DeleteN s 4 t => s -> t
del5 s = delN s (Proxy :: Proxy 4)
del6 :: DeleteN s 5 t => s -> t
del6 s = delN s (Proxy :: Proxy 5)
del7 :: DeleteN s 6 t => s -> t
del7 s = delN s (Proxy :: Proxy 6)
del8 :: DeleteN s 7 t => s -> t
del8 s = delN s (Proxy :: Proxy 7)
del9 :: DeleteN s 8 t => s -> t
del9 s = delN s (Proxy :: Proxy 8)
del10 :: DeleteN s 9 t => s -> t
del10 s = delN s (Proxy :: Proxy 9)

class FoldLeft f s t | f s -> t where
  -- | Fold left for n-ary tuples
  --
  -- >>> foldlT (-) 0 (4,3,2,1)
  -- -10
  foldlT :: f -> t -> s -> t

instance (GenericNP s rep_s, GFoldLeft f rep_s t) => FoldLeft f s t where
  foldlT f b = gfoldlT f b . from_np

class GFoldLeft f s t | f s -> t where
  gfoldlT :: f -> t -> s -> t

instance GFoldLeft (b -> a -> b) (NP I '[]) b where
  gfoldlT f b Nil = b

instance (a ~ a', GFoldLeft (b -> a -> b) (NP I xs) b) => GFoldLeft (b -> a -> b) (NP I (a' ': xs)) b where
  gfoldlT f b (I a :* xs) = gfoldlT f (f b a) xs

class FoldRight f s t | f s -> t where
  -- | Fold right for n-ary tuples
  --
  -- >>> foldrT (-) 0 (4,3,2,1)
  -- 2
  foldrT :: f -> t -> s -> t

instance (GenericNP s rep_s, GFoldRight f rep_s t) => FoldRight f s t where
  foldrT f b = gfoldrT f b . from_np

class GFoldRight f s t | f s -> t where
  gfoldrT :: f -> t -> s -> t

instance GFoldRight (a -> b -> b) (NP I '[]) b where
  gfoldrT f b Nil = b

instance (a ~ a', b ~ b', GFoldRight (a -> b' -> b) (NP I xs) b) => GFoldRight (a -> b' -> b) (NP I (a' ': xs)) b where
  gfoldrT f b (I a :* xs) = f a $ gfoldrT f b xs

class FoldLeftF f s t | f s -> t where
  foldlF :: f -> s -> t

-- Currently broken. So not exported until I can properly fix it.
class FlattenT s t | s -> t where
  -- | Compresses sub-tuples into their paren-tuples
  --
  -- >>> flattenT (5,6,(1,2,(3,4)))
  --  
  flattenT :: s -> t

instance (GenericNP s rep_s, GenericNP t rep_t, GFlattenT (AreProducts rep_s) rep_s rep_t) => FlattenT s t where
  flattenT = to_np . gflattenT (Proxy :: Proxy (AreProducts (RepNP s))) . from_np
  
class GFlattenT (ps :: [Bool]) s t | ps s -> t where
  gflattenT :: Proxy ps -> s -> t

instance GFlattenT '[] (NP I '[]) (NP I '[]) where
  gflattenT _ = id

instance (GenericNP x rep_x, GFlattenT (AreProducts rep_x) rep_x x', GFlattenT ps (NP I xs) (NP I xs'), GAppendT x' (NP I xs') (NP I xss)) => GFlattenT ('True ': ps) (NP I (x ': xs)) (NP I xss) where
  gflattenT _ (I x :* xs) = case (gflattenT (Proxy :: Proxy (AreProducts rep_x)) $ from_np x, gflattenT (Proxy :: Proxy ps) xs) of
    (x', xs') -> gappendT x' xs'

instance GFlattenT ps (NP I xs) (NP I xs') => GFlattenT ('False ': ps) (NP I (x ': xs)) (NP I (x ': xs')) where
  gflattenT _ (x :* xs) = x :* gflattenT (Proxy :: Proxy ps) xs

class AppendT s r t | s r -> t where
  -- | Appends two n-ary tuple into one larger tuple
  --
  -- >>> appendT (5,'c') ('d',False)
  -- (5,'c','d',False)
  appendT :: s -> r -> t

instance (GenericNP s rep_s, GenericNP r rep_r, GenericNP t rep_t, GAppendT rep_s rep_r rep_t) => AppendT s r t where
  appendT s r = to_np $ gappendT (from_np s) (from_np r)

class GAppendT s r t | s r -> t where
  gappendT :: s -> r -> t

instance GAppendT (NP I '[]) ys ys where
  gappendT _ = id

instance GAppendT (NP I xs) ys (NP I zs) => GAppendT (NP I (x ': xs)) ys (NP I (x ': zs)) where
  gappendT (x :* xs) ys = x :* gappendT xs ys

class ReverseT s t | s -> t where
  -- | Reverses the order of elements in an n-ary tuple
  --
  -- >>> reverseT (1,2,3,4)
  -- (4,3,2,1)
  reverseT :: s -> t

instance (GenericNP s rep_s, GenericNP t rep_t, GReverseT rep_s (NP I '[]) rep_t) => ReverseT s t where
  reverseT s = to_np $ greverseT (from_np s) (Nil :: NP I '[])

class GReverseT s r t | s r -> t where
  greverseT :: s -> r -> t

instance GReverseT (NP I '[]) xs xs where
  greverseT _ = id

instance GReverseT (NP I xs) (NP I (x ': ys)) zs => GReverseT (NP I (x ': xs)) (NP I ys) zs where
  greverseT (x :* xs) ys = greverseT xs (x :* ys)

class UnCurryT s t b | s t -> b where
  -- | Converts a curried function to a function that works on n-ary tuples
  -- 
  -- >>> uncurryT (\a b c -> a + b + c) (1,2,3)
  -- 6
  uncurryT :: s -> t -> b

instance (GenericNP t rep_t, GUnCurryT s rep_t b) => UnCurryT s t b where
  uncurryT f t = guncurryT f (from_np t)

class GUnCurryT s t b | s t -> b where
  guncurryT :: s -> t -> b

instance b ~ b' => GUnCurryT b (NP I '[]) b' where
  guncurryT f Nil = f

instance (a ~ a', GUnCurryT c (NP I xs) b') => GUnCurryT (a -> c) (NP I (a' ': xs)) b' where
  guncurryT f (I x :* xs) = guncurryT (f x) xs

class CurryT s t | s -> t where
  -- | Converts a function that works on n-ary tuples to a curried function
  --
  -- >>> curryT (\(a,b,c) -> a + b + c) 1 2 3 :: Integer
  -- 6
  --
  -- Currently, type inference is partially broken for this function
  curryT :: s -> t 

instance (FuncToGen s (NP I xs -> b), ToFun xs b ~ t, GCurryT (NP I xs -> b) (NP I '[]) xs t) => CurryT s t where
  curryT s = gcurryT (funcToGen s) (Nil :: NP I '[]) (Proxy :: Proxy xs)

class GCurryT s d (p :: [*]) t | s d p -> t where
  gcurryT :: s -> d -> Proxy p -> t 

instance (b ~ b', ys ~ Reverse xs '[], GReverse (NP I xs) (NP I '[]) (NP I ys)) => GCurryT (NP I ys -> b) (NP I xs) '[] b' where
  gcurryT f d _ = f $ greverse d (Nil :: NP I '[])

instance (f ~ (NP I ys -> c), Head (Diff ys (Reverse xs '[])) ~ a, ToFun (Tail (Diff ys (Reverse xs '[]))) c ~ b, GCurryT f (NP I (a ': xs)) ps b) => GCurryT f (NP I xs) (a ': ps) (a -> b) where
  gcurryT t xs _ = \a -> gcurryT t (I a :* xs) (Proxy :: Proxy ps)

class RepNP s ~ rep => GenericNP s rep | s -> rep, rep -> s where
  type RepNP s :: *
  from_np :: RepNP s ~ rep => s -> rep
  to_np :: RepNP s ~ rep => rep -> s

instance (Generic s, Rep s ~ SOP I '[xs], RepNP s ~ rep, ToTuple rep ~ s) => GenericNP s rep where
  type RepNP s = ToNP (Rep s)
  from_np s = gtoNP $ from s
  to_np p = to $ gfromNP p

type family ToNP s where
  ToNP (SOP I '[xs]) = NP I xs

gtoNP :: (SOP I '[xs]) -> NP I xs
gtoNP (SOP (Z np)) = np

gfromNP :: NP I xs -> SOP I '[xs]
gfromNP np = SOP $ Z np

class FuncToGen s t | s -> t where
  funcToGen :: s -> t

instance {-# OVERLAPPING #-} b ~ b' => FuncToGen b b' where
  funcToGen = id

instance {-# OVERLAPPING #-} (GenericNP a rep_a, rep_a ~ g, FuncToGen b b') => FuncToGen (a -> b) (g -> b') where
  funcToGen f = \s -> funcToGen (f $ to_np s)

class GReverse s d t | s d -> t where
  greverse :: s -> d -> t

instance GReverse (NP I '[]) (NP I ys) (NP I ys) where
  greverse _ = id

instance GReverse (NP I xs) (NP I (a ': ys)) t => GReverse (NP I (a ': xs)) (NP I ys) t where
  greverse (a :* xs) ys = greverse xs (a :* ys)

data Nat' = Z' | S' Nat'

type family Lit (n :: Nat) :: Nat' where
  Lit 0 = Z'
  Lit n = S' (Lit (n - 1))

type family IsProductType' s where
  IsProductType' (SOP I '[xs]) = 'True
  IsProductType' s = False

type family AreProducts s where
  AreProducts (NP I '[]) = '[]
  AreProducts (NP I (x ': xs)) = IsProductType' (Rep x) ': AreProducts (NP I xs)

type family LengthT s where
  LengthT s = GLengthT (RepNP s)

type family GLengthT s where
  GLengthT (NP I '[]) = 0
  GLengthT (NP I (a ': xs)) = 1 + GLengthT (NP I xs)

type family Applicable f s where
  Applicable f (NP I '[]) = '[]
  Applicable (a -> b)     (NP I (a ': xs)) = 'True  ': Applicable (a -> b)     (NP I xs)
  Applicable (Poly a b)   (NP I (d ': xs)) = 'True  ': Applicable (Poly a b) (NP I xs)
  Applicable (a -> b)     (NP I (c ': xs)) = 'False ': Applicable (a -> b)     (NP I xs)

type family ApplicableN f s n where
  ApplicableN (a -> b)     (NP I (a ': xs)) Z'     = '[ 'True ]
  ApplicableN (Poly a b)   (NP I (d ': xs)) Z'     = '[ 'True ]
  ApplicableN f            (NP I (a ': xs)) (S' n) = 'False ': ApplicableN f (NP I xs) n

data Poly a b where
  Poly :: (a -> b) -> Poly a b

poly :: (a -> b) -> Poly a b
poly = Poly

type family InsLoc a s t :: Nat' where
  InsLoc a (NP I (b ': s)) (NP I (a ': t)) = If (AreEqual (NP I (b ': s)) (NP I t)) Z' (S' (InsLoc a (NP I s) (NP I t)))
  InsLoc a (NP I (b ': s)) (NP I (b ': t)) = S' (InsLoc a (NP I s) (NP I t))
  InsLoc a (NP I '[]) (NP I '[a]) = Z'

type family If b (l :: k) (r :: k) :: k where
  If 'True l r = l
  If 'False l r = r

type family Head xs where
  Head (x ': xs) = x

type family Tail xs where
  Tail (x ': xs) = xs

type family Diff xs ys where
  Diff (x ': xs) (x ': ys) = Diff xs ys
  Diff (x ': xs) (y ': ys) = x ': xs
  Diff xs '[] = xs

type family Reverse xs ys where
  Reverse (x ': xs) ys = Reverse xs (x ': ys)
  Reverse '[] ys = ys

type family ToFun xs r where
  ToFun (a ': xs) f = a -> ToFun xs f
  ToFun '[] (a -> b) = a -> ToFun '[] b
  ToFun '[] b = b

type family AreEqual s t where
  AreEqual (NP I (a ': s)) (NP I (a ': t)) = AreEqual (NP I s) (NP I t)
  AreEqual (NP I (a ': s)) (NP I (b ': t)) = 'False
  AreEqual s t = 'True

data Rel = Nat :<= Nat | Nat :>= Nat

type family LEQ n m :: Constraint where
  LEQ n m = Relation n m ~ (n :>= m)

type family Relation n m where
  Relation n m = Relation' n m n m

type family Relation' n m i j where
  Relation' n m i 0 = n :>= m
  Relation' n m 0 j = n :<= m
  Relation' n m i j = Relation' n m (i - 1) (j - 1)

type family ToTuple s where
  ToTuple (NP I '[a, b]) = (a,b)
  ToTuple (NP I '[a, b, c]) = (a,b,c)
  ToTuple (NP I '[a, b, c, d]) = (a,b,c,d)
  ToTuple (NP I '[a, b, c, d, e]) = (a,b,c,d,e)
  ToTuple (NP I '[a, b, c, d, e, f]) = (a,b,c,d,e,f)
  ToTuple (NP I '[a, b, c, d, e, f, g]) = (a,b,c,d,e,f,g)
  ToTuple (NP I '[a, b, c, d, e, f, g, h]) = (a,b,c,d,e,f,g,h)
  ToTuple (NP I '[a, b, c, d, e, f, g, h, i]) = (a,b,c,d,e,f,g,h,i)
  ToTuple (NP I '[a, b, c, d, e, f, g, h, i, j]) = (a,b,c,d,e,f,g,h,i,j)
  ToTuple (NP I '[a, b, c, d, e, f, g, h, i, j, k]) = (a,b,c,d,e,f,g,h,i,j,k)