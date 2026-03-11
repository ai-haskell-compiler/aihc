{-# LANGUAGE FlexibleInstances,
             FunctionalDependencies,
             GeneralizedNewtypeDeriving #-}

-- | A library providing support for templating
module Tophat
  (
   -- * Basic functionality
   Context,
   Template(runTemplate),
   makeTemplate,
   embed,
   embedConst,
   embedShow,
   -- * Control structures
   -- ** for
   ForContext, forH, endfor,
   -- ** if
   IfContext, ifH, endif,
   -- ** process
   ProcessContext, procH, endproc,
   -- ** with
   WithContext, withH, endwith,
   -- * Re-exported code
   (>>>)
  ) where

import Control.Arrow ((>>>))
import Data.Functor.Identity (Identity(..))
import Data.Profunctor (Profunctor, rmap)
import Data.String (IsString, fromString)


-- | A @Template a s@ wraps a function which takes an argument of type
-- @a@, and is intended to return some 'IsString' type @s@.
newtype Template a s = Template { runTemplate :: a -> s }
  deriving (Semigroup, Monoid, Profunctor)

-- | We normally manipulate not 'Template's, but functions which
-- extend templates; this extracts a 'Template' from such a function
-- by applying it to the empty template.
makeTemplate :: (Monoid s) => (Template a s -> Template a s) -> Template a s
makeTemplate f = f mempty


-- | A 'Context' records the situation part-way through a template (in
-- the middle of control structures, perhaps).
class Context s a r | r -> s, r -> a where
  -- | Append a template to a context
  prolong :: Template a s -> r -> r

-- | A 'Template' is the basic example of a 'Context'
instance Semigroup s => Context s a (Template a s) where
  prolong = flip (<>)

-- | Insert something computed from the template argument
embed :: (Context s a r) => (a -> s) -> r -> r
embed = prolong . Template

-- | Insert something not depending upon the template argument
embedConst :: (Context s a r) => s -> r -> r
embedConst = embed . const

-- | Insert a string representation (obtained by @fromString . Show@)
-- derived from the template argument
embedShow :: (Context s a r, IsString s, Show b) => (a -> b) -> r -> r
embedShow f = embed (fromString . show . f)


-- | The 'ForContext' control structure iterates over any 'Foldable'
-- data structure: this is quite powerful, and can subsume many of
-- those control structures which follow.
data ForContext f s a b r = ForContext {
  _variableFor :: a -> f b,
  _previousFor :: r,
  _innerFor :: Template b s
}

instance (Semigroup s, Context s a r) => Context s b (ForContext f s a b r) where
  prolong u (ForContext v p t) = ForContext v p (prolong u t)

-- | This enters a 'ForContext'.
forH :: (Monoid s) => (a -> f b) -> r -> ForContext f s a b r
forH v p = ForContext v p mempty

-- | This exits from a 'ForContext'.
endfor :: (Monoid s, Context s a r, Foldable f) => ForContext f s a b r -> r
endfor (ForContext v p f) = prolong (Template (foldMap (runTemplate f) . v)) p


-- | The 'IfContext' control structure is a slightly disguised
-- 'ForContext' (using 'Maybe').
newtype IfContext s a b r = IfContext {
  ifFor :: ForContext Maybe s a b r
}

instance (Semigroup s, Context s a r) => Context s b (IfContext s a b r) where
  prolong u (IfContext c) = IfContext (prolong u c)

-- | This enters an 'IfContext'.
ifH :: (Monoid s) => (a -> Bool) -> r -> IfContext s a a r
ifH f = IfContext . forH g where
  g x = if f x then Just x else Nothing

-- | This exits from an 'IfContext'.
endif :: (Monoid s, Context s a r) => IfContext s a a r -> r
endif = endfor . ifFor


-- | The 'WithContext' control structure changes the argument to the
-- template temporarily. It is intended to be useful in situations
-- where tree-like data structures are passed as arguments, and there
-- is a section of the template where only one branch is of interest.
-- Again, this is a slightly disguised 'ForContext' (using 'Identity').
newtype WithContext s a b r = WithContext {
  withFor :: ForContext Identity s a b r
}

instance (Semigroup s, Context s a r) => Context s b (WithContext s a b r) where
  prolong u (WithContext c) = WithContext (prolong u c)

-- | This enters a 'WithContext'.
withH :: (Monoid s) => (a -> b) -> r -> WithContext s a b r
withH f = WithContext . forH (Identity . f)

-- | This exits from a 'WithContext'.
endwith :: (Monoid s, Context s a r) => WithContext s a b r -> r
endwith = endfor . withFor


-- | The 'ProcessContext' control structure postprocesses the template
-- output in a region (unlike a 'WithContext', which preprocesses the
-- template argument).
data ProcessContext s t a r = ProcessContext {
  _mapProcess :: t -> s,
  _previousProcess :: r,
  _innerProcess :: Template a t
}

instance (Semigroup t, Context s a r) => Context t a (ProcessContext s t a r) where
  prolong u (ProcessContext v p t) = ProcessContext v p (prolong u t)

-- | This enters a 'ProcessContext'.
procH :: (Monoid t) => (t -> s) -> r -> ProcessContext s t a r
procH f p = ProcessContext f p mempty

-- | This exits from a 'ProcessContext'.
endproc :: (Context s a r) => ProcessContext s t a r -> r
endproc (ProcessContext f p t) = prolong (rmap f t) p
