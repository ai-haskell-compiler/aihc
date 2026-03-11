{-# LANGUAGE FlexibleInstances #-}
-- |A convenient set of useful conditional operators.
module Control.Conditional
       ( -- *Conversion typeclass
         ToBool(..)
         -- * Basic conditional operators
       , if', (??), bool
       , ifM, (<||>), (<&&>), notM, xorM
         -- * Lisp-style conditional operators 
       , cond, condDefault, condPlus, condM, condPlusM, otherwiseM
         -- * Conditional operator on categories
       , (?.)
         -- * Conditional operator on monoids
       , (?<>)
         -- * Conditional operator on functions
       , select, selectM 
         -- * C-style ternary conditional
       , (?)
         -- *Hoare's conditional choice operator
         -- |The following operators form a ternary conditional of the form
         -- 
         -- > t <| p |> f
         --
         -- These operators chain with right associative fixity. This allows 
         -- chaining of conditions, where the result of the expression is 
         -- the value to the left of the first predicate that succeeds.
         -- 
         -- For more information see 
         -- <http://zenzike.com/posts/2011-08-01-the-conditional-choice-operator>
       , (|>), (<|)
         -- **Lifted conditional choice
         -- |In addition, you can write lifted conditionals of the form:
         -- 
         -- > t <<| p |>> f
       , (|>>), (<<|)
         -- **Unicode variants
         -- |Intended to resemble the notation used in Tony Hoare's 
         -- Unified Theories of Programming.
       , (⊳), (⊲)
         -- *Generalized monadic conditionals
       , guard, guardM, when, whenM, unless, unlessM, 
       ) where

import Data.Algebra.Boolean
import Control.Monad hiding (guard, when, unless)
import Control.Category 
import Data.Monoid
import Data.Maybe
import Prelude hiding ((.), id, (&&), (||), not)

infixr  0 <|, |>, ⊳, ⊲, ?, <<|, |>>
infixr  1 ??
infixr  2 <||>
infixr  3 <&&>
infixr  7 ?<>
infixr  9 ?. 

-- |Conversion of values to 'Bool'.
--
-- Instances of 'ToBool' that are also 'Boolean' should obey the following laws:
--
-- > p || q = if toBool p then true else q
--
-- > p && q = if toBool p then q else false
class ToBool bool where
  toBool :: bool -> Bool

instance ToBool Bool where toBool = id
instance ToBool Any  where toBool = getAny
instance ToBool All  where toBool = getAll
instance ToBool (Dual Bool) where toBool = getDual

-- |A simple conditional operator
if' :: ToBool bool => bool -> a -> a -> a
if' p t f = if toBool p then t else f
{-# INLINE if' #-}

-- |'if'' with the 'Bool' argument at the end (infixr 1).
(??) :: ToBool bool => a -> a -> bool -> a
(??) t f p = if' p t f 
{-# INLINE (??) #-}

-- |A catamorphism (aka fold) for booleans. This is analogous to 
-- 'foldr', 'Data.Maybe.maybe', and 'Data.Either.either'. The first argument is 
-- the false case, the second argument is the true case, and the last argument 
-- is the predicate value.
bool :: (ToBool bool) => a -> a -> bool -> a
bool f t p = if' p t f
{-# INLINE bool #-}

-- |Lisp-style conditionals. If no conditions match, then a runtime exception
-- is thrown. Here's a trivial example:
--
-- @
--   signum x = cond [(x > 0     , 1 )
--                   ,(x < 0     , -1)
--                   ,(otherwise , 0 )]
-- @
cond :: ToBool bool => [(bool, a)] -> a
cond [] = error "cond: no matching conditions"
cond ((p,v):ls) = if' p v (cond ls)

-- | Analogous to the 'cond' function with a default value supplied,
-- which will be used when no condition in the list is matched.
condDefault :: ToBool bool => a -> [(bool, a)] -> a
condDefault = (. condPlus) . (<|)
{-# INLINE condDefault #-}

-- |Lisp-style conditionals generalized over 'MonadPlus'. If no conditions
-- match, then the result is 'mzero'. This is a safer variant of 'cond'.
--
-- Here's a highly contrived example using 'Data.Maybe.fromMaybe': 
--
-- @
--   signum x = fromMaybe 0 . condPlus $ [(x > 0, 1 ) 
--                                       ,(x < 0, -1)]
-- @
--
-- Alternatively, you could use the '<|' operator from Hoare's ternary
-- conditional choice operator, like so:
--
-- @
--   signum x = 0 \<| condPlus [(x > 0, 1 ) 
--                            ,(x < 0, -1)]
-- @
condPlus :: (ToBool bool, MonadPlus m) => [(bool, a)] -> m a
condPlus [] = mzero
condPlus ((p,v):ls) = if' p (return v) (condPlus ls)

-- |Conditional composition. If the predicate is False, 'id' is returned
-- instead of the second argument. This function, for example, can be used to 
-- conditionally add functions to a composition chain.
(?.) :: (ToBool bool, Category cat) => bool -> cat a a -> cat a a
p ?. c = if' p c id
{-# INLINE (?.) #-}

-- |Composes a predicate function and 2 functions into a single
-- function. The first function is called when the predicate yields True, the
-- second when the predicate yields False.
--
-- Note that after importing "Control.Monad.Instances", 'select' becomes a  
-- special case of 'ifM'.
select :: ToBool bool => (a -> bool) -> (a -> b) -> (a -> b) -> (a -> b)
select p t f x = if' (p x) (t x) (f x)
{-# INLINE select #-}

-- |'if'' lifted to 'Monad'. Unlike 'liftM3' 'if'', this is  
-- short-circuiting in the monad, such that only the predicate action and one of
-- the remaining argument actions are executed.
ifM :: (ToBool bool, Monad m) => m bool -> m a -> m a -> m a 
ifM p t f = p >>= bool f t
{-# INLINE ifM #-}

-- |Lifted inclusive disjunction. Unlike 'liftM2' ('||'), This function is 
-- short-circuiting in the monad. Fixity is the same as '||' (infixr 2).
(<||>) :: (ToBool bool, Boolean bool, Monad m) => m bool -> m bool -> m bool
(<||>) t f = ifM t (return true) f
{-# INLINE (<||>) #-}

-- |Lifted conjunction. Unlike 'liftM2' ('&&'), this function is 
-- short-circuiting in the monad. Fixity is the same as '&&' (infxr 3).
(<&&>) :: (ToBool bool, Boolean bool, Monad m) => m bool -> m bool -> m bool
(<&&>) t f = ifM t f (return false)
{-# INLINE (<&&>) #-}

-- |Lifted boolean negation.
notM :: (Boolean bool, Monad m) => m bool -> m bool
notM = liftM not
{-# INLINE notM #-}

-- |Lifted boolean exclusive disjunction.
xorM :: (Boolean bool, Monad m) => m bool -> m bool -> m bool
xorM = liftM2 xor

-- |'cond' lifted to 'Monad'. If no conditions match, a runtime exception
-- is thrown.
condM :: (ToBool bool, Monad m) => [(m bool, m a)] -> m a 
condM [] = error "condM: no matching conditions"
condM ((p, v):ls) = ifM p v (condM ls)

-- |'condPlus' lifted to 'Monad'. If no conditions match, then 'mzero'
-- is returned.
condPlusM :: (ToBool bool, MonadPlus m) => [(m bool, m a)] -> m a
condPlusM [] = mzero
condPlusM ((p, v):ls) = ifM p v (condPlusM ls)

-- |A synonym for 'return' 'true'.
otherwiseM :: (Boolean bool, Monad m) => m bool
otherwiseM = return true

-- |Generalization of 'Control.Monad.guard'
guard :: (ToBool bool, MonadPlus m) => bool -> m ()
guard p = if' p (return ()) mzero
{-# INLINE guard #-}

-- |Generalization of 'Control.Monad.when'
when :: (ToBool bool, Monad m) => bool -> m () -> m ()
when p m = if' p m (return ())
{-# INLINE when #-}

-- |Generalization of 'Control.Monad.unless'
unless :: (Boolean bool, ToBool bool, Monad m) => bool -> m() -> m()
unless p m = if' (not p) m (return ())
{-# INLINE unless #-}

-- |A variant of 'when' with a monadic predicate.
whenM :: (ToBool bool, Monad m) => m bool -> m () -> m ()
whenM p m = ifM p m (return ())
{-# INLINE whenM #-}

-- |A variant of 'unless' with a monadic predicate.
unlessM :: (ToBool bool, Boolean bool, Monad m) => m bool -> m () -> m ()
unlessM p m = ifM (notM p) m (return ())
{-# INLINE unlessM #-}

-- |A variant of 'guard' with a monadic predicate.
guardM :: (ToBool bool, MonadPlus m) => m bool -> m ()
guardM = (guard =<<)
{-# INLINE guardM #-}

-- |'select' lifted to 'Monad'.
selectM :: (ToBool bool, Monad m) => 
           (a -> m bool) -> (a -> m b) -> (a -> m b) -> (a -> m b)
selectM p t f x = ifM (p x) (t x) (f x) 
{-# INLINE selectM #-}

-- |Conditional monoid operator. If the predicate is 'False', the second
-- argument is replaced with 'mempty'. The fixity of this operator is one
-- level higher than 'Data.Monoid.<>'. 
--
-- It can also be used to chain multiple predicates together, like this: 
--
-- > even (length ls) ?<> not (null ls) ?<> ls
(?<>) :: (ToBool bool, Monoid a) => bool -> a -> a
p ?<> m = if' p m mempty
{-# INLINE (?<>) #-}
 

-- |An operator that allows you to write C-style ternary conditionals of
-- the form:
--
-- > p ? t ?? f
--
-- Note that parentheses are required in order to chain sequences of
-- conditionals together. This is probably a good thing.
(?) :: b -> (b -> a) -> a
p ? f = f p
{-# INLINE (?) #-}

-- |Right bracket of the conditional choice operator. If the predicate
-- is 'True', returns 'Nothing', otherwise it returns 'Just' the right-hand
-- argument.
(|>) :: ToBool bool => bool -> a -> Maybe a
p |> v = if' p Nothing (Just v)
{-# INLINE (|>) #-}

-- |Left bracket of the conditional choice operator. This is equivalent to
-- 'Data.Maybe.fromMaybe'
(<|) :: a -> Maybe a -> a
t <| Nothing = t
_ <| Just f  = f
{-# INLINE (<|) #-}

-- |A monadic variant of '|>'.
(|>>) :: (ToBool bool, Monad m) => m bool -> m a -> m (Maybe a)
p |>> v = ifM p (return Nothing) (liftM Just v)
{-# INLINE (|>>) #-}

-- |A monadic variant of '<|'.
(<<|) :: Monad m => m a -> m (Maybe a) -> m a
v <<| mv = liftM2 fromMaybe v mv
{-# INLINE (<<|) #-}

-- |Unicode rebinding of '<|'. 
(⊲) :: a -> Maybe a -> a
(⊲) = (<|)

-- |Unicode rebinding of '|>'.
(⊳) :: ToBool bool => bool -> a -> Maybe a
(⊳) = (|>)
