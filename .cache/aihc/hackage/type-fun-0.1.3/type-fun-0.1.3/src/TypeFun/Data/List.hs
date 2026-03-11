-- | Collection of type families on type lists. This module differs
-- from __Data.Singletons.Prelude.List__ because works with __[*]__
-- and relies on GHC's type equality (~). The rest of required
-- operations like 'Reverse' or '(:++:)' you could find in singletons

module TypeFun.Data.List
  ( -- * Primitive operations on lists
    Length
  , Drop
  , Take
  , Delete
  , Remove
  , (:++:)
    -- * Elements lookup
  , IndexOf
  , IndexOfMay
  , IndexOfMay'
  , IndicesOfMay
  , IndicesOf
  , Index
  , IndexMay
  , IndicesMay
  , Indices
  , Elem
  , NotElem
  , Count
    -- * Operations with lists
  , SubList
  , NotSubList
  , IsPrefixOf
  , IsNotPrefixOf
  , IsPrefixOfBool
    -- * Set operations
  , Union
  , UnionList
  , AppendUniq
  , Intersect
  , Substract
    -- * Uniqueness checking
  , ElementIsUniq
  , UniqElements
  , UniqElements'
    -- * Unsafe helpers
  , appendId
  , subListId
  ) where

import           Data.Type.Bool
import           GHC.Exts
import           GHC.TypeLits
import           TypeFun.Data.Eq
import           TypeFun.Data.Maybe
import           TypeFun.Data.Peano
import           Unsafe.Coerce

------------------------------------------------------------------------
-- NOTE: Errors type classes. These type classes are not in separate  --
-- module because in this case user could import them and instantiate --
-- by mistake. The name of typeclass is an error message in fact. We  --
-- have not any way to fail constraint explicitly, so this hack was   --
-- introduced.                                                        --
------------------------------------------------------------------------

class ElementNotFoundInList a s

class ElementIsInList a s

class ListIsNotPrefixOf a b

class ListIsPrefixOf a b

class ElementIsNotUniqInList a s

class IndexNotFoundInList a s

----------------------
-- main module code --
----------------------


type family Length (a :: [k]) :: N where
  Length '[] = 'Z
  Length (a ': as) = 'S (Length as)

type family Drop (c :: N) (s :: [k]) :: [k] where
  Drop 'Z     s         = s
  Drop ('S c) '[]       = '[]
  Drop ('S c) (a ': as) = Drop c as

type family Take (c :: N) (s :: [k]) :: [k] where
  Take 'Z     s         = '[]
  Take ('S c) '[]       = '[]
  Take ('S c) (a ': as) = a ': (Take c as)

-- | Remove first argument type from anywhere in a list.
type family Delete (a :: k) (s :: [k]) :: [k] where
  Delete a '[]       = '[]
  Delete a (a ': as) = Delete a as
  Delete a (b ': as) = b ': (Delete a as)

-- | Remove index from the list
type family Remove (i :: N) (a :: [k]) :: [k] where
  Remove i      '[]       = '[]
  Remove 'Z     (a ': as) = as
  Remove ('S i) (a ': as) = a ': (Remove i as)

type family (:++:) (a :: [k]) (b :: [k]) :: [k] where
  '[] :++: b = b
  (a ': as) :++: b = a ': (as :++: b)

data Dict c where Dict :: c => Dict c

appendId
  :: forall proxy l r
   . proxy l
  -> (l ~ (l :++: '[]) => r)
  -> r
appendId _ r = case obvious of Dict -> r
  where obvious :: Dict (l ~ (l :++: '[]))
        obvious = unsafeCoerce (Dict :: Dict (l ~ l))

type IndexOf a s = FromJust (IndexOfMay a s)

type IndexOfMay a s = IndexOfMay' Z a s

type family IndexOfMay' (acc :: N) (a :: k) (s :: [k]) :: Maybe N where
  IndexOfMay' acc a '[]       = 'Nothing
  IndexOfMay' acc a (a ': as) = 'Just acc
  IndexOfMay' acc a (b ': as) = IndexOfMay' (S acc) a as

type family IndicesOfMay (a :: [k]) (b :: [k]) :: [Maybe N] where
  IndicesOfMay '[] bs = '[]
  IndicesOfMay (a ': as) bs = (IndexOfMay a bs) ': (IndicesOfMay as bs)

type family IndicesOf (a :: [k]) (b :: [k]) :: [N] where
  IndicesOf '[] bs = '[]
  IndicesOf (a ': as) bs = (IndexOf a bs) ': (IndicesOf as bs)

type Index idx s = FromJust (IndexMay idx s)

type family IndexMay (idx :: N) (s :: [k]) :: Maybe k where
  IndexMay idx     '[]       = 'Nothing
  IndexMay Z       (a ': as) = 'Just a
  IndexMay (S idx) (a ': as) = IndexMay idx as

type family IndicesMay (idxs :: [N]) (a :: [k]) :: [Maybe k] where
  IndicesMay '[] as = '[]
  IndicesMay (i ': idxs) as = (IndexMay i as) ': (IndicesMay idxs as)

type family Indices (idxs :: [N]) (a :: [k]) :: [k] where
  Indices '[]         as = '[]
  Indices (i ': idxs) as = (Index i as) ': (Indices idxs as)

-- | Generates unresolvable constraint if fists element is not
-- contained inside of second
type Elem a s = NothingToConstr (IndexOfMay a s)
                                (ElementNotFoundInList a s)

-- | Reverse of 'Elem'
type NotElem a s = JustToConstr (IndexOfMay a s)
                                (ElementIsInList a s)

-- | Count elements in a list
type family Count (a :: k) (s :: [k]) :: N where
  Count a '[]       = 'Z
  Count a (a ': as) = 'S (Count a as)
  Count a (b ': as) = Count a as

-- | Constrains that first argument is a sublist of second. Reduces
-- to __(Elem a1 b, Elem a2 b, Elem a3 b, ...)__
type family SubList (a :: [k]) (b :: [k]) :: Constraint where
  SubList '[]       bs = ()
  SubList (a ': as) bs = (Elem a bs, SubList as bs)

subListId
  :: forall proxy l r
   . proxy l
  -> (SubList l l => r) -> r
subListId _ r = case obvious of Dict -> r
  where obvious :: Dict (SubList l l)
        obvious = unsafeCoerce (Dict :: Dict ())

type family NotSubList (a :: [k]) (b :: [k]) :: Constraint where
  NotSubList '[]       bs = ()
  NotSubList (a ': as) bs = (NotElem a bs, NotSubList as bs)

type IsPrefixOf a b = ( If (IsPrefixOfBool a b)
                           (() :: Constraint)
                           (ListIsNotPrefixOf a b)
                      , SubList a b )

type IsNotPrefixOf a b = If (IsPrefixOfBool a b)
                            (ListIsPrefixOf a b)
                            (() :: Constraint)

-- | First argument is prefix of second
type family IsPrefixOfBool (a :: [k]) (b :: [k]) :: Bool where
  IsPrefixOfBool '[]       b         = 'True
  IsPrefixOfBool (a ': as) (a ': bs) = IsPrefixOfBool as bs
  IsPrefixOfBool as        bs        = 'False

-- | Appends elements from first list to second if they are not
-- presented in.
type family Union (a :: [k]) (b :: [k]) :: [k] where
  Union '[]       bs = bs
  Union (a ': as) bs = Union as (AppendUniq a bs)

type family UnionList (l :: [[k]]) :: [k] where
  UnionList '[]       = '[]
  UnionList (a ': as) = Union a (UnionList as)

-- | Append element to list if element is not already presented in
type family AppendUniq (a :: k) (s :: [k]) :: [k] where
  AppendUniq a (a ': bs) = a ': bs
  AppendUniq a (b ': bs) = b ': (AppendUniq a bs)
  AppendUniq a '[]       = '[a]

-- | Calculates intersection between two lists. Order of elements is
-- taken from first list
type Intersect a b = (Indices (CatMaybes (IndicesOfMay a b)) b)

-- | Removes from first list all elements occured in second
type family Substract (a :: [k]) (b :: [k]) :: [k] where
  Substract '[] b = '[]
  Substract a '[] = a
  Substract a (b ': bs) = Substract (Delete b a) bs

-- | Checks that element 'a' occurs in a list just once
type ElementIsUniq a s = If (Equal (S Z) (Count a s))
                            (() :: Constraint)
                            (ElementIsNotUniqInList a s)

-- | Checks that all elements in list are unique
type UniqElements a = UniqElements' a a

type family UniqElements' (a :: [k]) (self :: [k]) :: Constraint where
  UniqElements' '[]       self = ()
  UniqElements' (a ': as) self = (ElementIsUniq a self, UniqElements' as self)
