{- | Generic case analysis using [generics-sop](https://hackage.haskell.org/package/generics-sop).

"Case analysis" functions are those which take one function for each constructor of a sum type,
examine a value of that type, and call the relevant function depending on which constructor was
used to build that type. Examples include 'maybe', 'either' and 'Data.Bool.bool'.

It's often useful to define similar functions on user-defined sum types, which is boring at best
and error-prone at worst. This module gives us these functions for any type which
implements 'Generic'.

For any single-constructor types, such as tuples, this gives us generic uncurrying without
any extra effort - see 'tupleL', 'tuple3L'.

== Example

Let's use @These@ from
[these](https://hackage.haskell.org/package/these) as an example.
First we need an instance of 'Generic', which we can derive.

@
{\-# LANGUAGE DeriveGeneric #-\}
import qualified GHC.Generics as G
import Generics.SOP (Generic)

data These a b
  = This a
  | That b
  | These a b
  deriving (Show, Eq, G.Generic)

instance Generic (These a b)      -- we could also do this using DeriveAnyClass
@

We're going to re-implement the case analysis function
[these](https://hackage.haskell.org/package/these-1.2.1/docs/Data-These.html#v:these),
using 'gcase'. Our type has 3 constructors, so our function will have 4 arguments:
one for the @These@ we're analysing, and one function for each constructor.
The function is polymorphic in the result type.

@
these ::
  forall a b c.
  These a b ->
  _ -> _ -> _ ->
  c
@

What are the types of those 3 functions? For each constructor, we make a function type taking
one of each of the argument types, and returning our polymorphic result type @c@:

@
these ::
  forall a b c.
  These a b ->
  (a -> c) ->       -- for This
  (b -> c) ->       -- for That
  (a -> b -> c) ->  -- for These
  c
@

Finally, we add the implementation, which is just 'gcase':

@
these ::
  forall a b c.
  These a b ->
  (a -> c) ->
  (b -> c) ->
  (a -> b -> c) ->
  c
these = gcase
@

Note that we could have written the entire thing more succintly using 'Analysis':

@
these ::
  forall a b c.
  Analysis (These a b) c
these = gcase
@

== Flipping the argument order

'maybe', 'either' and 'Data.Bool.bool' have a slightly different shape to @these@: they take the datatype
(@Maybe a@, @Either a b@ or @Bool@) after the case functions, whereas @these@ (and generally any
analysis function implemented using 'gcase') takes the datatype as its first argument, followed by
the case functions. This is due to the implementation, and is the recommended usage due to performance.
However, you may want your function to follow the same pattern as 'maybe', since this is more ergonomic.
In this case you can use 'AnalysisR' and 'gcaseR':

@
theseR ::
  forall a b c.
  (a -> c) ->
  (b -> c) ->
  (a -> b -> c) ->
  These a b ->
  c
-- alternate signature: theseR :: forall a b c. AnalysisR (These a b) c
theseR = gcaseR @(These a b)
@

Note that we need the @TypeApplications@ extension here. If you're really against this extension,
see 'gcaseR_'.
-}
module Generics.Case
  ( -- * Generic case analysis
    Analysis
  , gcase

    -- ** Flipped argument order
  , AnalysisR
  , gcaseR
  , gcaseR_

    -- * Examples

    -- ** Maybe
  , maybeL
  , maybeR

    -- ** Either
  , eitherL
  , eitherR

    -- ** Bool
  , boolL
  , boolR

    -- ** Tuples
  , tupleL
  , tupleR
  , tuple3L
  , tuple3R

    -- ** Lists
  , listL
  , listR

    -- ** Non-empty lists
  , nonEmptyL
  , nonEmptyR
  )
where

import Data.List.NonEmpty (NonEmpty)
import Generics.Chain
import Generics.SOP

{- | The type of an analysis function on a generic type, in which the type comes before the functions.

You shouldn't ever need to create a function of this type manually; use 'gcase'.

You can exapand the type in a repl:

@
ghci> :k! Analysis (Maybe a) r
Analysis (Maybe a) r :: *
= Maybe a -> r -> (a -> r) -> r
@
-}
type Analysis a r = a -> Chains (Code a) r

{- | Generic case analysis. Similar to 'maybe' or 'either', except the type being analysed comes
before the functions, instead of after.

See the module header for a detailed explanation.
-}
gcase ::
  forall a r.
  (Generic a) =>
  Analysis a r
gcase = applyChains @(Code a) @r . unSOP . from

{- | Same as 'Analysis', except that the type comes after the functions.

You shouldn't ever need to create a function of this type; use 'gcaseR' or 'gcaseR_'.

You can exapand the type in a repl:

@
ghci> :k! AnalysisR (Maybe a) r
AnalysisR (Maybe a) r :: *
= r -> (a -> r) -> Maybe a -> r
@
-}
type AnalysisR a r = ChainsR (Code a) a r

{- | Generic case analysis, with the same shape as 'maybe' or 'either'. In other words this is the same
as 'gcase', except the datatype comes after the analysis functions.

== Note

This is undoubtedly more ergonomic, since it allows us to use partial application nicely:

@
let maybeToEither err = 'maybeR' (Left err) Right
in  ...
@

However, this carries a slight performance impact. It will __always__ be faster to use 'gcase', so if
performance is critical in your use-case, use that. Then again, if performance is __really__ critical,
you'll always be better off writing your analysis function manually; or just pattern-matching directly.
-}
gcaseR ::
  forall a r.
  (Generic a) =>
  AnalysisR a r
gcaseR = toChainsR @(Code a) @a @r $ gcase @a @r

{- | Morally the same as 'gcaseR', but takes a 'Proxy' to avoid @TypeApplications@.

Following our @These@ example:

@
these_ ::
  forall a b c.
  (a -> c) ->
  (b -> c) ->
  (a -> b -> c) ->
  These a b -> c
these_ = gcaseR_ (Proxy :: Proxy (These a b))
@
-}
gcaseR_ ::
  forall a r.
  (Generic a) =>
  Proxy a ->
  AnalysisR a r
gcaseR_ _ = gcaseR @a @r

------------------------------------------------------------
-- Examples

{- | Same as 'maybe', except the 'Maybe' comes before the case functions.

Equivalent type signature:

@
maybeL :: forall a r. Analysis (Maybe a) r
@

The implementation is just:

@
maybeL = gcase @(Maybe a)
@
-}
maybeL :: forall a r. Maybe a -> r -> (a -> r) -> r
maybeL = gcase

{- | 'maybe', implemented using 'gcaseR'.

Equivalent type signature:

@
maybeR :: forall a r. 'AnalysisR' (Maybe a) r
@

The implementation is just:

@
maybeR = gcaseR @(Maybe a)
@
-}
maybeR :: forall a r. r -> (a -> r) -> Maybe a -> r
maybeR = gcaseR @(Maybe a)

{- | Same as 'either', except the 'Either' comes before the case functions.

Equivalent type signature:

@
eitherL :: forall a b r. 'Analysis' (Either a b) r
@

The implementation is just:

@
eitherL = gcase
@
-}
eitherL :: forall a b r. Either a b -> (a -> r) -> (b -> r) -> r
eitherL = gcase

{- | 'either', implemented using 'gcaseR_'.

Equivalent type signature:

@
eitherR :: forall a b r. 'AnalysisR' (Either a b) r
@

The implementation is just:

@
eitherR = gcaseR_ (Proxy :: Proxy (Either a b))
@
-}
eitherR :: forall a b r. (a -> r) -> (b -> r) -> Either a b -> r
eitherR = gcaseR_ (Proxy :: Proxy (Either a b))

{- | Same as 'Data.Bool.bool', except the 'Bool' comes before the case functions.

Equivalent type signature:

@
boolL :: forall r. 'Analysis' Bool r
@

The implementation is just:

@
boolL = gcase
@
-}
boolL :: forall r. Bool -> r -> r -> r
boolL = gcase

{- | 'Data.Bool.bool', implemented using 'gcaseR'.

Equivalent type signature:

@
boolR :: forall r. 'AnalysisR' Bool r
@

The implementation is just:

@
boolR = gcaseR @Bool
@
-}
boolR :: forall r. r -> r -> Bool -> r
boolR = gcaseR @Bool

{- | Case analysis on a list. Same as
[list](https://hackage.haskell.org/package/extra/docs/Data-List-Extra.html#v:list)
from @extra@, except the list comes before the case functions.

Equivalent type signature:

@
listL :: forall a r. 'Analysis' [a] r
@
-}
listL :: forall a r. [a] -> r -> (a -> [a] -> r) -> r
listL = gcase

{- | Case analysis on a list. Same as
[list](https://hackage.haskell.org/package/extra/docs/Data-List-Extra.html#v:list)
from @extra@.

Equivalent type signature:

@
listR :: forall a r. 'AnalysisR' [a] r
@
-}
listR :: forall a r. r -> (a -> [a] -> r) -> [a] -> r
listR = gcaseR @[a]

{- | Case analysis on a tuple. Same as 'uncurry', except the tuple comes before the case function.

Equivalent type signature:

@
tupleL :: forall a b r. 'Analysis' (a, b) r
@
-}
tupleL :: forall a b r. (a, b) -> (a -> b -> r) -> r
tupleL = gcase

{- | Case analysis on a tuple. Interestingly, this is the same as 'uncurry'.

Equivalent type signature:

@
tupleR :: forall a b r. 'AnalysisR' (a, b) r
@
-}
tupleR :: forall a b r. (a -> b -> r) -> (a, b) -> r
tupleR = gcaseR @(a, b)

{- | Case analysis on a 3-tuple. Same as
[uncurry3](https://hackage.haskell.org/package/extra/docs/Data-Tuple-Extra.html#v:uncurry3)
from @extra@, except the tuple comes before the case function.

Equivalent type signature:

@
tuple3L :: forall a b c r. 'Analysis' (a, b, c) r
@
-}
tuple3L :: forall a b c r. (a, b, c) -> (a -> b -> c -> r) -> r
tuple3L = gcase

{- | Case analysis on a 3-tuple. Same as
[uncurry3](https://hackage.haskell.org/package/extra/docs/Data-Tuple-Extra.html#v:uncurry3)
from @extra@.

Equivalent type signature:

@
tuple3R :: forall a b c r. 'AnalysisR' (a, b, c) r
@
-}
tuple3R :: forall a b c r. (a -> b -> c -> r) -> (a, b, c) -> r
tuple3R = gcaseR @(a, b, c)

{- | Case analysis on a non-empty list, where the list comes before the case function.

Equivalent type signature:

@
nonEmptyL :: forall a r. 'Analysis' (NonEmpty a) r
@
-}
nonEmptyL :: forall a r. NonEmpty a -> (a -> [a] -> r) -> r
nonEmptyL = gcase

{- | Case analysis on a non-empty list.

Equivalent type signature:

@
nonEmptyR :: forall a r. 'AnalysisR' (NonEmpty a) r
@
-}
nonEmptyR :: forall a r. (a -> [a] -> r) -> NonEmpty a -> r
nonEmptyR = gcaseR @(NonEmpty a)
