# `kind-generics`: generic programming for arbitrary kinds and GADTs

*Note:* This README is a work in progress. The most up-to-date version of this document can be found in [GitLab](https://gitlab.com/trupill/kind-generics/blob/master/kind-generics/README.md).

Data type-generic programming in Haskell is restricted to types of kind `*` (by using `Generic`) or `* -> *` (by using `Generic1`). This works fine for implementing generic equality or generic printing, notions which are applied to types of kind `*`. But what about having a generic `Bifunctor` or `Contravariant`? We need to extend our language for describing data types to other kinds -- hopefully without having to introduce `Generic2`, `Generic3`, and so on.

The language for describing data types in `GHC.Generics` is also quite restricted. In particular, it can only describe algebraic data types, not the full extent of GADTs. It turns out that both problems are related: if you want to describe a constructor of the form `forall a. blah`, then `blah` must be a data type which takes one additional type variable. As a result, we need to enlarge and shrink the kind at will.

This library, `kind-generics`, provides a new type class `GenericK` and a set of additional functors `Field`, `(:=>:)` (for *constraints*), and `Exists` (for *existentials*) which extend the language of `GHC.Generics`. We have put a lot of effort in coming with a simple programming experience, even though the implementation is full of type trickery.

## Simple usage of `kind-generics`

Generic operations require conversion from and to generic representations to be supplied by the programmer. Within this library, such operations are represented by a set of `GenericK` instances, one per possible partial application of the data type. You don't have to write those instances manually, though, most of them can be derived automatically.

### Derivation using `kind-generics-th`

The simplest, and at the same time the most powerful, way to get your `GenericK` instances is to use the facilities provided by the `kind-generics-th` package. For example:

```haskell
{-# language TemplateHaskell #-}  -- this should be at the top of the file

data Tree a = Branch (Tree a) (Tree a) | Leaf a
$(deriveGenericK ''Tree)
```

By doing so, two instances are generated:

```haskell
instance GenericK Tree     where ...
instance GenericK (Tree a) where ...
```

### Derivation from `GHC.Generics`

The other possibility is to obtain `GenericK` instances from the built-in support in GHC. In order to use those facilities, your data type must implement the `Generic` type class. Fortunately, GHC can automatically derive such instances for algebraic data types. For example:

```haskell
{-# language DeriveGeneric #-}  -- this should be at the top of the file

data Tree a = Branch (Tree a) (Tree a) | Leaf a
            deriving Generic    -- this is the magical line
```

From this `Generic` instance, `kind-generics` can derive another one for its very own `GenericK`. It needs one additional piece of information, though: the description of the data type in the enlarged language of descriptions. The reason for this is that `Generic` does not distinguish whether the type of a field mentions one of the type variables (`a` in this case) or not. But `GenericK` requires so.

Let us look at the `GenericK` instance for `Tree`:

```haskell
instance GenericK Tree where
  type RepK Tree = (Field (Tree :$: Var0) :*: Field (Tree :$: Var0))
                   :+: (Field Var0)
instance GenericK (Tree a) where
  type RepK (Tree a) = SubstRep (RepK Tree) a
  fromK = fromRepK
  toK   = toRepK
```

In this case we have two constructors, separated by `(:+:)`. The first constructor has two fields, tied together by `(:*:)`. In the description of each field is where the difference with `GHC.Generics` enters the game: you need to describe *each* piece which makes us the type. In this case `Tree :$: Var0` says that the type constructor `Tree` is applied to the first type variable. Type variables, in turn, are represented by zero-indexed `Var0`, `Var1`, and so on.

## Putting `GenericK` instances to work

You can finally use the functionality from `kind-generics` and derive some type classes automatically. Those derivations are found in a separate package `kind-generics-deriving`:

```haskell
import Generics.Kind.Derive.Eq
import Generics.Kind.Derive.FunctorOne

instance Eq a => Eq (Tree a) where
  (==) = geq'
instance Functor Tree where
  fmap = fmapDefaultOne
```

## Type variables in a list: `LoT` and `(:@@:)`

Let us have a closer look at the definition of the `GenericK` type class. If you have been using other data type-generic programming libraries you might recognize `RepK` as the generalized version of `Rep`, which ties a data type with its description, and the pair of functions `fromK` and `toK` to go back and forth the original values and their generic counterparts.

```haskell
class GenericK (f :: k) where
  type family RepK f :: LoT k -> *
  fromK :: f :@@: x -> RepK f x
  toK   :: RepK f x -> f :@@: x
```

But what are those `LoT` and `(:@@:)` which appear there? That is indeed the secret sauce which makes the whole `kind-generics` library work. The name `LoT` comes from *list of types*. It is a type-level version of a regular list, where the `(:)` constructor is replaced by `(:&&:)` and the empty list is represented by `LoT0`. For example:

```haskell
Int :&&: [Bool] :&&: LoT0  -- a list with two basic types
Int :&&: [] :&&: LoT0      -- type constructor may also appear
```

What can you do with such a list of types? You can pass them as type arguments to a type constructor. This is the role of `(:@@:)` (which you can pronounce *of*, or *application*). For example:

```haskell
Either :@@: (Int :&&: Bool :&&: LoT0) = Either Int Bool
Free   :@@: ([]  :&&: Int  :&&: LoT0) = Free [] Int
Int    :@@:                     LoT0 = Int
```

Wait, you cannot apply any list of types to any constructor! Something like `Maybe []` is rejected by the compiler, and so should we reject `Maybe ([] :&&: LoT0)`. To prevent such problems, the list of types is decorated with the *kinds* of all the types inside of it. Going back to the previous examples:

```haskell
Int :&&: [Bool] :&&: LoT0  ::  LoT (* -> * -> *)
Int :&&: [] :&&: LoT0      ::  LoT (* -> (* -> *) -> *)
```

The application operator `(:@@:)` only allows us to apply a list of types of kind `k` to types constructors of the same kind. The shared variable in the head of the type class enforces this invariant also in our generic descriptions.

### Views of a data type

When the type has more than one type parameter, you can break it in different ways. For example, here are all the ways in which `Either Bool Int` could be split in a head and a list of types:

```haskell
Either          :@@: (Bool :&&: Int :&&: LoT0)
Either Bool     :@@:           (Int :&&: LoT0)
Either Bool Int :@@:                     LoT0
```

Different generic operations require different *views* on data types. That is, they require the list of types which is applied to the head to have a particular length. For example, `Eq` views data types as nullary, whereas `Functor` requires list of types of length 1. You can relate this to the fact that in `GHC.Generics` generic equality uses the `Generic` class, but generic functors use `Generic1`.

For a productive usage of `kind-generics`, you should provide as many views of your data type as you can. In the case of `Either` this entails writing the following instances:

```haskell
instance GenericK Either       where ...
instance GenericK (Either a)   where ...
instance GenericK (Either a b) where ...
```

Sometimes it is not possible to write all of these instances, due to restrictions in GHC's type system. The `kind-generics-th` package contains a thorough description of these limitations.

## Describing fields: the functor `Field`

As mentioned in the introduction, `kind-generics` features a more expressive language to describe the types of the fields of data types. We call the description of a specific type an *atom*. The language of atoms reproduces the ways in which you can build a type in Haskell:

1. You can have a constant type `t`, which is represented by `Kon t`.
2. You can mention a variable, which is represented by `Var0`, `Var1`, and so on. For those interested in the internals, there is a general `Var v` where `v` is a type-level number. The library provides the synonyms for ergonomic reasons.
3. You can take two types `f` and `x` and apply one to the other, `f :@: x`.

For example, suppose the `a` is the name of the first type variable and `b` the name of the second. Here are the corresponding atoms:

```haskell
a            ->  Var0
Maybe a      ->  Kon Maybe :@: Var0
Either b a   ->  Kon Either :@: Var1 :@: Var0
b (Maybe a)  ->  Var1 :@: (Kon Maybe :@: Var0)
```

Since the `Kon f :@: x` pattern is very common, `kind-generics` also allows you to write it as simply `f :$: x`. The names `(:$:)` and `(:@:)` are supposed to resemble `(<$>)` and `(<*>)` from the `Applicative` type class.

The kind of an atom is described by two pieces of information, `Atom d k`. The first argument `d` specifies the amount of variables that it uses. The second argument `k` tells you the kind of the type you obtain if you replace the variable markers `Var0`, `Var1`, ... by actual types. For example:

```haskell
Var0                       ->  Atom (k -> ks)             k
Var1 :@: (Maybe :$: Var0)  ->  Atom (* -> (* -> *) -> ks) (*)
```

In the first example, if you tell me the value of the variable `a` regardless of the kind `k`, the library can build a type of kind `k`. In the second example, the usage requires the first variable to be a ground type, and the second one to be a one-parameter type constructor. If you give those types, the library can build a type of kind `*`.

This operation we have just described is embodied by the `Interpret` type family. A call looks like `Interpret atom lot`, where `atom` is an atom and `lot` a list of types which matches the requirements of the atom. We speak of *interpreting* the `atom`. Going back to the previous examples:

```haskell
Interpret Var0                      Int                      =  Int
Interpret Var1 :@: (Maybe :$: Var0) (Bool :&&: [] :&&: LoT0) =  [Maybe Bool]
```

This bridge is used in the first of the pattern functors that `kind-generics` add to those from `GHC.Generics`. The pattern functor `Field` is used to represent fields in a constructor, where the type is represented by an atom. Compare its definition with the `K1` type from `GHC.Generics`:

```haskell
newtype Field (t :: Atom d (*)) (x :: LoT d)
                        = Field { unField :: Interpret t x }
newtype K1 i  (t ::  *) = K1    { unK1    :: t }
```

At the term level there is almost no difference in the usage, except for the fact that fields are wrapped in the `Field` constructor instead of `K1`.

```haskell
instance GenericK Tree where
  type RepK Tree = (Field (Tree :$: Var0) :*: Field (Tree :$: Var0))
                   :+: (Field Var0)

  fromK (Branch l r) = L1 (Field l :*: Field r)
  fromK (Node   x)   = R1 (Field x)
```

On the other hand, separating the atom from the list of types gives us the ability to interpret the same atom with different list of types. This is paramount to classes like `Functor`, in which the same type constructor is applied to different type variables.

## Functors for GADTS: `(:=>:)` and `Exists`

Generalised Algebraic Data Types, GADTs for short, extend the capabilities of Haskell data types. Once the extension is enabled, constructor gain the ability to constrain the set of allowed types, and to introduce existential types. Here is an extension of the previously-defined `Tree` type to include an annotation in every leaf, each of them with possibly a different type, and also require `Show` for the `a`s:

```haskell
data WeirdTree a where
  WeirdBranch :: WeirdTree a -> WeirdTree a -> WeirdTree a
  WeirdLeaf   :: Show a => t -> a -> WeirdTree a
```

The family of pattern functors `V1`, `U1`, `Field`, `(:+:)`, and `(:*:)` is not enough. Let us see what other things we use in the representation of `WeirdTree`:

```haskell
instance GenericK WeirdTree where
  type RepK WeirdTree
    = Field (WeirdTree :$: Var0) :*: Field (WeirdTree :$: Var0)
      :+: Exists (*) ((Show :$: Var1) :=>: (Field Var0 :*: Field Var1))
```

Here the `(:=>:)` pattern functor plays the role of `=>` in the definition of the data type. It reuses the same notion of atoms from `Field`, but requiring those atoms to give back a constraint instead of a ground type.

But wait a minute! You have just told me that the first type variable is represented by `Var0`, and in the representation above `Show a` is transformed into `Show :$: Var1`, what is going on? This change stems from `Exists`, which represents existential quantification. Whenever you go inside an `Exists`, you gain a new type variable in your list of types. This new variable is put *at the front* of the list of types, shifting all the other one position. In the example above, inside the `Exists` the atom `Var0` points to `t`, and `Var1` points to `a`. This approach implies that inside nested existentials the innermost variable corresponds to head of the list of types `Var0`.

In most cases, `GenericK` instances for GADTs can be derived by `kind-generics-th`. Just for the record, here is how one of such `GenericK` instances looks like:

```haskell
instance GenericK WeirdTree where
  type RepK WeirdTree = ...

  fromK (WeirdBranch l r) = L1 $                     Field l :*: Field r
  fromK (WeirdLeaf   a x) = R1 $ Exists $ SuchThat $ Field a :*: Field x

  toK ...
```

You just need to apply the `Exists` and `SuchThat` constructors every time there is an existential or constraint, respectively. However, since the additional information required by those types is implicitly added by the compiler, you do not need to write anything else.

## Atoms for families: `Eval`

If a field contains a type family application, it can be represented by
defunctionalization. For example:

```haskell
type family F a

data T a = C (F a)
```

`F` can be defunctionalized as follows, using `Fcf.Eval` from
[`first-class-families`](https://hackage.haskell.org/package/first-class-families).

```haskell
data DF a
type Fcf.Eval (DF a) = F a
```

All type family applications can thus be rewritten as the application of one
common `Fcf.Eval` to a matchable application. This can then be represented by
one `Atom` constructor, also named `Eval`:

```haskell
instance GenericK T where
  type RepK T = Field (Eval (Kon DF :@: Var0))

  fromK (C x) = Field x
  toK (Field x) = C x
```

`kind-generics-th` can generate `GenericK` instances for such types involving
type families. It also takes care of defunctionalizing type families as needed,
following a more sophisticated approach than described above, that avoids
declaring lots of artificial data types. For more details, see
[`fcf-family`](https://hackage.haskell.org/package/fcf-family)
and the documentation of `kind-generics-th`.

## Implementing a generic operation with `kind-generics`

The last stop in our journey through `kind-generics` is being able to implement a generic operation. At this point we assume that the reader is comfortable with the definition of generic operations using `GHC.Generics`, so only the differences with that style are pointed out.

As an example, we are going to write a generic `Show`. Using `GHC.Generics` style, you create a type class whose instances are the corresponding pattern functors:

```haskell
class GShow (f :: * -> *) where
  gshow :: f x -> String

instance GShow U1 ...
instance Show t => GShow (K1 i t) ...
instance (GShow f, GShow g) => GShow (f :+: g) ...
instance (GShow f, GShow g) => GShow (f :*: g) ...
```

### Introducing a requirements constraint

Let's start from the code above. When using `kind-generics` pattern functors are no longer of kind `* -> *`, but of the more general form `LoT k -> *`. So our first approach to `GShow` looks like:

```haskell
class GShow (f :: LoT k -> *) where
  gshow :: f x -> String
```

We can already provide a proxy function which performs the conversion to the generic representation and then calls the generic operation. A very common scenario is that GHC cannot infer the correct type arguments to `fromK`, but we can always help by providing explicit type applications.

```haskell
{-# language TypeApplications #-}

gshow' :: forall t. (GenericK t LoT0, GShow (RepK t))
       => t -> String
gshow' = gshow . fromK @_ @t @LoT0
```

However, we are stuck when we want to write the instance for `Field`. In the case of `GHC.Generics`, the instance for fields calls the `Show` class recursively:

```haskell
instance Show t => GShow (K1 i t) ...
```

But here we cannot do this. The reason is that we need to provide `Show` with a type. In order to turn an atom, as wrapped by `Field`, into a type we need a list of types. However, the list of types not provided until later, in the call to `gshow`. The trick is to introduce an additional *requirements* constraint:

```haskell
class GShow (f :: LoT k -> *) where
  type family ReqsShow f (x :: LoT k) :: Constraint
  gshow :: ReqsShow f x => f x -> String

gshow' :: forall t. (GenericK t LoT0
          , GShow (RepK t), ReqsShow (RepK t) LoT0)
       => t -> String
gshow' = gshow . fromK @_ @t @LoT0
```

Now in the `Field` instance we can express the requirements for a specific atom:

```haskell
instance GShow (Field t) where
  type ReqsShow (Field t) x = Show (Interpret t x)
  gshow = ...
```

Adding this constraint involves some work also on the rest of pattern functors, because we need to produce requirements for all of them. How to build them changes from generic operation to generic operation, but in general has the following structure:

```haskell
instance GShow U1 where
  type ReqsShow U1 x = ()
instance (GShow f, GShow g) => GShow (f :+: g) where
  type ReqsShow (f :+: g) x = (ReqsShow f x, ReqsShow g x)
instance (GShow f, GShow g) => GShow (f :*: g) where
  type ReqsShow (f :*: g) x = (ReqsShow f x, ReqsShow g x)
```

In theory, the requirements for `(:=>:)` would take into account that the `SuchThat` constructor introduces additional constraints into place. Thus one would write:

```haskell
instance GShow f => GShow (c :=>: f) where
  type ReqsShow (c :=>: f) x = (Interpret c x => ReqsShow f x)
```

Unfortunately, this is currently rejected by GHC: type families cannot return a qualified type. The only option for now is to use the more restrictive version:

```haskell
instance GShow f => GShow (c :=>: f) where
  type ReqsShow (c :=>: f) x = ReqsShow f x
```

However, that means that this version of `GShow` cannot be used with the `WeirdTree` data type defined above. In that case, the `Show a` instance introduced in `WeirdLeaf` would not be accounted for. This is not the only limitation of the requirements constraint approach: existentials in constructors cannot be handled either.

### Using an explicit list of types

A more powerful approach to using `kind-generics` is to separate the head of a type from its type arguments. That means extending the class with a new parameter, and reworking the basic cases to include that argument.

```haskell
class GShow (f :: LoT k -> *) (x :: LoT k) where
  gshow :: f x -> String

instance GShow U1 x ...
instance (GShow f x, GShow g x) => GShow (f :+: g) x ...
instance (GShow f x, GShow g x) => GShow (f :*: g) x ...
```

Now we have the three new constructors. Let us start with `Field atom`: when is it `Show`able? Whenever the interpretation of the atom, with the given list of types, satisfies the `Show` constraint. We can use the type family `Interpret` to express this fact:

```haskell
instance (Show (Interpret t x)) => GShow (Field t) x where
  gshow (Field x) = show x
```

In the case of existential constraints we do not need to enforce any additional constraints. However, we need to extend our list of types with a new one for the existential. We can do that using the `QuantifiedConstraints` extension introduced in GHC 8.6:

```haskell
{-# language QuantifiedConstraints #-}

instance (forall (t :: k). Show f (t :&&: x)) => GShow (Exists k f) x where
  gshow (Exists x) = gshow x
```

The most interesting case is the one for constraints. If we have a constraint in a constructor, we know that by pattern matching on it we can use the constraint. In other words, we are allowed to assume that the constraint at the left-hand side of `(:=>:)` holds when trying to decide whether `GShow` does. This is again allowed by the `QuantifiedConstraints` extension:

```haskell
{-# language QuantifiedConstraints #-}

instance (Interpret c x => GShow f x) => GShow (c :=>: f) x where
  gshow (SuchThat x) = gshow x
```

Note that sometimes we cannot implement a generic operation for every GADT. One example is generic equality: when faced with two values of a constructor with an existential, we cannot move forward, since we have no way of knowing if the types enclosed by each value are the same or not.

### Working with a position

This final section gives an overview of the changes required to bring automatic derivation of `Functor` from `GHC.Generics` to `kind-generics`. In the `generics-deriving` library, the corresponding `GFunctor` class reads as follows:

```haskell
class GFunctor f where
  gmap :: (a -> b) -> f a -> f b
```

Following the approach outlined above, we need to reify the arguments to `f` as additional parameters to the type class. Since `f` appears applied to two different arguments, we get not one but two parameters in the type class.

```haskell
class GFunctor (f :: LoT k -> *) (as :: LoT k) (bs :: LoT k) where ...
```

The problem now is that `as` and `bs` are *lists* of types. But the functor action only works over the *last* one (in general, only over *one* position). So how do we express the type of `gmap`? We can use a `TyVar` to specify that position, and the interpret it over the list of types. Since the new variable `v` appears only as argument to a type family, we need some kind of `Proxy` type to make GHC happy, or to enable the `AllowAmbiguousTypes` extension and work around the lack of inference with type applications.

```haskell
class GFunctor (f :: LoT k -> *) (v :: TyVar d *) (as :: LoT k) (bs :: LoT k) where
  gmap :: (Interpret (Var v) as -> Interpret (Var v) bs)
       -> f as -> f bs
```

This additional `TyVar` is not only needed to write the type of `gmap`. Also, if we want to handle the case of constructors with *existentials*, we need to account for the change of index for the variable.

```haskell
instance (forall (t :: k). GFunctor f (VS v) (t :&&: as) (t :&&: bs))
         => GFunctor (Exists k f) v as bs where ...
```

We have seen three ways of handling generic operations in `kind-generics`:

* *Introducing a requirements constraint*. This is the simpler one, and code stays almost verbatim from a `GHC.Generics` implementation. However, we cannot support existentials or constraints.
* *Using an explicit list of types*. In this case the code can also be copied almost verbatim from a `GHC.Generics` implementations. The type class implementing the generic operation is enlarged with additional parameters to account for the lists of types which are applied in the operations. With this approach we can handle almost any operation which consumes a value of a GADT.
* *Explicit list of types + position*. When defining generic operations over higher-rank types -- like `Functor` -- it is usually required to have an additional parameter in the type class to account for the *position* (or positions) which are affected by the operation. We need to do so because going under the `Exists` constructor shifts the indices of the variables.

### Inspecting atoms

The implementation of `GFunctor` follows the general pattern of calling `gmap` recursively when you find sums, products, constraints, or existentials. The complex part comes in the handling of fields: at that point we need to figure out whether the atom in that field mentions the specific variable we are mapping over, so we can apply the corresponding function. Take for example the representation of `Either`:

```haskell
type RepK Either = Field Var0 :+: Field Var1
```

If we want to implement the usual `fmap`, we need to map over `Var1`, but not over `Var0`. This section shows the technique required to do so. Luckily, the very strong types guarantee that we don't make a mistake.

In order to distinguish the shape of the atoms we need to introduce another type class, `GFunctorField`. It looks pretty much like `GFunctor`, with the difference that its first argument is an *atom* instead of a *pattern functor*. In turn, this means that in the type signature of its methods we need to interpret the atom to turn it into a type. Here are the two type classes side by side:

```haskell
class GFunctorField (t :: Atom k (*)) (v :: TyVar d *) (as :: LoT k) (bs :: LoT k) where
  gmapf :: (Interpret (Var v) as -> Interpret (Var v) bs)
        -> Interpret t as -> Interpret t bs
-- compare with
class GFunctor      (f :: LoT k -> *) (v :: TyVar d *) (as :: LoT k) (bs :: LoT k) where
  gmap  :: (Interpret (Var v) as -> Interpret (Var v) bs)
        -> f as -> f bs
```

If we assume that we satisfy the `GFunctorField` constraint for a given atom, we can write the `GFunctor` instance for the field constructor. Note that we have used explicit type applications because many of these types are ambiguous and cannot be resolved otherwise:

```haskell
instance forall t v as bs. GFunctorField t v as bs
         => GFunctorPos (Field t) v as bs where
  gmap f (Field x) = Field (gmapf @_ @t @v @as @bs f x)
```

This pattern is very common when dealing with generic derivation of operations for types which are not of kind `*`: introduce first a type class for the pattern functors, and then another one with each specific shape of `Field` you may have. Now the question turns into how to write each of the instances of `GFunctorField`.

Let's begin with the simplest one. If we have a constant, we know we don't need to apply any function to it. So `gmapf` is effectively just the identity function:

```haskell
instance GFunctorField (Kon t) v as bs where
  gfmappf _ = id
```

Another case we can handle is an type application of the form `f x`, provided that `f` is a functor and we recursively know how to map over `x`. Think of a data type similar to rose trees:

```haskell
data Rose a = a :<: [Rose a]
```

If we would write the functor instance by hand, in the case of the field of type `[Rose a]`, we would `fmap` over the list, using as argument the recursive `fmap` over `Rose`. The same pattern is captured with the following instance:

```haskell
instance forall f x v as bs.
         ( Functor (Interpret f as), Interpret f as ~ Interpret f bs
         , GFunctorField x v as bs )
         => GFunctorField (f :@: x) v as bs where
  gmapf f x = fmap (gmapf @_ @x @v @as @bs f) x
```

Ok, a bit more is happening than we I have just stated. The additional requirement `Interpret f as ~ Interpret f bs` forces the type constructor being applied to remain constant. This covers the case of `[Rose a]` being mapped to `[Rose b]`, since the type constructor is `[]` regardless of the type of its elements. However, `kind-generics` makes it possible to express more exotic types such as `Var1 :@: Var0`; in that case we are only able to construct the generic mapping operation if the argument to `Var1` is the same in both the input and output lists of kinds.

We come to the most important case: how to handle variables. The idea is quite simple: we want to apply the function only if the atom is the same variable as the one we intended to map over. Otherwise, the `gmap` function should keep the field as it was. A first approach would be to use the following two instances:

```haskell
instance {-# OVERLAPS     #-} GFunctorField (Var v) v as bs where ...
instance {-# OVERLAPPABLE #-} GFunctorField (Var v) w as bs where ...
```

The problem is that we require overlapping instances, which lead to brittle type checking, and are commonly regarded as a construct to avoid if possible. Fortunately, we can work around this problem in two different ways:

#### Preventing overlapping with more instances

Let us think for a moment how we would compare two type variables if we were writing the function in usual term-level Haskell. Usually the two final equations would be written with a catch-all pattern, but here it's important to have non-overlapping equations.

```haskell
compareTyVar :: TyVar d k -> TyVar d k -> Bool
compareTyVar VZ     VZ     = True
compareTyVar (VS v) (VS w) = compareTyVar v w
compareTyVar (VS v) VZ     = False
compareTyVar VZ     (VS w) = False
```

Each of these branches can be translated into an instance of `GFunctorField`. Note that the shape of the second argument limits the shape of the lists of types given afterwards, and this is reflected in the instances:

```haskell
-- case VZ / VZ -> apply the function
instance GFunctorField (Var VZ) VZ (a :&&: as) (b :&&: bs) where
  gmapf f x = f x
-- case VS v / VS w -> recur
instance forall v w r as s bs. GFunctorField (Var v) w as bs
         => GFunctorField (Var (VS v)) (VS w) (r :&&: as) (s :&&: bs) where
  gmapf f x = gmapf @d @(Var v) @w @as @bs f x
-- cases for different head constructors
instance GFunctorField (Var VZ) (VS w) (r :&&: as)   (r :&&: bs) where
  gmapf _ = id
instance GFunctorField (Var (VS v)) VZ (r :&&: LoT0) (r :&&: LoT0) where
  gmapf _ = id
```

#### Preventing overlapping using a type family

If you are not afraid of throwing more machinery at the problem, there's another approach to solve this problem. Checking for equality of types is brittle when used in the head of a type class. However, *closed* type families provide this ability in a well-behaved way:

```haskell
type family EqualTyVar (v :: TyVar d (*)) (w :: TyVar d (*)) :: Bool where
  EqualTyVar v v = True
  EqualTyVar v w = False
```

So what we can do is to introduce (yet) another type class which dispatches based on the result of applying `EqualTyVar` to the two involved type variables.

```haskell
class GFunctorVar (v :: TyVar d *) (w :: TyVar d *)
                  (as :: LoT d) (bs :: LoT d)
                  (equal :: Bool) where
  gmapv :: (Interpret (Var w) as -> Interpret (Var w) bs)
        ->  Interpret (Var v) as -> Interpret (Var v) bs
```

We have two cases: if the `equal` parameter is `True`, we know by construction that `v` and `w` are equal. Haskell's type system is not strong enough to carry this evidence, but we can force this to happen using a type equality constraint. So the following instance corresponds (finally) to the case in which we need to apply the function to the argument:

```haskell
instance v ~ w => GFunctorVar v w as bs True where
  gmapv f x = f x
```

In the other case we know by construction that interpreting `Var v` should result in the same type, since we are not mapping over it. Once again, we cannot bring the evidence from `EqualTyVar` to this point, but we can force GHC to check that it is the case using a type equality constraint:

```haskell
instance (Interpret (Var v) as ~ Interpret (Var v) bs)
        => GFunctorVar v w as bs False where
  gmapv _ = id
```

The last question is: how do we tell `GFunctorField` to use the result of `EqualTyVar` to choose an instance? We simply call the type family in the `instance` declaration:

```haskell
instance forall v w as bs. GFunctorVar v w as bs (EqualTyVar v w)
         => GFunctorField (Var v) w as bs where
  gmapf = gmapv @_ @v @w @as @bs @(EqualTyVar v w)
```

Adding this instance requires the `UndecidableInstances` extension, because GHC cannot guarantee resolution will terminate (as far as the compiler knows, `EqualTyVar` could be doing arbitrary computation). For that reason, I personally prefer to use the previous approach.

### More tricks for `Functor`

The implementation of `GFunctor` outlined above is correct, but could be more expensive that a hand-written one. For example, if you have a field of type `[[Int]]`, the implementation you get is equivalent to `fmap (fmap id)`: two levels of `fmap` corresponding to the two nested lists, and `id` because `Int` is a constant. But a programmer would just notice that the entire field never mentions a type variable, and would write `id` directly.

If you look at the implementation of `GFunctor` in `kind-generics-deriving` (called [`GFunctorPosition`](https://gitlab.com/trupill/kind-generics/blob/master/kind-generics-deriving/src/Generics/Kind/Derive/FunctorPosition.hs) in that library), you will notice a call to `ContainsTyVar` in the instance for `Field`. The role of this parameter is to short-cut evaluation in those cases. So before going into the `GFunctorField` recursive structure, we check whether the field mentions the type variable we are interested in.

## Conclusion and limitations

The `kind-generics` library extends the support for data type-generic programming from `GHC.Generics` to account for kinds different from `*`  and `* -> *` and for GADTs. We have tried to reuse as much of the machinery as possible -- including `V1`, `U1`, `(:+:)`, and `(:*:)`. Furthermore, we provide both Template Haskell-based and `Generic`-based derivation of the required `GenericK` instances.

Although we can now express a larger amount of types and operations, not *all* Haskell data types are expressible in this language. In particular, we cannot have *dependent* kinds, like in the following data type:

```haskell
data Proxy k (d :: k) = Proxy
```

because the kind of the second argument `d` refers to the first argument `k`.
