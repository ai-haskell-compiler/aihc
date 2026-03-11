# generic-enumeration

This is a library that provides generics-based enumeration of all possible
values for a type.

It differs from `deriving Enum` in that:

* It supports non-nullary data constructors.
* It does not try for integer encoding/decoding. It works by providing a
  list containing every value, and also providing `succMay` and `predMay`
  analogs to Enum's `succ` and `pred`
* It does not allow user-defined instances. Instances either work generically or they don't.

Because of this it is a little less powerful that `Enum` in some cases,
but it is also a lot more safe. There are no partial functions, and
disallowing user-defined instances means that not even a human can mess
it up.


## Use case

I wrote this to solve a particular kind of problem I often have. When I have a
type:

```Haskell
data A
  = A1
  | A2
  | A3
  deriving (Enum, Bounded)
```

And I want to produce a list of all the values:

```Haskell
allAs :: [A]
allAs = [minBound .. maxBound]
```

This `deriving (Enum, Bounded)` and `[minBound .. maxBound]` dance is
a little awkward, and it is not always the case `Enum` or `Bounded` is
meaningful for the type except as an implementation detail for getting
the full list of values. Worse, this doesn't even work for types with
non-nullary constructors. E.g. this won't compile:

```Haskell
data A
  = A1 B
  | A2 C
  | A3
  deriving (Enum, Bounded)

data B
  = B1
  | B2
  deriving (Enum, Bounded)

data C
  = C1
  | C2
  deriving (Enum, Bounded)
```

On the other hand, this will compile just fine:

```Haskell
data A
  = A1 B
  | A2 C
  | A3
  deriving (Generic)

data B
  = B1
  | B2
  deriving (Generic)

data C
  = C1
  | C2
  deriving (Generic)

allAs :: [A]
allAs = enumeration
  -- where `enumeration` is a function from this package.
  -- Has the value `[A1 B1, A1 B2, A2 C1, A2 C2, A3]`
```





