# binary-generic-combinators

## tl;dr

This library provides a set of combinators and utility types to make `Generic`-based deriving of
[binary](https://hackage.haskell.org/package/binary) (de)serialization instances easier and more flexible,
especially when dealing with existing formats.

## Motivation

Isn't it great to just define data types representing your problem domain and let the compiler derive all the instances?
If we are talking about [binary](https://hackage.haskell.org/package/binary) (de)serialization,
the `Binary` instance is already derivable for `Generic` types, but there are a couple of problems with that.

### Compound types

Firstly, the serialization format of compound types is carved in stone (again, we're talking about `Generic`-based deriving).
For example, for some list `[a]` the `binary` library always assumes the list length is mentioned explicitly.
This is totally fine if we just need to be able to serialize our type and deserialize it later in the same program without caring about outside world.
But what if we're writing a parser (or serializer) for some existing data format?
In this case (and it's often _the_ case) the format might just imply the strategy of
"try to parse the elements of some type `a` until failure, and build a list out of that" — pretty much like `Alternative`'s `some` or `many`.
With stock `binary`, we'd have to write a custom instance by hand:

```haskell
data Element = Element { .. } deriving (Generic, Binary)
data MyType = MyType { elems :: [Element] }

instance Binary MyType where
  get = MyType <$> many
  put = mapM_ put . elems
```

Wouldn't it be great if we could just annotate our types in such a way that we don't have to write that instance?
Something like, well... This?
```haskell
data Element = Element { .. } deriving (Generic, Binary)
data MyType = MyType { elems :: Many Element } deriving (Generic, Binary)
```

This library provides an array of wrappers that solve precisely this issue.

### Utilities

Then, what if we need to skip some number of bytes, or any number of bytes as long as their value is `0xff`?
Or, maybe, make sure that the input starts with a certain signature sequence of bytes?
With stock `binary`, we again have to write an instance by hand.
This library provides a few helpers for that too:
```haskell
data MyType = MyType
  { header :: MatchBytes "my format header" '[ 0xd3, 0x4d, 0xf0, 0x0d ]   -- consume 0xd34df00d, or fail the parse
  , slack :: SkipByte 0xff                                                -- skip all subsequent 0xff
  , reserved :: SkipCount Word8 4                                         -- 4 bytes reserved
  ..
  } deriving (Generic, Binary)
```

### Deriving strategies

With stock `binary`, if we serialize an ADT, then `binary` first writes the integer denoting the index of the constructor,
and then the contents of that constructor.
This is not always what's needed.
Consider:
```haskell
data JfifSegment
  = App0Segment (MatchByte "app0 segment" 0xe0, JfifApp0)
  | DqtSegment  (MatchByte "dqt segment"  0xdb, QuantTable)
  | SofSegment  (MatchByte "sof segment"  0xc0, SofInfo)
  | DhtSegment  (MatchByte "dht segment"  0xc4, HuffmanTable)
  | DriSegment  (MatchByte "dri segment"  0xdd, RestartInterval)
  | SosSegment  (MatchByte "sos segment"  0xda, SosImage)
  | UnknownSegment RawSegment
```
Here, the identifiers of the constructors are effectively defined in the standard (by the way, this is JPEG/JFIF).
Deriving `Binary` for this type would yield incorrect results: we don't need to encode or decode the index of the constructor,
it's already baked in the `MatchBytes` part.
In this case, what we need is to try to parse each constructor in order, moving on to the next one if its segment identifier doesn't match
what's in the byte stream.
That's basically what `Alternative`'s `<|>` does!
Here, we can leverage that via this library's handy `Alternatively` type and `DerivingVia`:
```haskell
{-# LANGUAGE DerivingVia #-}

data JfifSegment
  = App0Segment (MatchByte "app0 segment" 0xe0, JfifApp0)
  | DqtSegment  (MatchByte "dqt segment"  0xdb, QuantTable)
  | SofSegment  (MatchByte "sof segment"  0xc0, SofInfo)
  | DhtSegment  (MatchByte "dht segment"  0xc4, HuffmanTable)
  | DriSegment  (MatchByte "dri segment"  0xdd, RestartInterval)
  | SosSegment  (MatchByte "sos segment"  0xda, SosImage)
  | UnknownSegment RawSegment
  deriving Generic
  deriving Binary via Alternatively JfifSegment

```
