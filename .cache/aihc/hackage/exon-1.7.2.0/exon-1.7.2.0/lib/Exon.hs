-- | Customizable Quasiquote Interpolation
module Exon (
  -- * Introduction
  -- $intro

  -- * Implicit conversion #conversion#
  -- $conversion

  exon,
  exonws,
  intron,
  exun,
  exonWith,

  -- * Performance
  -- $performance

  -- * Customizing Concatenation
  -- $custom

  Exon (..),
  ExonBuild (..),
  ExonAppend (..),
  ExonSegment (..),
  ExonBuilder (..),
  ExonString (..),
  ExonExpression (..),
  ToSegment (..),
  SkipWs (..),
  skipWs,
  ExonUse (..),

  -- * Type inference
  -- $inference

  -- * Combinators
  module Exon.Combinators,

  -- * Data Types
  module Exon.Data.Segment,
  module Exon.Data.Result,
) where

import Exon.Class.Exon (
  Exon (..),
  ExonAppend (..),
  ExonBuild (..),
  ExonBuilder (..),
  ExonExpression (..),
  ExonSegment (..),
  ExonString (..),
  ExonUse (..),
  )
import Exon.Class.ToSegment (ToSegment (..))
import Exon.Combinators (intercalate, intercalate1, intercalateMay)
import Exon.Data.Result (Result (..))
import Exon.Data.Segment (Segment (..))
import Exon.Quote (exon, exonWith, exonws, exun)
import Exon.SkipWs (SkipWs (..), intron, skipWs)

{- $intro
This Haskell library provides quasiquote string interpolation with customizable concatenation for arbitrary types.

A quasiquote with the quoter 'exon' is transformed into a chain of concatenations, allowing expressions to be
interpolated between the characters @#{@ and @}@.
A common use case is plain text interpolation:

>>> :set -XOverloadedStrings
>>> animal = "snake"
>>> location = "a tree"
>>> [exon|#{animal} in #{location}|]
"snake in a tree"

The quote is effectively converted to a sequence of 'Semigroup' concatenations:

>>> animal <> " " <> "in" <> " " <> location
"snake in a tree"

It is precisely converted to an intermediate data structure, 'Segment', and concatenated using customizable classes:

>>> exonProcess [Expression animal, String " in ", Expression location]
"snake in a tree"

The default implementation uses `Monoid` and `IsString`, resulting in something like:

>>> foldl' (<>) mempty [animal, fromString " ", fromString "in", fromString " ", location] :: String
"snake in a tree"

This allows any type that implements those two classes to be used as the result:

>>> newtype Str = Str Text deriving stock (Show) deriving newtype (IsString, Semigroup, Monoid)
>>> [exon|#{animal} in #{location}|] :: Str
Str "snake in a tree"

The astute reader might notice that it is unclear what type @animal@ is expected to be – it hasn't been annotated, yet
it works for both the quote without type annotation as well as the one of type @Str@.

The reason for this phenomenon is that GHC types @animal = "snake"@ as @animal :: IsString a => a@ thanks to
@OverloadedStrings@.
When the quote is written without annotation, the evaluator defaults to @String@ in order to print it (for example,
@hls-eval-plugin@ or GHCi).

When the two names are used in the quote with @Str@, they will be instantiated as @Str@ as well.

One neat application of Exon is for writing 'Text.Show.showsPrec' methods, whose type is @t'String' -> t'String'@ and
which are used to stringify a type with automatic parentheses when contained in a larger type:

> data Numbers =
>   Numbers Int (Maybe Int) Value
>
> instance Show Numbers where
>   showsPrec d Numbers number maybeNumber value =
>     showParen (d > 10)
>       [exon|Numbers #{showsPrec 11 number} #{showsPrec 11 maybeNumber} #{showsPrec 11 value}|]

-}

{- $conversion
Values of different types can be interpolated if they meet one of two conditions:

- They are instances of 'Generic' and representationally equal to the result type (i.e. newtypes)
- They are one of the common stringly types: t'String', 'Text', 'LText', 'ByteString' and 'LByteString'

Exon requires auto-converted values to be interpolated with a different bracket, @##{@, to avoid accidents:

>>> newtype Str = Str Text deriving stock (Show) deriving newtype (IsString, Semigroup, Monoid)
>>> newtype Animal = Animal ByteString deriving stock (Generic)
>>> animal = Animal "lemur"
>>> location = "a tree"
>>> [exon|##{animal} in #{location}|] :: Str
Str "lemur in a tree"

In this case, the two conditions are even combined – the @Animal@ is unwrapped, UTF-8-decoded to @Text@, and rewrapped
into @Str@.

If you absolutely want to splice newtypes with the same brackets, you can use the quoter 'exun' (the @un@ stands for
@unsafe@).
-}

{- $performance
Generic string concatenation can be quite expensive.
If a quote's result type is 'Text', as in:

>>> [exon|#{"x"} #{"y"}|] :: Text

then the resulting concatenation will use the 'Semigroup' operation for 'Text', which has @O(n)@ complexity for each
pair of appended strings.

In order to improve performance, Exon allows the type used for concatenation (the /builder/) to differ from the result
type, which is implemented by the class 'ExonBuilder'.
For example, the instance for 'Text' selects 'Text.Builder' as the builder type and converts the segments and the result
accordingly.
In a quote of 1000 segments, this improves performance by a factor of 100!

However, if the result type is a newtype, the 'Text' builder will not be used:

>>> newtype Str = Str Text deriving stock (Show) deriving newtype (IsString, Semigroup, Monoid)
>>> [exon|#{"x"} #{"y"}|] :: Str

This restriction can be circumvented by making @Str@ an instance of 'Generic' – in that case, Exon will unwrap the type
(even multiply nested newtypes) and use the builder associated with the inner type.
@Str@ doesn't even need to derive 'IsString', 'Semigroup' and 'Monoid' in this case, as all the operations are performed
on 'Text.Builder'.

In principle, this conversion could be done with 'Coercible' as well, but type inference is really bad with that method.

/Note/ that when using [generic segment conversion]("Exon#g:conversion") in conjunction with this, the result type must
also derive 'IsString'.
-}

{- $custom
Quote types don't have to be transparent wrappers for strings.
Concatenation can be changed in a type's 'Semigroup':

>>> import Data.Text (toUpper)
>>> newtype Name = Name Text deriving newtype (Show, IsString)
>>> instance Semigroup Name where Name l <> Name r = Name (l <> " | " <> r)
>>> deriving instance Monoid Name
>>> lastName = Name "Fry"
>>> [intron|Philip J. #{lastName}|]
"Philip | J. | Fry"

This example uses the quoter 'intron', which ignores whitespace.
The 'Semigroup' then inserts custom separators.

The same result can be achieved by using the whitespace-aware quoter 'exonws' and providing a specialized instance of
some of Exon's classes, like 'ExonString':

> newtype Name = Name Text deriving stock (Generic)
> instance ExonString Name Text.Builder where
>   exonWhitespace _ = Result " | "

This example additionally uses the 'Generic' newtype unwrapping feature, allowing the customization to be performed
directly in the efficient builder type.
-}

{- $inference
The type of an expression segment is usually known, for example when the interpolated value is an argument of the
enclosing function:

>>> f :: Text -> Text; f t = [exon|value: #{t}|]
>>> f "snake"
"value: snake"

Here both the interpolated expression and the quote's result type are known.

However, expressions may be polymorphic:

>>> t :: IsString a => a; t = "snake"
>>> f :: Text; f = [exon|value: #{t}|]
>>> f
"value: snake"

The quasiquote parser turns this into the expression:

> [Segment.String "value: ", Segment.Expression t] :: Text

The list of segments has the known type 'Text', obtained from the type signature of @f@.
In this case, the type of @t@ is instantiated as 'Text' as well, causing no type inference problems.

However, if the quote is used polymorphically as well, as in:

> printThing :: Show a => a -> IO ()
> printThing [exon|value: #{t}|]

the type of the segment list would be @∀ a . IsString => [a]@, which would not be possible to concatenate, so @t@ must
be annotated.

Since the segment list is typechecked like any other expression, an expression segment with known type will determine
the result type of an otherwise polymorphic quote:

>>> newtype Str = Str Text deriving stock (Show) deriving newtype (IsString, Semigroup, Monoid)
>>> [exon|pre #{"x" :: Str} post|]
Str "pre x post"

The result type is unambiguously fixed to @Str@.
-}
