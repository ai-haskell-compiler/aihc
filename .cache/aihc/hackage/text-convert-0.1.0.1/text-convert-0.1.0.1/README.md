# text-convert

Convert between various types representing textual data. This library differs
from the similar `string-conversions` library in that this library exports class
methods that are monomorphic in their return type. This enhances readability and
aids type inference.

![Haskell textual type conversions](haskell-textual-type-conversions.png)

The module `Text.Convert` exports seven one-method type classes. The methods are
designed to be input-type polymorphic and output-type monomorphic, to aid with
readability and type inference. The classes are:
- `ToString`, with method `asString`;
- `ToByteString`, with method `asByteString`;
- `ToLazyByteString`, with method `asLazyByteString`;
- `ToByteStringBuilder`, with method `asByteStringBuilder`;
- `ToText`, with method `asText`;
- `ToLazyText`, with method `asLazyText`; and
- `ToTextBuilder`, with method `asTextBuilder`.

Design goals in order of importance:
1. correctness (including totality);
2. efficiency; and
3. ease of use.
The author hopes that the design achieves these goals to a high degree.

To comment on correctness, we are using lenient UTF-8 decoding when converting
between `ByteString`s and `Text`s, which means the conversions are correct in
the sense that they are total, but they are incorrect in the sense that they may
replace some unicode characters with the usual replacement character. Since this
library serves a very general purpose, users will not necessarily place any
restrictions on the inputs of these conversion functions, so the author thought
that totality was important. Notably, this choice agrees with the behavior of
many text editors and readers that use similar decoding strategies.

To comment on efficiency, we avoid unnecessary intermediate conversions where
possible. For example, instead of converting `Text` to `ByteString` through
`String` (a design admirable in that it is simple, correct, and a quantity of
code that is linear in number of supported datatypes), we choose to use a
bespoke conversion function for every pair of supported datatypes wherever
possible (resulting in a quadratic quantity of code). We also avoid unnecessary
allocation. For example, instead of converting `LazyByteString` to `Text`
through `LazyText` (an approach that would entail creating a new rope of `Text`
that would immediately be converted into a single array), we convert
`LazyByteString` to `Text` through `ByteString` (which is a single array). We
don't have any benchmarks to support this design choice, admittedly, as we
consider the cost of creating a benchmark suite overrides the benefits of having
one at this time. Should interest in this library grow, we may revisit this
decision.
