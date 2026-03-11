[hackage-binrep]: https://hackage.haskell.org/package/binrep

# bytezap
Build strict bytestrings with zero intermediate allocation.

If you're looking for general high-performance serialization, you probably want
[mason](https://hackage.haskell.org/package/mason). But if you're dealing with
data that is already "shaped" like binary data, e.g. using types defined in
[binrep][hackage-binrep], and you want the best performance possible, read on...

## Why?
Most binary serialization libraries tend towards a model where the serializer
itself handles allocation. In the plumbing, serialization operations are
bracketed by a check to ensure the current buffer has enough space for the next
operation; if not, we obtain more space in some way, and serialization
continues. This design is nice because we can _chunk_ the serializing:

* for writing to a lazy bytestring, we can emit a new chunk and clear our buffer
* for writing to a handle, we can write, flush and clear our buffer
* for writing to a strict bytestring, we must grow our current buffer (meh)

But if we know the size of the serialized data _before_ serializing it, we don't
need those space checks, nor these intermediate steps. We may allocate a single
buffer with the required size upfront, then use that as we like.

Great, you say, but most data isn't so simple that we can easily calculate its
serialized length without actually performing the serialization. This is true.
bytezap is designed specifically for cases where

* it's easy to calculate the serialized length of your data, and
* you want to write to a strict bytestring (one big contiguous block of memory)

This last point notably may limit usage for serializing large data, depending on
memory limitations. In most cases, we'll use more memory than a buffering
library such as mason.

## So... why?
Well, bytezap will be slightly faster where it's applicable, and the
implementation is extremely simple. It's a fun niche to fill, and it's
convenient for my [binrep][hackage-binrep] library.

## Struct handling
We define even simpler parser & serializer types which can only handle "C
struct"-like types, where there is only one constructor and all fields have
known length at compile time. The way these work, GHC is pretty much guaranteed
to generate the fastest code possible. These are very experimental, but see
[binrep][hackage-binrep] for example usage.

## Non-features
### Serialize to `ByteString` (pinned byte arrays) only
No `ShortByteString`s, no writing directly to handles.

(One _may_ support writing to `ShortByteString`s (unpinned byte arrays) by doing
a bunch of class indirection. But it's a lot of extra work for a use case that I
don't see as very popular at all. Check the Git history for an early
implementation.)

## License
Provided under the MIT license. See `LICENSE` for license text.
