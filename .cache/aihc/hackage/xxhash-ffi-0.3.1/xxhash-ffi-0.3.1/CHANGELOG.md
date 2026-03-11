# 0.3.1

* Update bundled `xxhash` C library to 0.8.3.
* A few critical bug fixes.

# 0.3

* Update bundled `xxhash` C library to 0.8.2.
* Add FFI bindings for `XXH3` family of hash functions.
* Add `newtype XXH3` and `instance Hashable` for it.
* Deprecate `class XXHash`, please use `XXH3`-based API instead.
