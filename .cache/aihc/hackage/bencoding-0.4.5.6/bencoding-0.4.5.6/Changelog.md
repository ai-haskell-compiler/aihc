# 0.4.5.6

- Fix build for GHC 9.10

# 0.4.5.5

- Fix build for GHC 9.6

# 0.4.5.2

- Fix build for GHC 8.8.1

# 0.4.5.1

- Fix benchmark build when dependency on AttoBencode is switched off

# 0.4.5.0

- Test against GHC 8.6. Make benchmark dependency on AttoBencode disableable

# 0.4.4.0

- Fix build for GHC 8.0-8.4. Derive Generic instances. Add Semigroup instance.

# 0.4.3.0

- Add lookAhead and match functions.

# 0.4.2.1

- Override default Monad(fail) method so it is possible to catch Get monad failures from pure code.

# 0.4.2.0

- Override default fixity for operators. Previously it has been impossible to mix bencode Get operators (`(<$>!)`, `(<$>?)`, `(<*>!)`, `(<*>?)`) with applicative operators (`(<$>)`, `(<*>)`).

# 0.4.1.0

- Expose parser and builder so it is possible to use parser incrementally.

# 0.4.0.2

- Minor fixes.

# 0.4.0.1

- Nothing changed.

# 0.4.0.0

- Faster dictionary conversion.

# 0.3.0.0

- Rename `BEncode` to `BValue` and `BEncodable` to `BEncode`.

# 0.2.2.0

- Arbitrary length integers. (by specification)

# 0.2.0.0

- Added default decoders/encoders using GHC Generics.

# 0.1.0.0

- Initial version.
