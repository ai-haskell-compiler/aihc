# mt19937
Efficient MT19937 (standard 32-bit Mersenne Twister PRNG) implementation, in pure Haskell
(no FFI) with a pure interface.

Only the PRNG itself is implemented. If you'd like to use this as a PRNG in any
capacity, you'll need to wrap it into some monad or some such.

Doesn't seem too useful? You'd be surprised. This was originally written while
decoding a file header which was bafflingly encoded using a set-seed MT19937.

## License
Provided under the MIT license. See `LICENSE` for license text.
