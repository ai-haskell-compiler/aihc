# 0.1.1.1

- Removed `mtl` dependency

# 0.1.1.0
- Add tests
- Mark `FromToml Value String` as overlapping - now it can be derived for newtypes
- Rename old `pTable` into `pTableL`, introduce `pTable` with produces value without location
- Rework `FromToml b (L a)` to delegate work to `FromToml b a`

# 0.1.0.0
Initial release

