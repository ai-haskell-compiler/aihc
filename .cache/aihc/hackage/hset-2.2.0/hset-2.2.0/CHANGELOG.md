# TODO
* `HDiff` typeclass to authomatically calculate difference between two hsets
* Use `Data.Map TypeRep Dynamic` as internal representation of hset to
  increase access speed to arbitrary fields
* Add lens support

# CHANGELOG

## 2.2.0
### Removed
* Module `Data.HSet.TypeLevel` removed

### Changed
* added dependency on `type-fun`
* code rewritten with use of `type-fun` instead of `TypeLevel`
* type class `SubHSet` simplified for constraint solver happiness

### Added
* `AllHGettable` typefamily for convenience

## 2.1.0
### Changed
* `hgetTagged` moved to module `Data.HSet.Get`
* fixed fundep of `HModify`
### Added
* functions to work with tagged types in `Data.HSet.Modify`
* type families to work with type lists, like `Union` `Replace`
  `TagElem`

## 2.0.0
### Changed
* `HGetable` renamed to `HGettable` because of popular grammar nazi demand.
* Monolithic module `Data.HSet` is splitted to submodules
* `HSet` is now lazy on it's tail because it must be ... I suppose.
### Added
* `NFData` instance for `HSet`
* `HRemove` typeclass to remove specific element from `HSet` by index
* `HUnion` typeclass to merge two subsets
* `HModify` typeclass to modify value and type of arbitrary element of `HSet`
### Deleted
* `mtl` dependency dropped in favor of using lens combinators for
  `MonadReader` and `MonadState`

## 1.1.0
### Added
* `SubHSet` typeclass and instances to, yes, get arbitrary subhsets of
  arbitrary hset.
* More type level fun for future stuff.
## 1.0.1
### Changed
* Grammar nazi fixed horrible bugs

## 1.0.0
### Changed
* type family `Contains` renamed to `HGetable` to not clash names with
  lens

## 0.1.2
### Added
* Facepalm `Applicative` constraint

## 0.1.1
### Added
* DeriveDataTypeable to default-extensions

## 0.1.0
### Added
* `Labeled`: newtype wrapper with anonymous type parameter of any
  kind. Usefull if you want just put several same-typed things to hset
* Instances of `Show`, `Eq`, `Ord` for `HSet`
* Tests added
### Updated
* TypeLevel is polykinded now
* base restricted up to >= 4.7
* HSet is strict now
* Docs improved

## 0.0.3
### Added
* Export typefamily `Contains`
* Export `hask`
### Updated
* Make it compilable on base < 4.8

## 0.0.2
### Added
* Add `hask` to read from MonadReaer

## 0.0.1
The first compilable and working version
