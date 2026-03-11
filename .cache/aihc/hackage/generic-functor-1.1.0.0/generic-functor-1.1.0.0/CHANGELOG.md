## 1.1.0.0

- Add `Functor` and `Foldable` instances for `GenericBifunctor`, anticipating them becoming superclasses of `Bifunctor` and `Bifoldable`

## 1.0.0.0

- Split `Generic.Functor`, moving `gsolomap`, `solomap`, `gmultimap`, `multimap` to `Generic.Functor.Multimap`

## 0.2.0.0

- Add `gfoldMap`, `gtraverse`, `gbifoldMap`, `gbitraverse` `GFoldable`, `GFoldMap`, `GTraversable`, `GTraverse`, `GBiFoldable`, `GBifoldMap`, `GBitraversable`, `GBitraverse`
- Rename `GBifunctor` to `GBimap`, and add new `GBifunctor` (class synonym for `GBimap`, `GFirst`, and `GSecond`)
- Rename `DeriveFunctor` to `GenericFunctor`, and rename `DeriveBifunctor` to `GenericBifunctor`
- Add instances for deriving-via `Foldable` and `Bifoldable`

## 0.1.0.0

- Add `gmultimap`, `multimap`, `(:+)`
- Add `DeriveBifunctor`, `gbimap`, `gfirst`, `gsecond`

## 0.0.1.1

* Include README

## 0.0.1.0

* Create generic-functor
