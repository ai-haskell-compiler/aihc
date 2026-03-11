## 0.8.0 (2024-10-12)
* tweak `Simplify` exports (should stay fairly stable now)

## 0.7.0 (2024-10-11)
* add predicate simplifier at `Rerefined.Simplify`
  * old "normalizer" stuff removed

## 0.6.0 (2024-10-01)
* remove `Via` predicate, only need `validateVia`
* swap `validateBool` args

## 0.5.1 (2024-06-11)
* add `Via` predicate

## 0.5.0 (2024-05-26)
* add tentative operator versions of predicates
* clear up WIP normalization story

## 0.4.0 (2024-05-11)
* add `Eq`, `Ord`, `Arbitrary` instances
* simplify modules (fewer)
* add `reifyPredicate`, `reifyPredicate1` for reifying predicates to `a -> Bool`

## 0.3.0 (2024-05-07)
* refactor predicate names: now handled with a sort of type-level `Show`. no
  `Typeable`, lots of custom prettiness (infix operators!)
* refactor logical predicates, keeping them together doesn't help
* define relational operators using `Ordering` and logical `Or`
* add missing `Foldable`, `Traversable` instances to `Refined1`
* general cleanup

## 0.2.0 (2024-05-01)
* add missing `Show` instance to `RefineFailure`
* add missing `Functor` instance to `Refined1`
* general cleanup

## 0.1.0 (2024-04-30)
Initial release.

* rewrite of Nikita Volkov's refined library
