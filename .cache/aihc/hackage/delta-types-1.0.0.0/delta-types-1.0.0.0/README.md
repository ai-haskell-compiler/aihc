## Overview

This package provides a notion of *delta types* via the `Delta` typeclass.

A *delta type* `da` for a *base type* `a` is a collection of values that each correspond to a change `a → a` of the base type. The following typeclass captures this correspondence:

    class Delta da where
        type Base da = a
        apply :: da → (a → a)

In the literature, this concept is also known as a *change action*, with relations to incremental computation and differential lambda calculus. See References.

For example, one delta type for `Set a` is given by

    data DeltaSet1 a = Insert a | Delete a
    
    instance Delta (DeltaSet1 a) where
        type Base (DeltaSet1 a) = Set a
        apply (Insert a) = Data.Set.insert a
        apply (Delete a) = Date.Set.delete a

In general, there may be multiple delta types associated with a single base type.

## References

1. Alvarez-Picallo, M., Eyers-Taylor, A., Peyton Jones, M., Ong, CH.L. (2019). [Fixing Incremental Computation][1811.06069]. In: Caires, L. (eds) Programming Languages and Systems. ESOP 2019. Lecture Notes in Computer Science(), vol 11423. Springer, Cham.
2. M. Alvarez-Picallo, M. (2020). [Change actions: from incremental computation to discrete derivatives][2002.05256]. PhD thesis, Oxford.
3. Apfelmus, H. (2023). [Delta encodings help separate business logic from database operations][bobkonf2023]. Bobkonf 2023, Berlin.

  [bobkonf2023]: https://bobkonf.de/2023/apfelmus.html
  [2002.05256]: https://arxiv.org/abs/2002.05256
  [1811.06069]: https://arxiv.org/abs/1811.06069