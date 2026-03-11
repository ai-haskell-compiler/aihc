-- | Descriptive type errors for generic representation assertion failures.
--
-- These are potentially useful separately from the asserts, in places where
-- we're unable to perform asserts which evaluate to 'Constraint's e.g. type
-- families, so we expose them neatly here.

module Generic.Type.Assert.Error where

import GHC.TypeError
import GHC.TypeLits ( type Symbol )

-- polymorphic kind keeps us extra useful -- we only use as 'Constraint', but
-- library users may want to use this in type families
type GAssertError :: ka -> Symbol -> k
type GAssertError a msg = TypeError
         ('Text "Assertion on generic representation failed for type: "
    :<>: 'ShowType a
    :$$: 'Text "Message: " :<>: 'Text msg)

type GAssertErrorVoid a =
    GAssertError a "not non-void type (>=1 constructor)"

type GAssertErrorSum a =
    GAssertError a "not non-sum type (1 constructor)"

type GAssertErrorNotSum a =
    GAssertError a "not sum type (>=2 constructors)"

type GAssertErrorNotEnum a =
    GAssertError a "not enum type (all empty constructors)"
