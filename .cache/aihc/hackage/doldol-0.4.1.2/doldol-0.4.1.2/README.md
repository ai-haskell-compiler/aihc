doldol
====

doldol packs multiple flaggable data in a `Flag` value,
and checks its flags by cheap operators.

In the current version, doldol will handle `Enum e` in a list only.

# The name

`doldol` comes from Korean word **돌돌** which is a mimetic presenting *roll up fabrics or something like flags*.

# Gap analysis

## EnumSet

EnumSet is good library, but only works with List (ex. `toEnum`).
I also need List interface only. However, next version of my simulation project needs to support `Traversal` class. Therefore, I keep to develop this.
Of course, you can use EnumSet with `fromList` or etc., but when you consider performance seriously, let think one more time.
