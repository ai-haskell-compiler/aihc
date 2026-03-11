# *diff-loc*: Map file locations across diffs [![Hackage](https://img.shields.io/hackage/v/diff-loc.svg)](https://hackage.haskell.org/package/diff-loc)

## Example

You have a diff between two versions of a file. Given a source span
in one version, find the corresponding span in the other version.

For example, here is a diff `d` between a source string `"abcdefgh"` and a target
string `"appcfgzzh"`, with deletions and insertions in the middle:

```
  ab cdefg  h
-  b  de
+  pp     zz
  appc  fgzzh
```

Diffs are represented by the type `Diff`.
Only locations and lengths are recorded, not the actual characters.

```
import DiffLoc
import DiffLoc.Unsafe (offset)

d :: Diff N
d = addDiff (Replace 1 (offset 1) (offset 2))  -- at location 1, replace "b" (length 1) with "pp" (length 2)
  $ addDiff (Replace 3 (offset 2) (offset 0))  -- at location 3, replace "de" with ""
  $ addDiff (Replace 7 (offset 0) (offset 2))  -- at location 7, replace "" with "zz"
  $ emptyDiff
-- N.B.: replacements should be inserted right to left, starting from 'emptyDiff'.
```

The span `s` of `"fg"` in the first string is an interval that starts at
location 5 and has length 2.

```
s :: Interval N
s = 5 :.. offset 2
```

Illustration of the span:

```
 a b c d e f g h
0 1 2 3 4 5 6 7 8
          ^f+g+ length 2
          ^
          start 5
```

After applying the diff, the span has been shifted to location 4.

```
>>> mapDiff d (5 :.. offset 2)
Just (4 :.. offset 2)
```

```
 a p p c f g q q h
0 1 2 3 4 5 6 7 8 9
        ^f+g+ length 2
        ^
        start 4
```

Conversely, we can map spans from the target string to the source string of the diff:

```
>>> comapDiff d (4 :.. offset 2)
Just (5 :.. offset 2)
```

If part of the input span is modified by the diff, there is no
corresponding output span.

```
>>> mapDiff d (1 :.. offset 2)  -- "bc" contains "b" which is edited by the diff
Nothing
```

See the API documentation in [`DiffLoc`](https://hackage.haskell.org/package/diff-loc/docs/DiffLoc.html).
