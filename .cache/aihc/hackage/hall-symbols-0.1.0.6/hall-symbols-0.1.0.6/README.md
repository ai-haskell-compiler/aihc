# hall-symbols

Haskell Hall Symbols Library

## Quickstart

Make new stack project and move to project directory.

```shell
% stack new hmRepl
% cd hmRepl
```

Edit extra-deps part of stack.yaml like below.

```
extra-deps:
- matrix-as-xyz-0.1.1.3
- symmetry-operations-symbols-0.0.1.2
- hall-symbols-0.1.0.6
```

Edit dependencies part of package.yaml like below.

```
dependencies:
- base >= 4.7 && < 5
- matrix-as-xyz
- symmetry-operations-symbols
- hall-symbols
```

Then start repl.

```shell
% stack repl
```

Setup packages and load modules.

```haskell
repl> :m Data.Matrix.AsXYZ Data.Matrix.SymmetryOperationsSymbols Crystallography.HallSymbols
```

Use like below.

```haskell
-- print General Positions.
repl> prettyXYZ <$> fromHallSymbols' "C -2yc"
 ["x,y,z","x+1/2,y+1/2,z","x,-y,z+1/2","x+1/2,-y+1/2,z+1/2"]

repl> fromHallSymbols' "C -2yc" >>= fromMatrix'
[" 1 "," c  x,0,z"," t (1/2,1/2,0) "," n (1/2,0,1/2) x,1/4,z"]

```

Or use like below.

```haskell
-- print Generators
repl> prettyXYZ <$> generatorsOfHallSymbols "C -2yc"
["x,y,z","x+1/2,y+1/2,z","x,-y,z+1/2"]

repl> generatorsOfHallSymbols "C -2yc" >>= fromMatrix'
[" 1 "," t (1/2,1/2,0) "," c  x,0,z"]

```

## References

1. Concise Space-Group Symbols http://cci.lbl.gov/sginfo/hall_symbols.html , See also : https://github.com/rwgk/sginfo

2. Space-Group Notation with an Explicit Origin
   S.R. Hall; Space-Group Notation with an Explicit Origin ; Acta Cryst. (1981). A37, 517-525

3. ITVB 2001 Table A1.4.2.7 Hall symbols http://cci.lbl.gov/sginfo/itvb_2001_table_a1427_hall_symbols.html

## License

See the [LICENSE](https://raw.githubusercontent.com/narumij/hall-symbols/master/LICENSE)
file in the repository.
