# trie-simple

Trie data structure `TMap` to hold mapping from list of characters to
something, i.e. isomorphic to `Map [c] v`.
This package also contains `TSet`, which is isomorphic to `Set` of lists of
characters.

This package implements these structures using `Map` from containers
package, and require the character type to be only `Ord`.

Advantages of using this package over `Map` or `Set` are:

  * 2x Faster `lookup` (`member`) operation
  * Retrieving subset of map or set with given prefix
  * `append`, `prefixes`, and `suffixes` support
  * Can be more memory-efficient (but not always; needs
    benchmark anyway).

## Benchmarks

Benchmarks compared against plain `Map` and `Set`. 

![benchmark chart for TMap](https://raw.githubusercontent.com/viercc/trie-simple/master/doc/ratio-map.png)

![benchmark chart for TSet](https://raw.githubusercontent.com/viercc/trie-simple/master/doc/ratio-set.png)

Each of these benchmarks has two sets of point and errorbars, representing two datasets they are run against.

**Notice**: the datasets are **not** included in the tarball distributed from Hackage.
To run the benchmark, either get the entire repository from the source repository (GitHub) or
modify the benchmark program and supply any dataset of your choice.

## About License

[LICENSE](LICENSE) tells the licence of this project, EXCEPT
one file for benchmark input data. See [ABOUT](ABOUT) for that
file.

If you install `trie-simple` from Hackage, that input data is not
included in the distributed files.
