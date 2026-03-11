# Summary

Data structure intended for accumulating a sequence of elements
for later traversal or folding.
A great basis for implementing many custom monoids,
most notably of the Builder pattern.

It shines with its `Monoid` instance,
which relieves the user from caring about from which side to append.
This is important because,
different data-structures exhibit very different performance depending on that.
Most notably linked list.
`Acc` on the other hand is neutral and performs well in all scenarios.

For such purposes it is common to use `Seq` or `DList`.
The benchmark results below show that `Acc` is a better fit.

# Benchmark results

These benchmarks compare the performance of `Acc` vs. various other structures
as used for aggregation with intent of reduction.

In other words a two-step process of the following structure is measured as a whole:

1. Construct the measured data-structure using a particular method (`cons`, `snoc`, `fromList`)
2. Fold the data-structure into a final result (`sum`, `length`)

Following are the highlights from the benchmark results
grouped by the method of construction of the datastructure.

### Consing 1000 elements

```
acc               12.40 μs
list              18.70 μs
dlist             43.95 μs
sequence          27.54 μs
```

### Snocing 1000 elements

```
acc               17.02 μs
dlist             38.93 μs
sequence          27.15 μs
```

_No list here because it will blow up the memory._

### Construction from a list of 1000 elements

```
acc               13.27 μs
list              12.97 μs
dlist             27.57 μs
sequence          10.70 μs
```

### Appending chunks of 1000 elements 1000 times from left

```
acc               4.256 ms
list              553.7 ms
dlist             315.9 ms
sequence          10.05 ms
```

### Appending chunks of 1000 elements 1000 times from right

```
acc               4.305 ms
list              5.126 s
dlist             360.4 ms
sequence          7.209 ms
```

---

For complete results see [the dump](bench-results).

_Executed on an AWS c6i.2xlarge instance running Ubuntu._

## Conclusions

Given the preconditions of the benchmarks, the following can be concluded:

- Neither list or `DList` are suitable as monoidal structures, due to exponential performance degradation on appends from both sides
- Snocing and even consing `Acc` is better than all alternatives
- `Acc` performs better than `Seq` on both left- and right-appends (2-3x)
- `Seq` gets constructed from list faster than `Acc` (1.5x)
