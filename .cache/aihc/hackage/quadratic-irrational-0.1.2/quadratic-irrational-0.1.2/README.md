# `quadratic-irrational`

[![Build Status](https://travis-ci.org/ion1/quadratic-irrational.svg)](https://travis-ci.org/ion1/quadratic-irrational) [![Hackage](https://budueba.com/hackage/quadratic-irrational)](https://hackage.haskell.org/package/quadratic-irrational)

A library for exact computation with [quadratic irrationals][qi] with support
for exact conversion from and to [(potentially periodic) simple continued
fractions][pcf].

[qi]:  http://en.wikipedia.org/wiki/Quadratic_irrational
[pcf]: http://en.wikipedia.org/wiki/Periodic_continued_fraction

A quadratic irrational is a number that can be expressed in the form

```
(a + b √c) / d
```

where `a`, `b` and `d` are integers and `c` is a square-free natural number.

Some examples of such numbers are

* `7/2`,

* `√2`,

* `(1 + √5)/2` ([the golden ratio][gr]),

* solutions to quadratic equations with rational constants – the [quadratic
  formula][qf] has a familiar shape.

[gr]: http://en.wikipedia.org/wiki/Golden_ratio
[qf]: http://en.wikipedia.org/wiki/Quadratic_formula

A simple continued fraction is a number in the form

```
a + 1/(b + 1/(c + 1/(d + 1/(e + …))))
```

or alternatively written as

```
[a; b, c, d, e, …]
```

where `a` is an integer and `b`, `c`, `d`, `e`, … are positive integers.

Every finite SCF represents a rational number and every infinite, periodic SCF
represents a quadratic irrational.

```
3.5      = [3; 2]
(1+√5)/2 = [1; 1, 1, 1, …]
√2       = [1; 2, 2, 2, …]
```
