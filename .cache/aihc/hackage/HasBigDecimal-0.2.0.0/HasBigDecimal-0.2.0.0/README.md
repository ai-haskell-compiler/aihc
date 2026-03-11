[![Actions Status](https://github.com/thma/HasBigDecimal/workflows/Haskell-CI/badge.svg)](https://github.com/thma/HasBigDecimal/actions)

# HasBigDecimal

This module defines the type 'BigDecimal' which provides a representation of arbitrary precision decimal numbers.
'BigDecimal' is a native Haskell implementation based on arbitrary sized 'Integer' values.
The implementation was inspired by Java BigDecimals. It aims to provide a simple to use API.

```haskell
-- | BigDecimal is represented by an unscaled Integer value and a Natural that defines the scale
--   E.g.: (BigDecimal value = 1234, scale = 2) represents the decimal value 12.34.
data BigDecimal = BigDecimal
  { 
    value :: Integer,  -- ^ the unscaled Integer value    
    scale :: Natural   -- ^ the scale (i.e. the number of digits after the decimal point)
  }
```

BigDecimal instantiates the following typeclasses:

```haskell
instance Eq BigDecimal
instance Ord BigDecimal

instance Num BigDecimal
instance Fractional BigDecimal
instance Real BigDecimal

instance Read BigDecimal
instance Show BigDecimal
```

It is thus possible to use all common numerical operations on operators like '+', '-', '*', '/', '^' on them.


# Some examples from a ghci REPL
```haskell
λ> a = BigDecimal 144 2
λ> a
1.44
λ> b = sqrt a
λ> b
1.2
λ> b * b
1.44
λ> b^3
1.728

λ> c = fromString "123.4567890"
λ> c
123.4567890

λ> value c
1234567890
λ> scale c
7

λ> a / c
0.01166400010614240096589584878965222398584
λ> roundBD it (halfUp 10)
0.0116640001
λ> divide (a, c) (halfUp 20)
0.01166400010614240097


```

# BigFloating
in addition to the pretty complete BigDecimal module there is the rather scetchy BigFloating module.
BigFloating contains a few first steps to let `BigDecimal` instantiate the `Floating` typeclass.
As of now it contains arbitrary precision implementations for pi (based on Chudnovskis algorithm), sqrt and nthroot (based on Newtons classic algorithm).
All trigonometric functions, log and exp are still missing.
All code contributions are most welcome!

Here are some working examples:

```haskell
λ> r = sqrt (BigDecimal 2 0)
λ> r
1.4142135623730950488016887242096980785696718753769480731766797379907324784621070388503875343276415727
λ> r^2*pi
6.2831853071795864769252867665590057683943387987502116419498891846156328125724179972560696506842341354888751599627582719047851094900943142191176629514606739
28547017151357805018682925970564827587058974690236729643325013696514697383143361638452329945607739055327681644609147889519349178329780951524191191

λ> sqr 2 (halfUp 50)
1.41421356237309504880168872420969807856967187537695
λ> sqr 2 (halfUp 500)
1.4142135623730950488016887242096980785696718753769480731766797379907324784621070388503875343276415727350138462309122970249248360558507372126441214970999358
314132226659275055927557999505011527820605714701095599716059702745345968620147285174186408891986095523292304843087143214508397626036279952514079896872533965
463318088296406206152583523950547457502877599617298355752203375318570113543746034084988471603868999706990048150305440277903164542478230684929369186215805784
6311159666871301301561856898723724

λ> piChudnovsky (halfUp 500)
3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679821480865132823066470938446095505822317253594081284811
174502841027019385211055596446229489549303819644288109756659334461284756482337867831652712019091456485669234603486104543266482133936072602491412737245870066
063155881748815209209628292540917153643678925903600113305305488204665213841469519415116094330572703657595919530921861173819326117931051185480744623799627495
6735188575272489122793818301194913


```

