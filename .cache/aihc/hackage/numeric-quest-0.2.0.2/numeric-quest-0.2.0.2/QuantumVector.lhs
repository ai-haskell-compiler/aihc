<html>
<head>
<BASE HREF="http://www.numeric-quest.com/haskell/QuantumVector.html">

<title>
         Quantum vector
</title>
</head>
<body>
<center>
<h1>
        ***
</h1>
<h1>
        Quantum vector
</h1>
<p>
<b>

        Jan Skibinski, <a href=http://www.numeric-quest.com/news/>
        Numeric Quest Inc.</a>, Huntsville, Ontario, Canada
<br>
        Literate Haskell module <em>QuantumVector.lhs</em>
<p>
        Initialized: 2000-05-31, last modified: 2000-06-10
</b>
</center>

<blockquote>
<em>
<p>
<hr>
<p>
        This is our attempt to model the abstract Dirac's formalism
        of Quantum Mechanics in Haskell. Although we have been
        developing quantum mechanical applications and examples for some time [2], the
        machinery used there is tightly coupled to a concrete
        representation of states and observables by complex vectors
        and matrices. implemented mainly as Haskell lazy lists.
<p>
        However, the Dirac's formalism in Hilbert space is much more
        abstract than that, and many problems of Quantum Mechanics can be
        solved without referring to any
        particular matrix representation, but using certain generic properties
        of operators, such as their commutative relations instead.
        Haskell seems to be well suited for such abstract tasks,
        even in its current form that does not support any
        of the abstract notions of computer algebra as yet.
        This has been already recognized by Jerzy Karczmarczuk [1],
        where he proposes a very interesting representation of Hilbert
        space and illustrates it by several powerful examples.
        But the task is not trivial and far from being complete.
        Quantum Mechanics presents many challenges to any formalism
        and only by careful examination of many of its facets
        and alternative approaches, a consistent model of
        Dirac's formalism can be developed for Haskell. Hoping to
        help with solving this problem, we present here a computing
        abstract, which is quite different from that of [1].
<p>
        We recognize a quantum state as an abstract vector | x &gt;,
        which can be represented in one of many possible bases -- similar
        to many alternative representations of a 3D vector in rotated systems
        of coordinates. A choice of a particular basis is controlled
        by a generic type variable, which can be any Haskell object
        -- providing that it supports a notion of equality and ordering.
        A state which is composed of many quantum subsystems, not
        necessarily of the same type, can be represented in a vector
        space considered to be a tensor product of the subspaces.

<p>
        With this abstract notion we proceed with Haskell definition of two
        vector spaces: Ket and its dual Bra. We demonstrate
        that both are properly defined according to the abstract
        mathematical definition of vector spaces. We then introduce inner
        product and show that our Bra and Ket can be indeed
        considered the vector spaces with inner product. Multitude
        of examples is attached in the description. To verify
        the abstract machinery developed here we also provide the basic library
        module <a href="http://www.numeric-quest.com/haskell/Momenta.html">
        Momenta</a> -- a non-trivial example designed to compute Clebsch-Gordan coefficients
        of a transformation from one basis of angular momenta to another.
<p>
        Section 6 is a rehash of known definitions of linear operators
        with the emphasis on both Dirac and Haskell notations and on
        Haskell examples. The formalism developed here centers around
        two operations: a scalar product of two vectors, <b>x &lt;&gt; y</b>,
        and a closure operation, <b>a &gt;&lt; x</b>, which can be considered
        an application of a quantum operator <b>a</b> to a vector <b>x</b>.
        At this stage our formalism applies only to discrete cases, but
        we hope to generalize it on true Hilbert space as well.
</em>
<p>
<hr>
<p>
<b>
        Contents
</b>
<ul>
<li>
        1. Infix operators
<li>
        2. Vector space
<li>
        3. Ket vector space
<li>
        4. Bra vector space
<li>
        5. Bra and Ket spaces as inner product spaces
<li>
        6. Linear operators
<ul>
<li>            6.1. Operator notation
<li>
                6.2. Renaming the representation
<li>
                6.3. Closure formula, or identity operator
<li>
                6.4. Changing the representation
<li>
                6.5. Implementation of the operator equation A | x &gt; = | y &gt;
<li>
                6.6. Inverse operator
<li>
                6.7. Matrix representation of an operator
<li>
                6.8. Adjoint operator
<li>
                6.9. Unitary operator
<li>
                6.10. Hermitian operator
</ul>
<li>
        7. Showing kets and bras
<li>
        8. Data Tuple for tensor products
<li>
        9. References
<li>
        10. Copyright and license

</ul>

<p>
<hr>
<p>
<b>
        1. Infix operators
</b>
<p>
        Haskell requires that fixities of infix operators are defined
        at the top of the module. So here they are. They are
        to be explained later.

</b>
<pre>

> module QuantumVector where
> import Data.Complex -- our Scalar is Complex Double
> import Data.List (nub)
> import Prelude2010
> import Prelude ()

> infixl 7 *>  -- tensor product of two kets
> infixl 7 <*  -- tensor product of two bras

> -- scalar-ket multiplication
> infix 6 |>
> -- scalar-bra multiplication
> infix 6 <|


> infixl 5 +>  -- sum of two kets
> infixl 5 <+  -- sum of two bras


> infix 4 <>  -- inner product
> infix 5 ><  -- closure

</pre>
<p>
<hr>
<p>
<b>
        2. Vector space
</b>
<p>
        Definition. A set V of elements x ,y ,z ,...is called a vector
        (or linear) space over a complex field C if
<ul>
<li>
        (a) vector addition  + is defined in V such that V is an
        abelian group under addition, with identity element 0
<pre>
        1: <b>x</b> + <b>y</b>       = <b>y</b> + <b>x</b>
        2: <b>x</b> + (<b>y</b> + <b>z</b>) = (<b>x</b> + <b>y</b>) + <b>z</b>
        3: <b>0</b> + <b>x</b>       = <b>x</b> + <b>0</b>

</pre>
<p>
<li>
        (b) the set is close with respect to scalar multiplication
        and vector addition
<pre>
        4: a (<b>x</b> + <b>y</b>)   = a <b>x</b> + a <b>y</b>
        5: (a + b) <b>x</b>   = a <b>x</b> + b <b>x</b>
        6: a (b <b>x</b>)     = (a b) <b>x</b>
        7: 1 <b>x</b>         = <b>x</b>
        8: 0 <b>x</b>         = <b>0</b>
            where
                a, b, c are complex scalars
</pre>
</ul>
        Definition. The maximum number of linearly independent vectors
        in V or, what is the same thing, the minimum number of linearly
        independent vectors required to span V is the dimension r of
        vector space V.
<p>
        Definition. A set of r linearly independent vectors is called
        a basis of the space. Each vector of the space is then a unique
        linear combination of the vectors of this basis.
<p>
        Based on the above definitions we will define two vector
        spaces: ket space and its dual -- bra space, which, in addition
        to the above properties, will also support
        several common operations -- grouped below in the class
        DiracVector.
<pre>

> class DiracVector a where
>     add        :: a -> a -> a
>     scale      :: Scalar -> a -> a
>     reduce     :: a -> a
>     basis      :: a -> [a]
>     components :: a -> [Scalar]
>     compose    :: [Scalar] -> [a] -> a
>     dimension  :: a -> Int
>     norm       :: a -> Double
>     normalize  :: a -> a

>     dimension x   = length (basis x)
>
>     normalize x
>         | normx == 0 = x
>         | otherwise  = compose cs (basis x)
>          where
>             cs     = [a*v :+ b*v |a :+ b <- components x]
>             v      = 1 / normx
>             normx  = norm x

</pre>
<p>
<hr>
<p>
<b>
        3. Ket vector space
</b>
<p>
        We submit that the following datatype and accompanying
        operations define a complex vector space, which we will call
        the ket vector space.
<pre>

> type Scalar = Complex Double

> data Ket a  =
>            KetZero                     -- zero ket vector
>          | Ket a                       -- base ket vector
>          | Scalar  :|> Ket a           -- scaling ket vectors
>          | Ket a   :+> Ket a           -- spanning ket space

</pre>

        A tensor product of two ket spaces is also a ket space.
<pre>

> (*>) :: (Ord a, Ord b) => Ket a -> Ket b -> Ket (Tuple a b)
> Ket a   *> Ket b    = Ket (a :* b)
> _       *> KetZero  = KetZero
> KetZero *> _        = KetZero
> x       *> y        = foldl1 (:+>) [((Bra a <> x) * (Bra b <> y)) :|> Ket (a :* b)
>                                   | Ket a <- basis x, Ket b <- basis y]


> (|>) :: Ord a => Scalar -> Ket a -> Ket a
>     --
>     -- Multiplication of ket by scalar
>     --
> s |> (x :+> y)  = (s |> x) +> (s |> y)
> _ |> KetZero    = KetZero
> 0 |> _          = KetZero
> s |> (s2 :|> x) = (s * s2) |> x
> s |> x          = s :|> x


> (+>) :: Ord a => Ket a  -> Ket a  -> Ket a
>     --
>     -- Addition of two kets
>     --
> x +> KetZero = x
> KetZero +> x = x
> x +> y       = reduce (x :+> y)


> instance (Eq a, Ord a) => Eq (Ket a) where
>     --
>     -- Two ket vectors are equal if they have identical
>     -- components
>     --
>     x == y = and [c k x == c k y  | k <- basis x]
>         where
>             c k z = (toBra k) <> z


</pre>
        The data Ket is parametrized by type variable "a", which can be
        anything that can be compared for equality and ordered: integer,
        tuple, list of integers, etc. For example, the data
        constructor <code>Ket (3::Int)</code> creates a base vector <code>|3></code>,
        annotated by Int.
        Similarly, <code>Ket (2::Int,1::Int)</code>, creates a base vector
        <code>|(2,1)></code> annotated by a tuple of Ints. Those two
        vectors belong to two different bases.
<p>
        The eight examples below illustrate the eight defining equations
        of the vector space, given in section 1. All of them evaluate
        to True.
<pre>

        1: Ket 2 +> Ket 3            == Ket 3 +> Ket 2
        2: Ket 1 +> (Ket 2 +> Ket 3) == (Ket 1 +> Ket 2) +> Ket 3
        3: Ket 1 +> KetZero          == KetZero +> Ket 1
        4: 5 |> (Ket 2 +> Ket 3)     == 5 |> Ket 2 +> 5 |> Ket 3
        5: (5 + 7) |> Ket 2          == 5 |> Ket 2 +> 7 |> Ket 2
        6: 2 |> (4 |> Ket 2)         == 8 |> Ket 2
        7: 1 |> Ket 2                == Ket 2
        8: 0 |> Ket 2                == KetZero
</pre>
        The ket expressions can be pretty printed, as shown below.
<pre>
        Ket 2 +> Ket 3        ==> 1.0 |2> + 1.0 |3>
        5 |> (Ket 2 +> Ket 3) ==> 5.0 |2> + 5.0 |3>
        2 |> (4 |> Ket 2)     ==> 8.0 |2>
</pre>
        In order to support all those identities we also need several
        additional functions for reducing the vector to its canonical form,
        for composing the ket vector, and for extracting the ket
        basis and the ket components -- as shown below.
<pre>


> reduceKet :: Ord a => Ket a -> Ket a
> reduceKet x
>     --
>     -- Reduce vector `x' to its canonical form
>     --
>     = compose cs ks
>       where
>           ks = basis x
>           cs = [toBra k <> x | k <- ks]


> ketBasis :: Ord a => Ket a -> [Ket a]
>     --
>     -- Sorted list of unique base vectors of the ket vector
>     --
> ketBasis KetZero        = []
> ketBasis (Ket k)        = [Ket k]
> ketBasis (_ :|> x)      = [x]
> ketBasis (k1 :+> k2)    = nub (ketBasis k1 ++ ketBasis k2)


> toBra :: Ord a => Ket a -> Bra a
>     --
>     -- Convert from ket to bra vector
>     --
> toBra (Ket k)           = Bra k
> toBra (x :+> y)         = toBra x :<+ toBra y
> toBra (p :|> x)         = (conjugate p) :<| toBra x


> instance Ord a => DiracVector (Ket a)  where
>     add           = (+>)
>     scale         = (|>)
>     reduce        = reduceKet
>     basis         = ketBasis
>     components x  = [toBra e <> x | e <- basis x]
>     compose xs ks = foldl1 (:+>) [fst z :|> snd z  | z <- zip xs ks]
>
>     norm KetZero  = 0
>     norm x        = sqrt $ realPart (toBra x <> x)


</pre>
        But those auxilliary functions refer to vectors from the
        conjugated space bra, which we shall now define below.
<p>
<hr>
<p>
<b>
        4. Bra vector space
</b>
<p>
        Definition. Let V be the defining n-dimensional complex vector
        space. Associate with the defining n-dimensional complex vector
        space V a conjugate (or dual) n-dimensional vector space
        obtained by complex conjugation of elements x in V.
<p>
        We will call this space the bra space, and the corresponding vectors
        - the bra vectors. Further, we submit that the following datatype and the corresponding
        operations define bra space in Haskell.
<pre>

> data Bra a =
>            BraZero                   -- zero bra vector
>          | Bra a                     -- base bra vector
>          | Scalar :<| Bra a          -- scaling bra vectors
>          | Bra a  :<+ Bra a          -- spanning bra space


</pre>
        A tensor product of two bra spaces is also a bra space.
<pre>

> (<*) :: (Ord a, Ord b) => Bra a -> Bra b -> Bra (Tuple a b)
> Bra a   <* Bra b    = Bra (a :* b)
> _       <* BraZero  = BraZero
> BraZero <* _        = BraZero
> x       <* y        = foldl1 (:<+) [((x <> Ket a) * (y <> Ket b)) :<| Bra (a :* b)
>                                   | Bra a <- basis x, Bra b <- basis y]

> (<|) :: Ord a => Scalar -> Bra a -> Bra a
> s <| (x :<+ y)  = (s <| x) <+ (s <| y)
> _ <| BraZero    = BraZero
> 0 <| _          = BraZero
> s <| (s2 :<| x) = (s * s2) <| x
> s <| x          = s :<| x


> (<+) :: Ord a => Bra a -> Bra a -> Bra a
>     --
>     -- Sum of two bra vectors
>     --
> x <+ BraZero = x
> BraZero <+ x  = x
> x <+ y       = reduce (x :<+ y)


> instance (Eq a, Ord a) => Eq (Bra a) where
>     --
>     -- Two bra vectors are equal if they have
>     -- identical components
>     --
>     --
>     x == y = and [c b x == c b y  | b <- basis x]
>         where
>             c b z = z <> toKet b

</pre>

        Similarly to what we have done for ket vectors, we also define several
        additional functions for reducing the bra vector to its canonical form,
        for composing the bra vector, and for extracting the bra
        basis and the bra components -- as shown below.
<pre>

> reduceBra :: Ord a => Bra a -> Bra a
> reduceBra x
>     --
>     -- Reduce bra vector `x' to its canonical form
>     --
>     = compose cs bs
>       where
>           bs = basis x
>           cs = [x <> toKet b | b <- bs]


> braBasis :: Ord a => Bra a -> [Bra a]
>     --
>     -- List of unique basis of the bra vector
>     --
> braBasis BraZero        = []
> braBasis (Bra b)        = [Bra b]
> braBasis (_ :<| x)     = [x]
> braBasis (b1 :<+ b2)   = nub (braBasis b1 ++ braBasis b2)


> toKet :: Ord a => Bra a -> Ket a
>     --
>     -- Convert from bra to ket vector
>     --
> toKet (Bra k)            = Ket k
> toKet (x :<+ y)        = toKet x :+> toKet y
> toKet (p :<| Bra k)    = (conjugate p) :|> Ket k


> instance Ord a => DiracVector (Bra a)  where
>     add           = (<+)
>     scale         = (<|)
>     reduce        = reduceBra
>     basis         = braBasis
>     components x  = [x <> toKet e | e <- basis x]
>     compose xs ks = foldl1 (:<+) [fst z :<| snd z  | z <- zip xs ks]
>
>     norm BraZero  = 0
>     norm x        = sqrt $ realPart (x <> toKet x)


</pre>
<p>
<hr>
<p>
<b>
        5. Bra and Ket spaces as inner product spaces
</b>
<p>

        Definition. A complex vector space V is an inner product space
        if with every pair of elements x ,y  from V there is associated
        a unique inner (or scalar) product < x | y > from C, such that
<pre>
        9:  < x | y >          = < y | x ><sup>*</sup>
        10: < a x | b y >      = a<sup>*</sup> b < x | y >
        11: < z | a x + b y >  = a < z | x > + b < z, y >
            where
                a, b, c are the complex scalars
</pre>
        We submit that the dual ket and bra spaces are inner product
        spaces, providing that the inner product is defined by the operator
        <> given below:
<pre>



> (<>) :: Ord a => Bra a -> Ket a -> Scalar
>     --
>     -- Inner product, or the "bra-ket" product
>     --
> BraZero       <> _              = 0
> _             <> KetZero        = 0
> Bra i         <> Ket j          = d i j
> (p :<| x)     <> (q :|> y)      = p * q * (x <> y)
> (p :<| x)     <> y              = p * (x <> y)
> x             <> (q :|> y)      = q * (x <> y)
> x             <> (y1 :+> y2)    = (x  <> y1) + (x <> y2)
> (x1 :<+ x2)   <> y              = (x1 <> y)  + (x2 <> y)


> d :: Eq a => a -> a -> Scalar
> d i j
>     --
>     -- Classical Kronecker's delta
>     -- for instances of Eq class
>     --
>     | i == j    = 1
>     | otherwise = 0
>

</pre>
        The expressions below illustrate the definitions 9-11.
        They are all true.
<pre>
9:  (toBra x <> y) == conjugate (toBra y <> x)
10: (toBra (a |> x) <> (b |> y)) == (conjugate a)*b*(toBra x <> y)
11: (toBra z <> (a |> x +> b |> y)) == a*(toBra z <> x) + b*(toBra z <> y)
    where
        x = (2 :+ 3) |> Ket 2
        y = ((1:+2) |> Ket 3) +> Ket 2
        z = Ket 2 +> Ket 3
        a = 2:+1
        b = 1
</pre>
<p>
<hr>
<p>
<b>
        6. Linear operators
</b>
<p>

        Linear operators, or simply operators, are functions from vector
        in representation a <em>a</em> to vector in representation <em>b</em>

<pre>
        a :: Ket a -> Ket b
</pre>
        although quite often the operations are performed
        on the same representation. The linear operators A are defined by
<pre>
        A (c1 | x > + c2 | y > ) = c1 A | x > + c2 A | y >
</pre>

<p>
        We will describe variety of special types
        of operators, such as inverse, unitary, adjoint and hermitian.
        This is not an accident that the names of those operators
        resemble names from matrix calculus, since
        Dirac vectors and operators can be viewed as matrices.
<p>
        With the exception of variety of examples, no significant
        amount of Haskell code will be added here. This section
        is devoted mainly to documentation; we feel that it is important
        to provide clear definitions of the operators, as seen from
        the Haskell perspective. Being a strongly typed language,
        Haskell might not allow for certain relations often shown
        in traditional matrix calculus, such as
<pre>
        A = B
</pre>
        since the two operators might have in fact two distinct signatures.
        In matrix calculus one only compares tables of unnamed numbers,
        while in our Haskell formalism we compare typed
        entieties.
        For this reason, we will be threading quite
        slowly here, from one definition to another to assure that
        they are correct from the perspective of
        typing rules of Haskell.

<p>
<hr>
<p>
<b>
        6.1. Operator notation
</b>
<p>
        The notation
<pre>
        | y > = A | x >
</pre>
        is pretty obvious: operator A acting on vector | x &gt; produces
        vector | y &gt;. It is not obvious though whether both vectors
        use the same representation. The Haskell version of the above
        clarifies this point, as in this example:
<pre>
        y = a >< x
           where
                a :: Ket Int -> Ket (Int, Int)
                a = ......
</pre>
        In this case it is seen the two vectors have distinct
        representations. The operator &gt;&lt; will be explained soon
        but for now treat is as an application of an operator
        to a vector, or some kind of a product of the two.
<p>
        The above can be also written as
<pre>
        | y > = | A x >
</pre>
        where the right hand side is just a defining label saying that the
        resulting vector has been produced by operator A acting on | x &gt;.
<p>
        Linear operators can also act on the bra vectors
<pre>
        < y | = < x | A
                <---
</pre>
        providing that they have correct signatures. This postfix notation
        though is a bit awkward, and not supported by Haskell. To avoid
        confusion we will be using the following notation instead:
<pre>
        < y | = < A x |
</pre>
        which says that bra y is obtained from ket y,
        where | y &gt; = | A x &gt;,  as before. In Haskell we will write
        it as
<pre>
        y = toBra $ a >< x

</pre>

<p>
<hr>
<p>
<b>
        6.2. Renaming the representation
</b>
<p>
        One simple example of an operator is <em>label "new"</em>
        which renames a vector representation by adding extra label
        <em>"new"</em> in the basis vectors <em>Ket a</em>. Silly
        as it sounds, this and other similar re-labeling operations
        can be actually quite useful; for example,
        we might wish to distinguish between old and new bases, or
        just to satisfy the Haskell typechecker.
<pre>

        label :: (Ord a, Ord b) => b -> Ket a -> Ket (b, a)
        label i (Ket a) = Ket (i, a)
        label i x       = (label i) >< x

</pre>
<p>
<hr>
<p>
<b>
        6.3. Closure formula, or identity operator
</b>
<p>
        Although the general Dirac formalism often refers to
        abstract vectors | x &gt;, our implementation must
        be more concrete than that -- we always represent the
        abstract vectors in some basis of our choice, as in:
<pre>
        | x > = c<sub>k</sub> | k >   (sum over k)
</pre>
        To recover the component c<sub>k</sub> we form
        the inner product
<pre>
            c<sub>k</sub> = < k | x >
</pre>
        Putting it back to the previous equation:
<pre>
        | x > = < k | x > | k >      (sum over k)
              = | k > < k | x >
              = Id | x >
        where
            Id = | k > < k |        (sum over k)
</pre>
        we can see that the vector | x &gt; has been abstracted away. The formula
        says that vector | x &gt; can be decomposed in any basis
        by applying identity operator Id to it. This is also known
        as a closure formula. Well, Haskell has the "id" function too,
        and we could apply it to any ket, as in:
<pre>
        id (Ket 1 +> 10 |> Ket 2) ==> | 1 > + 10 | 2 >
</pre>
        but Haskell's "id" does not know anything about representations;
        it just gives us back the same vector | x &gt; in our original
        representation.
<p>
        We need something more accurately depicting the closure
        formula | k &gt; &lt; k |, that would allow us to change
        the representation if we wanted to, or leave it alone
        otherwise. Here is the <em>closure</em> function and
        coresponding operator (&gt;&lt;) that implement
        the closure formula for a given <em>operator</em>.
<pre>

> closure :: (DiracVector a, DiracVector b) => (a -> b) -> a -> b
> closure operator x =
>    compose' (components x) (map operator (basis x))
>      where
>         compose' xs ks = foldl1 add (zipWith scale xs ks)

> (><) :: (DiracVector b, DiracVector a) => (a -> b) -> a -> b
> operator >< x = closure operator x


</pre>
<p>
<hr>
<p>
<b>
        6.4. Changing the representation
</b>
<p>
        The silly <em>label</em> function found in the comment of the
        section 6.1 uses in fact the closure relation. But we could
        define is simpler than that:
<pre>

> label :: t -> Ket t1 -> Ket (t, t1)
> label i (Ket x) = Ket (i, x)

</pre>
        and then apply a closure to a vector x, as in:
<pre>
        closure (label 0) (Ket 2 +> 7 |> Ket 3)
                ==> 1.0 |(0,2)> + 7.0 |(0,3)>
</pre>
        Somewhat more realistic example involves "rotation" of
        the old basis with simulaneous base renaming:
<pre>

> rot :: Ket Int -> Ket (Int, Int)
> rot (Ket 1) = normalize $ Ket (1,1) +> Ket (1,2)
> rot (Ket 2) = normalize $ Ket (1,1) +> (-1) |> Ket (1,2)
> rot (Ket _) = error "exceeded space dimension"

</pre>
        The example function
        <em>rot</em> assumes transformation from
        two-dimensional basis [| 1 &gt;, | 2 &gt;] to another
        two-dimensional basis [| (1,1) &gt;, | (1,2) &gt;] by
        expressing the old basis by the new one. Given this
        transformation we can apply the closure to any vector | x &gt;
        represented in the old basis; as a result we will get
        the same vector | x &gt; but represented in the new
        basis.
<pre>
        rot >< (Ket 1 +> 7 |> Ket 2) ==>
                5.65685 |(1,1)> + -4.24264 |(1,2)>
</pre>

<p>
<hr>
<p>
<b>
        6.5. Implementation of the operator equation A | x &gt; = | y &gt;
</b>
<p>
        The Haskell implementation of the closure formula is not just
        a useless simulation of the theoretical closure  - it is one of the
        workhorses of the apparatus employed here.
<p>
        We will be using linear operators to evaluate equations
        like this:
<pre>
        | y > = A | x >
</pre>
        The resulting vector | y &gt; can have either the same
        representation as | x &gt; or different - depending on
        the nature of operator A. The most general type of
        A is
<pre>
        Ket a -> Ket b
</pre>
        but more often than not the basis will be the same as before.
        But how we define the operator A itself? The best way is
        to specify how it acts on the base vectors | k &gt;. If we can chose
        as our basis the eigenvectors of A this would be even better,
        because the definition of A would be then extremely simple.
        After inserting the identity | k &gt;&lt; k | between the
        operator A and vector | x &gt; in the above equation one gets
<pre>
        | y > = A | k > < k | x >            (sum over k)
</pre>
        This will be implemented in Haskell as:
<pre>
        y = a >< x
</pre>
        The closure formula will take care of the rest and it will
        produce the result | y &gt; . The examples previously given
        do just that. One caveat though: since operator A will
        only be defined for the basis, but not for other vectors,
        skipping the closure formula and coding directly
<pre>
        y = a' x
</pre>
        is not advisable.
        This will certainly fail for vectors other than basis unless
        one makes extra provisions for that. This is what we did
        in module Momenta, before we had the closure support ready.
        Using the closure is safe and this is the way to go!


<p>
<hr>
<p>
<b>
        6.6. Inverse operator
</b>
<p>
        An operator B = A<sup>-1</sup> that inverses the
        equation
<pre>
        | y > = A | x >
          y   = a >< x -- where a :: Ket a -> Ket b
</pre>
        into
<pre>
        | x > = B | y >
          x   = b >< y -- where b :: Ket b -> Ket a
</pre>
        is called the inverse operator.
<p>
        For example, the inverse operator to the operator <em>label i</em>
        is:
<pre>

> label' :: (Ord a, Ord b) => Ket (a, b) -> Ket b
> label' (Ket (_, x)) = Ket x

</pre>
        It is easy to check that applying the operator A and its inverse
        A<sup>-1</sup> in succession to any ket | x &gt; one should
        obtain the same vector | x &gt; again, as in:

<pre>
        A<sup>-1</sup> A | x > = | x >

        -- Haskell example
        label' >< (label 0 >< x) == x
           where
                x = Ket 1 +> 10 |> Ket 7
        ==> True
</pre>
        Once again, notice the omnipresent closure operator in Haskell
        implementation. Tempting as it might be to implement the
        above example as
<pre>
        -- Do not do it in Haskell!!!
        (label' . label 0) >< x == x
            where
               x = Ket 1 +> 10 |> Ket 7
        ==> True
</pre>
        this is not a recommended way. Although this example would work,
        but a similar example for <em>rotation</em> operations would
        fail in a spectacular way. The correct way is to insert the
        closure operator between two rotations:
<pre>
        rot' >< (rot >< x) == x
            where
                x = Ket 1 +> 10 |> Ket 2
        ==> True
</pre>
        where the inverse operator <em>rot'</em> is defined below:

<pre>

> rot' :: Ket (Int, Int) -> Ket (Int)
> rot' (Ket (1,1)) = normalize $ Ket 1 +> Ket 2
> rot' (Ket (1,2)) = normalize $ Ket 1 +> (-1) |> Ket 2
> rot' (Ket (_,_)) = error "exceeded space dimension"

</pre>
<p>
<hr>
<p>
<b>
        6.7. Matrix representation of an operator
</b>
<p>
<p>
        The scalar products
<pre>
        < k | A l' > = < k | A | l' >
</pre>
        such that | k &gt; and | l' &gt; are the base vectors
        (in general belonging to two different bases), form a transformation
        matrix Akl'.
<p>
        In Haskell this matrix is formed as
<pre>
        k <> a >< l'
            where
               k  = ... :: Bra b
               l' = ... :: Ket a
               a  = ... :: Ket a -> Ket b
</pre>

<p>
<hr>
<p>
<b>
        6.8. Adjoint operator
</b>
<font color="teal">
<p>
        Our definition of adjoint operator is different
        than that in theory of determinants. Many books, not necessarily
        quantum mechanical oriented, refer to the latter as <em>
        classical adjoint operator</em>.
</font>

<p>
        With every linear operator A we can associate an adjoint
        operator B = A<sup>+</sup>, also known as Hermitian conjugate
        operator, such that equality of the two scalar
        products
<pre>
        < A<sup>+</sup> u | x > = < u | A x >
</pre>
        holds for every vector | u &gt; and | x &gt;.
        In Haskell notation the above can be written as:
<pre>
        (toBra (b >< u) <> x) == toBra u <> a >< x
            where
                 a = ... :: Ket a -> Ket b
                 b = ... :: Ket b -> Ket a
                 x = ... :: Ket a
                 u = ... :: Ket b

</pre>
        For example, the operator <em>rot'</em> is adjoint
        to operator <em>rot</em>
<pre>
        (toBra (rot' >< u) <> x) == (toBra u <> rot >< x)
            where
                x = Ket 1 +> 10 |> Ket 2
                u = Ket (1,1) +> 4 |> Ket (1,2)
        ==> True

</pre>
        It can be shown that
<pre>
        (A<sup>+</sup>)<sup>+</sup> = A
</pre>
        Matrix A<sup>+</sup> is conjugate transposed to A, as
        proven below

<pre>
        = A<sup>+</sup>kl'
        = < k | A<sup>+</sup> | l' >
        = < k | A<sup>+</sup> l' >
        = < A<sup>+</sup> l' | k ><sup>*</sup>
        = < l' | A | k ><sup>*</sup>
        = A<sup>*</sup>l'k
</pre>


<p>
<hr>
<p>
<b>
        6.9. Unitary operator
</b>
<p>
        Unitary transformations preserve norms of vectors.
        We say, that the norm of a vector is invariant under unitary
        transformation.
        Operators describing such transformations are called
        unitary operators.
<pre>
        < A x | A x > = < x | x >

</pre>
        The example of this is rotation transformation, which indeed
        preserves the norm of any vector x, as shown in this Haskell
        example
<pre>
        (toBra u <> u) == (toBra x <> x)
            where
                u = rot >< x
                x = Ket 1 +> 10 |> Ket 2

        ==> True
</pre>
<p>
        Inverse and adjoint operators of unitary operators are equal
<pre>
        A<sup>-1</sup> = A<sup>+</sup>
</pre>
        which indeed is true for our example operator <em>rot</em>.
<p>
        Computation of the adjont operators A<sup>+</sup> from A
        is quite easy since the process is rather mechanical, as
        described in the previous section. On the other hand, finding
        inverse operators is not that easy, with the exception of
        some simple cases, such as our example 2D rotation.
        It is therefore important to know whether a given operator
        is unitary, as this would allow us to replace inverse
        operators by adjoint operators.


<p>
<hr>
<p>
<b>
        6.10. Hermitian operator
</b>
<p>
        A Hermitian operator is a self adjoint operator; that is
<pre>
        < A u | x > = < u | A x >
</pre>
        Another words: A<sup>+</sup> = A.
<p>
        Notice however, that this relation holds only for the
        vectors in the same representation, since in general
        the operators
        A and A<sup>+</sup> have distinct signatures, unless
        types a, b are the same:
<pre>
        a  :: Ket a -> Ket b -- operator A
        a' :: Ket b -> Ket a -- operator A<sup>+</sup>
</pre>
        Elements of hermitian matrices must therefore satisfy:
<pre>
         Aij = (Aji)<sup>*</sup>
</pre>
        In particular, their diagonal elements must be real.
<p>
        Our example operator <em>rot</em> is not hermitian,
        since it describes transformation from one basis
        to another.
        But here is a simple example of a hermitian operator, which
        multiplies any ket by scalar 4. It satisfies our definition:
<pre>
        (toBra (a >< u) <> x) == (toBra u <> a >< x)
        where
            a v = 4 |> v

            x = Ket 1 +> Ket 2
            u = Ket 2

        ==> True
</pre>
        Here is a short quote from [3].
<blockquote>
        Why do we care whether an operator is Hermitian?
        It's because of a few theorems:

<ol>
<li>
        The eigenvalues of Hermitian operators are always real.
<li>
        The expectation values of Hermitian operators are always real.
<li>
        The eigenvectors of Hermitian operators span the Hilbert space.
<li>
        The eigenvectors of Hermitian operators belonging to distinct eigenvalues are orthogonal.
</ol>
        In quantum mechanics, these characteristics are essential if you
        want to represent measurements with operators. Operators must be
        Hermitian so that observables are real. And, you must be able to
        expand in the eigenfunctions - the expansion coefficients
        give you probabilities!
</blockquote>
<p>
<hr>
<p>
<b>
        7. Showing kets and bras
</b>
<p>
        Lastly, here are show functions for pretty printing of Dirac
        vectors.
<pre>

> instance (Show a, Eq a, Ord a) => Show (Ket a)  where
>     showsPrec _ KetZero   = showString "| Zero >"
>     showsPrec n (Ket j)   = showString "|" . showsPrec n j . showString ">"
>     showsPrec n (x :|> k) = showsScalar n x . showsPrec n k
>     showsPrec n (j :+> k) = showsPrec n j . showString " + " . showsPrec n k

> instance (Show a, Eq a, Ord a) => Show (Bra a)  where
>     showsPrec _ BraZero   = showString "< Zero |"
>     showsPrec n (Bra j)   = showString "<" . showsPrec n j . showString "|"
>     showsPrec n (x :<| k) = showsScalar n x . showsPrec n k
>     showsPrec n (j :<+ k) = showsPrec n j . showString " + " . showsPrec n k


> showsScalar :: (Show t, RealFloat t) => Int -> Complex t -> String -> String
> showsScalar n x@(a :+ b)
>     | b == 0    = showsPrec n a . showString " "
>     | otherwise = showString "(" .showsPrec n x . showString ") "

</pre>
<p>
<hr>
<p>
<b>
        8. Data Tuple for tensor products
</b>
<p>
        A state vector of several subsystems is modelled as a ket parametrized
        by a type variable Tuple, which is similar to ordinary () but is
        shown differently. Tensor product of several simple states leads
        to deeply entangled structure, with many parenthesis obstructing
        readability. What we really want is a simple notation for easy
        visualization of products of several states, as in:
<pre>
        Ket 1 *> Ket (2, 1) * Ket '+' ==> | 1; (2,1); '+' >
</pre>
        See module Momenta for practical example of tensor products
        of vector spaces.
<pre>

> data Tuple a b =  a :* b
>     deriving (Eq, Ord)

> instance (Show a, Show b) => Show (Tuple a b) where
>     showsPrec n (a :* b) = showsPrec n a . showString "; " . showsPrec n b

</pre>
<p>
<hr>
<p>
<b>
        9. References
</b>
<p>
<ul>
<p>
<li>

        [1] Jerzy Karczmarczuk, Scientific computation and functional
        programming, Dept. of Computer Science, University of Caen, France,
        Jan 20, 1999, <a href="http://www.info.unicaen.fr/~karczma/">
        http://www.info.unicaen.fr/~karczma/</a>
<p>
<li>
        [2] Jan Skibinski, Collection of Haskell modules,
        Numeric Quest Inc., <a href="http://www.numeric-quest.com/haskell/">
        http://www.numeric-quest.com/haskell/"</a>
<p>
<li>
        [3] Steven Pollock, University of Colorado,
        <a href="http://www.colorado.edu/physics/phys3220/3220_fa97/notes/notes_table.html">
        Quantum Mechanics, Physics 3220 Fall 97, lecture notes</a>

</ul>
<p>
<hr>
<p>
<b>
        10. Copyright and license
</b>

<pre>
--
-- Copyright:
--
--      (C) 2000 Numeric Quest, All rights reserved
--
--      Email: jans@numeric-quest.com
--
--      http://www.numeric-quest.com
--
-- License:
--
--      GNU General Public License, GPL
--

</pre>
</blockquote>
</body>

<SCRIPT language="Javascript">
<!--

// FILE ARCHIVED ON 20010421035521 AND RETRIEVED FROM THE
// INTERNET ARCHIVE ON 20030715011358.
// JAVASCRIPT APPENDED BY WAYBACK MACHINE, COPYRIGHT INTERNET ARCHIVE.
// ALL OTHER CONTENT MAY ALSO BE PROTECTED BY COPYRIGHT (17 U.S.C.
// SECTION 108(a)(3)).

   var sWayBackCGI = "http://web.archive.org/web/20010421035521/";

   function xLateUrl(aCollection, sProp) {
      var i = 0;
      for(i = 0; i < aCollection.length; i++)
         if (aCollection[i][sProp].indexOf("mailto:") == -1 &&
             aCollection[i][sProp].indexOf("javascript:") == -1)
            aCollection[i][sProp] = sWayBackCGI + aCollection[i][sProp];
   }

   if (document.links)  xLateUrl(document.links, "href");
   if (document.images) xLateUrl(document.images, "src");
   if (document.embeds) xLateUrl(document.embeds, "src");

   if (document.body && document.body.background)
      document.body.background = sWayBackCGI + document.body.background;

//-->

</SCRIPT>
</html>
