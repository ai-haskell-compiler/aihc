<html>
<head>
<BASE HREF="http://www.numeric-quest.com/haskell/Orthogonals.html">

<title>
        Indexless linear algebra algorithms
</title>
</head>
<body>
<ul>
<center>
<h1>
                        ***
</h1>
<h1>
        Indexless linear algebra algorithms
</h1>
<b>
<br>
        Orthogonalization, linear equations, eigenvalues and eigenvectors
<br>
        Literate Haskell module <i>Orthogonals.lhs</i>
</b>
<p>
        Jan Skibinski, <a href="http://www.numeric-quest.com/news/">
        Numeric Quest Inc.</a>, Huntsville, Ontario, Canada
<p>
        1998.09.19, last modified 1998.12.28
</center>
<hr>
<p>
        It has been argued that the functional paradigm offers more
        support for scientific computing than the traditional imperative
        programming, such as greater similarity of functional implementation
        to mathematical specification of a problem. However, efficiency
        of scientific algorithms implemented in Haskell is very low compared
        to efficiencies of C or Fortran implementations - notwithstanding
        the exceptional descriptive power of Haskell.
<dd>
        It has been also argued that tradition and inertia are partially
        responsible for this sore state and that many functional algorithms
        are direct translations of their imperative counterparts.
<dd>
        Arrays - with their indexing schemes and destructive updating
        are basic tools of imperative programming. But pure functional
        languages, which prohibit variable reassignments, cannot compete
        with imperative languages by using the same tools and following
        similar reasoning and patterns - unless the functional arrays
        themselves are designed with performance in mind. This is
        a case with Clean, where efficiency of one kind of their arrays
        -- strict unboxed array, approaches efficiency of C.
<dd>
        But this has not been done for Haskell arrays yet. They are
        lazy, boxed and use auxiliary association lists (index, value)
        for initialization -- the latter being mostly responsible for
        low efficiency of those algorithms that create many interim
        arrays.
<dd>
        It appears, that -- as long as indexing scheme is not used
        for lookups and updates -- Haskell lists are more efficient
        than arrays -- at least at the currents state of Haskell.
<p>
        With this in mind, we are attempting to demonstrate here
        that the indexing traps can be successfully avoided.
        This module implements afresh several typical problems from linear
        algebra. Standard Haskell lists are employed instead of arrays
        and not a single algorithm ever uses indices for lookups
        or updates.
<dd>
        We do not claim high efficiency of these algorithms; consider
        them exploratory. However, we do claim that the clarity of
        these algorithms is significantly better than of those functionally
        similar algorithms that employ indexing schemes.
<p>
        Two major algorithms have been invented and implemented in Haskell:
        one for solving systems of linear equations and one for finding
        eigenvalues and eigenvectors of almost any type of a square matrix.
        This includes symmetric, hermitian, general complex or nonsymmetric
        matrices with real eigenvalues.
<dd>
        Amazingly, both methods are based on the same factorization, akin
        to QR method, but not exactly the same as the standard QR one.
        A simple trick allows to extend this method to nonsymmetric real
        matrices with complex eigenvalues and thus one method applies to
        all types of matrices.
        It follows that the eigenvalue/eigenvector problem can be consistently
        treated all across the board. In addition, no administrative
        (housekeeping) boring trivia is required here and that helps to
        clearly explain the mechanisms employed.

</i>
<p>
<hr>
<p>
<b>
        Contents
</b>
<p>
<ul>
<p><li>
        Notation
<p><li>
        Scalar products and vector normalization
<ul>
<li><b>
            bra_ket</b>, scalar product
<li><b>
            sum_product</b>, a cousin of bra_ket
<li><b>
            norm</b>, vector norm
<li><b>
            normalized</b>, vector normalized to one
</ul>
<p><li>
        Transposition and adjoining of matrices
<ul>
<li><b>
            transposed</b>, transposed matrix
<li><b>
            adjoint</b>, transposed and conjugated matrix
</ul>
<p><li>
        Products involving matrices
<ul>
<li><b>
            matrix_matrix</b>, product of two matrices as list of rows
<li><b>
            matrix_matrix'</b>, product of two matrices as list of columns
<li><b>
            triangle_matrix'</b>, upper triangular matrix times square matrix
<li><b>
            matrix_ket</b>, matrix times ket vector
<li><b>
            bra_matrix</b>, bra vector times matrix
<li><b>
            bra_matrix_ket</b>, matrix multiplied on both sides by vectors
<li><b>
            scalar_matrix</b>, scalar times matrix
</ul>
<p><li>
        Orthogonalization process
<ul>
<li><b>
            orthogonals</b>, set of orthogonal vectors
<li><b>
            gram_schmidt</b>, vector perpendicular to a hyperplane
</ul>

<p><li>
        Solutions of linear equations by orthogonalization
<ul>
<li><b>
            one_ket_triangle</b>, triangularization of one vector equation
<li><b>
            one_ket_solution</b>, solution for one unknown vector
<li><b>
            many_kets_triangle</b>, triangularization of several vector equations
<li><b>
            many_kets_solution</b>, solution for several unknown vectors
</ul>
<p><li>
        Matrix inversion
<ul>
<li><b>
            inverse</b>, inverse of a matrix
</ul>
<p><li>
        QR factorization of matrices provided by "many_kets_triangle"
<ul>
<li><b>
            factors_QR</b>, QR alike factorization of matrices
<li><b>
            determinant</b>, computation of the determinant based on the QR factorization
</ul>
<p><li>
        Similarity transformations and eigenvalues
<ul>
<li><b>
            similar_to</b>, matrix obtained by similarity transformation
<li><b>
            iterated_eigenvalues</b>, list of approximations of eigenvalues
<li><b>
            eigenvalues</b>, final approximation of eigenvalues
</ul>
<p><li>
        Preconditioning of real nonsymmetric matrices
<ul>
<li><b>
            add_to_diagonal</b>, simple preconditioning method
</ul>
<p><li>
        Examples of iterated eigenvalues
<ul>
<li>
            Symmetric real matrix
<li>
            Hermitian complex matrix
<li>
            General complex matrix
<li>
            Nonsymmetric real matrix with real eigenvalues
<li>
            Nonsymmetric real matrix with complex eigenvalues
</ul>
<p><li>
        Eigenvectors for distinct eigenvalues
<ul>
<li><b>
                eigenkets</b>, eigenvectors for distinct eigenvalues
</ul>
<p><li>
        Eigenvectors for degenerated eigenvalues
<ul>
<li><b>
                eigenket'</b>, eigenvector based on a trial vector
</ul>

<p><li>
        Auxiliary functions
<ul>
<li><b>
        unit_matrix</b>, a unit matrix with 1's on a diagonal
<li><b>
        unit_vector</b>, a vector with one non-zero component
<li><b>
        diagonals</b>, vector made of a matrix diagonal
</ul>
</ul>

<p>
<hr>
<p>
<b>
        Notation
</b>
<p>
        What follows is written in Dirac's notation, as used
        in Quantum Mechanics. Matrices are represented by capital
        letters, while vectors come in two varieties:
<ul>
<p><li>
        Bra vector x, written &lt; x |, is represented by one-row matrix
<p><li> Ket vector y, written | y &gt;, is represented by one-column matrix
</ul>
<p>
        Bra vectors can be obtained from ket vectors by transposition
        and conjugation of their components. Conjugation is only
        important for complex vectors.
<p>
        Scalar product of two vectors | x &gt; and | y &gt; is written
        as
<pre>
        < x | y >
</pre>
        which looks like a bracket and is sometimes called a "bra_ket".
        This justifies "bra" and "ket" names introduced by Dirac. There
        is a good reason for conjugating the components of "bra-vector":
        the scalar product of
<pre>
        < x | x >
</pre>
        should be a square of the norm of the vector "x", and that
        means that it should be represented by a real number, or complex
        number but with its imaginary part equal to zero.
<p>
<hr>
<p>
<pre>

> module Orthogonals where
> import Data.Complex
> import Data.Ratio
> import qualified Data.List as List

</pre>
<b>
        Scalar product and vector normalization
</b>
<p>
        The scalar product "bra_ket" is a basis of many algorithms
        presented here.


<pre>

> bra_ket :: (Scalar a, Num a) => [a] -> [a] -> a
> bra_ket u v =
>       --
>       -- Scalar product of two vectors u and v,
>       -- or < u | v > in Dirac's notation.
>       -- This is equally valid for both: real and complex vectors.
>       --
>       sum_product u (map coupled v)

</pre>

        Notice the call to function "coupled" in the above implementation
        of scalar product. This function conjugates its argument
        if it is complex, otherwise does not change it. It is defined
        in the class Scalar - specifically designed for this purpose
        mainly.
<dd>
        This class also defines a norm of a vector that might be used
        by some algorithms. So far we have been able to avoid this.
<pre>

> class Eq a => Scalar a where
>     coupled    :: a->a
>     norm       :: [a] -> a
>     almostZero :: a -> Bool
>     scaled     :: [a] -> [a]

> instance Scalar Double where
>     coupled x    = x
>     norm u       = sqrt (bra_ket u u)
>     almostZero x = (abs x) < 1.0e-8
>     scaled       = scaled'

> instance Scalar Float where
>     coupled x    = x
>     norm u       = sqrt (bra_ket u u)
>     almostZero x = (abs x) < 1.0e-8
>     scaled       = scaled'

> instance (Integral a) => Scalar (Ratio a) where
>     coupled x    = x
>     -- norm u    = fromDouble ((sqrt (bra_ket u u))::Double)
>     -- Intended hack to silently convert to and from Double.
>     -- But I do not know how to declare it properly.
>     --
>     -- Our type Fraction, when used instead of Ratio a, has its own
>     -- definition of sqrt. No hack would be needed here.
>     almostZero x = abs x < 1e-8
>     scaled       = scaled'

> instance (RealFloat a) => Scalar (Complex a) where
>     coupled (x:+y) = x:+(-y)
>     norm u         = sqrt (realPart (bra_ket u u)) :+ 0
>     almostZero z   = (realPart (abs z)) < 1.0e-8
>     scaled u       = [(x/m):+(y/m) | x:+y <- u]
>        where m = maximum [max (abs x) (abs y) | x:+y <- u]

> norm1 :: (Num a) => [a] -> a
> norm1 = sum . map abs

> norminf :: (Num a, Ord a) => [a] -> a
> norminf = maximum . map abs

> matnorm1 :: (Num a, Ord a) => [[a]] -> a
> matnorm1 = matnorminf . transposed

> matnorminf :: (Num a, Ord a) => [[a]] -> a
> matnorminf = maximum . map norm1


</pre>

        But we also need a slightly different definition of
        scalar product that will appear in multiplication of matrices
        by vectors (or vice versa): a straightforward accumulated product
        of two lists, where no complex conjugation takes place.
        We will call it a 'sum_product".
<pre>

> sum_product :: Num a => [a] -> [a] -> a
> sum_product u v =
>       --
>       -- Similar to scalar product but without
>       -- conjugations of | u > components
>       -- Used in matrix-vector or vector-matrix products
>       --
>       sum (zipWith (*) u v)

</pre>
        Some algorithms might need vectors normalized to one, although
        we'll try to avoid the normalizations due to its high cost
        or its inapplicability to rational numbers. Instead, we wiil
        scale vectors by their maximal components.
<pre>

> normalized :: (Scalar a, Fractional a) => [a] -> [a]
> normalized u =
>       map (/norm u) u

> scaled' :: (Fractional t, Ord t) => [t] -> [t]
> scaled' u =
>       map (/norminf u) u

</pre>
<hr>
<p>
<b>
        Transposition and adjoining of matrices
</b>
<p>
        Matrices are represented here by lists of lists.
        Function "transposed" converts from row-wise to column-wise
        representation, or vice versa.
<dd>
        When transposition is combined with complex conjugation
        the resulting matrix is called "adjoint".
<p>
        A square matrix is called symmetric if it is equal to its transpose
<pre>
        A = A<sup>T</sup>
</pre>
        It is called Hermitian, or self-adjoint, if it equals to
        its adjoint
<pre>
        A = A<sup>+</sup>

> transposed :: [[a]] -> [[a]]
> transposed a
>     | null (head a) = []
>     | otherwise = ([head mi| mi <- a])
>                   :transposed ([tail mi| mi <- a])

> adjoint :: Scalar a => [[a]] -> [[a]]
> adjoint a
>     | null (head a) = []
>     | otherwise = ([coupled (head mi)| mi <- a])
>                   :adjoint ([tail mi| mi <- a])

</pre>
<p>
<hr>
<p>
<b>
        Linear combination and sum of two matrices
</b>
<p>
        One can form a linear combination of two matrices, such
        as
<pre>
        C = alpha A + beta B
        where
            alpha and beta are scalars
</pre>
        The most generic form of any combination, not necessary
        linear, of components of two matrices is given by "matrix_zipWith"
        function below, which accepts a function "f" describing such
        combination. For the linear combination with two scalars
        the function "f" could be defined as:
<pre>
        f alpha beta a b = alpha*a + beta*b
</pre>
        For a straightforward addition of two matrices this auxiliary
        function is simply "(+)".
<pre>

> matrix_zipWith :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
> matrix_zipWith f a b =
>     --
>     -- Matrix made of a combination
>     -- of matrices a and b - as specified by f
>     --
>     [zipWith f ak bk | (ak,bk) <- zip a b]

> add_matrices :: (Num a) => t -> t1 -> [[a]] -> [[a]] -> [[a]]
> add_matrices _ _ = matrix_zipWith (+)

</pre>

<p>
<hr>
<p>
<b>
        Products involving matrices
</b>
<p>
        Variety of products involving matrices can be defined.
        Our Haskell implementation is based on lists of lists
        and therefore is open to interpretation: sublists
        can either represent the rows or the columns of a matrix.
<dd>
        The following definitions are somehow arbitrary, since
        one can choose alternative interpretations of lists
        representing matrices.
<p>
<b>
        C = A B
</b>
<p>
        Inner product of two matrices A B can be expressed quite simply,
        providing that matrix A is represented by a list of rows
        and B - by a list of columns. Function "matrix_matrix"
        answers list of rows, while "matrix_matrix'" - list
        of columns.
<dd>
        Major algorithms of this module make use of "triangle_matrix'",
        which calculates a product of upper triangular matrix
        with square matrix and returns a rectangular list of columns.

<pre>

> matrix_matrix :: Num a => [[a]] -> [[a]] -> [[a]]
> matrix_matrix a b
> --
> -- A matrix being an inner product
> -- of matrices A and B, where
> --     A is represented by a list of rows a
> --     B is represented by a list of columns b
> --     result is represented by list of rows
> -- Require: length of a is equal of length of b
> -- Require: all sublists are of equal length
>
>       | null a = []
>       | otherwise = ([sum_product (head a) bi | bi <- b])
>                  : matrix_matrix (tail a) b

> matrix_matrix' :: (Num a) => [[a]] -> [[a]] -> [[a]]
> matrix_matrix' a b
>       --
>       -- Similar to "matrix_matrix"
>       -- but the result is represented by
>       -- a list of columns
>       --
>       | null b = []
>       | otherwise = ([sum_product ai (head b) | ai <- a])
>                    : matrix_matrix' a (tail b)


> triangle_matrix' :: Num a => [[a]] -> [[a]] -> [[a]]
> triangle_matrix' r q =
>       --
>       -- List of columns of of a product of
>       -- upper triangular matrix R and square
>       -- matrix Q
>       -- where
>       --     r is a list of rows of R
>       --     q is a list of columns of A
>       --
>       [f r qk | qk <- q]
>       where
>           f t u
>               | null t = []
>               | otherwise = (sum_product (head t) u)
>                             : (f (tail t) (tail u))



</pre>
<b>
        | u &gt; = A | v &gt;
</b>
<p>
        Product of a matrix and a ket-vector is another
        ket vector. The following implementation assumes
        that list "a" represents rows of matrix A.
<pre>

> matrix_ket :: Num a => [[a]] -> [a] -> [a]
> matrix_ket a v = [sum_product ai v| ai <- a]

</pre>
<b>
        &lt; u | = &lt; v | A
</b>
<p>
        Bra-vector multiplied by a matrix produces
        another bra-vector. The implementation below
        assumes that list "a" represents columns
        of matrix A. It is also assumed that vector
        "v" is given in its standard "ket" representation,
        therefore the definition below uses "bra_ket"
        instead of "sum_product".
<pre>

> bra_matrix :: (Scalar a, Num a) => [a] -> [[a]] -> [a]
> bra_matrix v a = [bra_ket v ai | ai <- a]

</pre>
<b>
        alpha = &lt; u | A | v &gt;
</b>
<p>
        This kind of product results in a scalar and is often
        used to define elements of a new matrix, such as
<pre>
        B[i,j] = < ei | A | ej >
</pre>
        The implementation below assumes that list "a" represents
        rows of matrix A.
<pre>

> bra_matrix_ket :: (Scalar a, Num a) => [a] -> [[a]] -> [a] -> a
> bra_matrix_ket u a v =
>     bra_ket u (matrix_ket a v)

</pre>
<b>
        B = alpha A
</b>
<p>
        Below is a function which multiplies matrix by a scalar:
<pre>

> scalar_matrix :: Num a => a -> [[a]] -> [[a]]
> scalar_matrix alpha a =
>       [[alpha*aij| aij <- ai] | ai<-a]

</pre>
<p>
<hr>
<p>
<b>
        Orthogonalization process
</b>
<p>

        Gram-Schmidt orthogonalization procedure is used here
        for calculation of sets of mutually orthogonal vectors.
<dd>
        Function "orthogonals" computes a set of mutually orthogonal
        vectors - all orthogonal to a given vector. Such set plus
        the input vector form a basis of the vector space. Another
        words, they are the base vectors, although we cannot call them
        unit vectors since we do not normalize them for two reasons:
<ul>
<li>
        None of the algorithms presented here needs this -- quite
        costly -- normalization.
<li>
        Some algorithms can be used either with doubles or with
        rationals. The neat output of the latter is sometimes desirable
        for pedagogical or accuracy reasons. But normalization requires "sqrt"
        function, which is not defined for rational numbers. We could
        use our module Fraction instead, where "sqrt" is defined,
        but we'll leave it for a future revision of this module.
</ul>
<p>
        Function "gram_schmidt" computes one vector - orthogonal
        to an incomplete set of orthogonal vectors, which form a hyperplane
        in the vector space. Another words, "gram_schmidt" vector is
        perpendicular to such a hyperlane.


<pre>

> orthogonals :: (Scalar a, Fractional a) => [a] -> [[a]]
> orthogonals x =
>       --
>       -- List of (n-1) linearly independent vectors,
>       -- (mutually orthogonal) and orthogonal to the
>       -- vector x, but not normalized,
>       -- where
>       --     n is a length of x.
>       --
>       orth [x] size (next (-1))
>       where
>           orth a n m
>               | n == 1        = drop 1 (reverse a)
>               | otherwise     = orth ((gram_schmidt a u ):a) (n-1) (next m)
>               where
>                   u = unit_vector m size
>           size = length x
>           next i = if (i+1) == k then (i+2) else (i+1)
>           k = length (takeWhile (== 0) x) -- first non-zero component of x

> gram_schmidt :: (Scalar a, Fractional a) => [[a]] -> [a] -> [a]
> gram_schmidt a u =
>       --
>       -- Projection of vector | u > on some direction
>       -- orthogonal to the hyperplane spanned by the list 'a'
>       -- of mutually orthogonal (linearly independent)
>       -- vectors.
>       --
>       gram_schmidt' a u u
>       where
>           gram_schmidt' [] _ w = w
>           gram_schmidt' (b:bs) v w
>               | all (== 0) b = gram_schmidt' bs v w
>               | otherwise    = gram_schmidt' bs v w'
>               where
>                   w' = vectorCombination w (-(bra_ket b v)/(bra_ket b b)) b
>           vectorCombination x c y
>               | null x = []
>               | null y = []
>               | otherwise = (head x + c * (head y))
>                             : (vectorCombination (tail x) c (tail y))

</pre>
<p>
<hr>
<p>
<b>
        Solutions of linear equations by orthogonalization
</b>
<p>
        A matrix equation for unknown vector | x &gt;
<pre>
        A | x > = | b >
</pre>
        can be rewritten as
<pre>
        x1 | 1 > + x2 | 2 > + x3 | 3 > + ... + xn | n > = | b >     (7.1)
        where
                | 1 >, | 2 >... represent columns of the matrix A
</pre>
        For any n-dimensional vector, such as "1", there exist
        n-1 linearly independent vectors "ck" that are orthogonal to "1";
        that is, each satisfies the relation:
<pre>
        < ck | 1 > = 0, for k = 1...m, where m = n - 1
</pre>
        If we could find all such vectors, then we could multiply
        the equation (7.1) by each of them, and end up with m = n-1
        following equations
<pre>
        < c1 | 2 > x2 + < c1 | 3 > x3 + ... < c1 | n > xn = < c1 | b >
        < c2 | 2 > x2 + < c2 | 3 > x3 + ... < c2 | n > xn = < c2 | b >
        .......
        < cm | 2 > x2 + < cm | 3 > x3 + ... < cm | n > xn = < cm | b >
</pre>
        But the above is nothing more than a new matrix equation
<pre>
        A' | x' > = | b' >
        or

        x2 | 2'> + x3 | 3'> .... + xn | n'> = | b'>
        where
            primed vectors | 2' >, etc. are the columns of the new
            matrix A'.
</pre>
        with the problem dimension reduced by one.

<dd>
        Taking as an example a four-dimensional problem and writing
        down the successive transformations of the original equation
        we will end up with the following triangular pattern made of
        four vector equations:

<pre>
        x1 | 1 > + x2 | 2 > + x3 | 3 >  + x4 | 4 >   = | b >
                   x2 | 2'> + x3 | 3'>  + x4 | 4'>   = | b'>
                              x3 | 3''> + x4 | 4''>  = | b''>
                                          x4 | 4'''> = | b'''>
</pre>
        But if we premultiply each vector equation by a non-zero vector
        of our choice, say &lt; 1 | , &lt; 2' |, &lt; 3'' |, and &lt; 4''' | - chosen
        correspondingly for equations 1, 2, 3 and 4, then the above
        system of vector equations will be converted to much simpler
        system of scalar equations. The result is
        shown below in matrix representation:

<pre>
        | p11  p12   p13   p14 | | x1 | = | q1 |
        | 0    p22   p23   p24 | | x2 | = | q2 |
        | 0    0     p33   p34 | | x3 | = | q3 |
        | 0    0     0     p44 | | x4 | = | q4 |
</pre>
        In effect, we have triangularized our original matrix A.
        Below is a function that does that for any problem size:
<pre>

> one_ket_triangle :: (Scalar a, Fractional a) => [[a]] -> [a] -> [([a],a)]
> one_ket_triangle a b
>     --
>     -- List of pairs: (p, q) representing
>     -- rows of triangular matrix P and of vector | q >
>     -- in the equation P | x > = | q >, which
>     -- has been obtained by linear transformation
>     -- of the original equation A | x > = | b >
>     --
>     | null a = []
>     | otherwise = (p,q):(one_ket_triangle a' b')
>     where
>         p    = [bra_ket u ak | ak <- a]
>         q    = bra_ket u b
>         a'   = [[bra_ket ck ai | ck <- orth] | ai <- v]
>         b'   = [ bra_ket ck b  | ck <- orth]
>         orth = orthogonals u
>         u    = head a
>         v    = tail a

</pre>
        The triangular system of equations can be easily solved by
        successive substitutions - starting with the last equation.

<pre>

> one_ket_solution :: (Scalar a, Fractional a) => [[a]] -> [a] -> [a]
> one_ket_solution a b =
>     --
>     -- List representing vector |x>, which is
>     -- a solution of the matrix equation
>     --     A |x> = |b>
>     -- where
>     --     a is a list of columns of matrix A
>     --     b is a list representing vector |b>
>     --
>     solve' (unzip (reverse (one_ket_triangle a b))) []
>     where
>         solve' (d, c) xs
>             | null d  = xs
>             | otherwise = solve' ((tail d), (tail c)) (x:xs)
>             where
>                 x = (head c - (sum_product (tail u) xs))/(head u)
>                 u = head d

</pre>
        The triangularization procedure can be easily extended
        to a list of several ket-vectors | b &gt; on the right hand
        side of the original equation A | x &gt; = | b &gt; -- instead
        of just one:
<pre>

> many_kets_triangle :: (Scalar a, Fractional a) => [[a]] -> [[a]] -> [([a],[a])]
> many_kets_triangle a b
>     --
>     -- List of pairs: (p, q) representing
>     -- rows of triangular matrix P and of rectangular matrix Q
>     -- in the equation P X = Q, which
>     -- has been obtained by linear transformation
>     -- of the original equation A X = B
>     -- where
>     --     a is a list of columns of matrix A
>     --     b is a list of columns of matrix B
>     --
>     | null a = []
>     | otherwise = (p,q):(many_kets_triangle a' b')
>     where
>         p    = [bra_ket u ak   | ak <- a]
>         q    = [bra_ket u bk   | bk <- b]
>         a'   = [[bra_ket ck ai | ck <- orth] | ai <- v]
>         b'   = [[bra_ket ck bi | ck <- orth] | bi <- b]
>         orth = orthogonals u
>         u    = head a
>         v    = tail a

</pre>
        Similarly, function 'one_ket_solution' can be generalized
        to function 'many_kets_solution' that handles cases with
        several ket-vectors on the right hand side.
<pre>

> many_kets_solution :: (Scalar a, Fractional a) => [[a]] -> [[a]] -> [[a]]
> many_kets_solution a b =
>     --
>     -- List of columns of matrix X, which is
>     -- a solution of the matrix equation
>     --     A X = B
>     -- where
>     --     a is a list of columns of matrix A
>     --     b is a list of columns of matrix B
>     --
>     solve' p q emptyLists
>     where
>         (p, q) = unzip (reverse (many_kets_triangle a b))
>         emptyLists = [[] | _ <- [1..(length (head q))]]
>         solve' a' b' x
>             | null a'  = x
>             | otherwise = solve' (tail a') (tail b')
>                                 [(f vk xk):xk  | (xk, vk) <- (zip x v)]
>             where
>                 f vk xk = (vk - (sum_product (tail u) xk))/(head u)
>                 u = head a'
>                 v = head b'


</pre>
<p>
<hr>
<p>
<b>
        Matrix inversion
</b>
<p>
        Function 'many_kets_solution' can be used to compute
        inverse of matrix A by specializing matrix B to a unit
        matrix I:
<pre>

        A X = I
</pre>
        It follows that matrix X is an inverse of A; that is X = A<sup>-1</sup>.
<pre>

> inverse :: (Scalar a, Fractional a) => [[a]] -> [[a]]
> inverse a = many_kets_solution a (unit_matrix (length a))
>       --
>       -- List of columns of inverse of matrix A
>       -- where
>       --     a is list of columns of A

</pre>
<p>
<hr>
<p>
<b>
        QR factorization of matrices
</b>
<p>
        The process described above and implemented by
        'many_kets_triangle' function transforms the equation
<pre>
        A X = B
</pre>
        into another equation for the same matrix X
<pre>
        R X = S
</pre>
        where R is an upper triangular matrix. All operations
        performed on matrices A and B during this process are linear,
        and therefore we should be able to find a square matrix Q
        that describes the entire process in one step. Indeed, assuming
        that matrix A can be decomposed as a product of unknown matrix Q
        and triangular matrix R and that Q<sup>-1</sup> is an inverse of matrix Q
        we can reach the last equation by following these steps:
<pre>
        A X       = B
        (Q R) X   = B
        Q<sup>-1</sup> Q R X = Q<sup>-1</sup> B
        R X       = S
</pre>
        It follows that during this process a given matrix B
        transforms to matrix S, as delivered by 'many_kets_triangle':
<pre>
        S = Q<sup>-1</sup> B
</pre>
        from which the inverse of Q can be found:
<pre>
        Q<sup>-1</sup> = S B<sup>-1</sup>
</pre>
        Having a freedom of choice of the right hand side matrix B
        we can choose the unit matrix I in place of B, and therefore
        simplify the definition of Q<sup>-1</sup>:
<pre>
        Q<sup>-1</sup> = S,  if B is unit matrix
</pre>
        It follows that any non-singular matrix A can be decomposed
        as a product of a matrix Q and a triangular matrix R

<pre>
        A = Q R
</pre>
        where matrices Q<sup>-1</sup> and R are delivered by "many_kets_triangle"
        as a result of triangularization process of equation:
<pre>
        A X = I
</pre>
        The function below extracts a pair of matrices Q and R
        from the answer provided by "many_kets_triangle".
        During this process it inverts matrix Q<sup>-1</sup> to Q.
        This factorization will be used by a sequence of similarity
        transformations to be defined in the next section.

<pre>

> factors_QR :: (Scalar a, Fractional a) => [[a]] -> ([[a]],[[a]])
> factors_QR a =
>       --
>       -- A pair of matrices (Q, R), such that
>       -- A = Q R
>       -- where
>       --     R is upper triangular matrix in row representation
>       --     (without redundant zeros)
>       --     Q is a transformation matrix in column representation
>       --     A is square matrix given as columns
>       --
>       (inverse (transposed q1),r)
>       where
>           (r, q1) = unzip (many_kets_triangle a (unit_matrix (length a)))

</pre>

<p>
<hr>
<p>
<b>
        Computation of the determinant
</b>

<!-- added by Henning Thielemann -->

<pre>

> determinant :: (Scalar a, Fractional a) => [[a]] -> a
> determinant a =
>    let (q,r) = factors_QR a
>    -- matrix Q is not normed so we have to respect the norms of its rows
>    in  product (map norm q) * product (map head r)

</pre>

Naive division-free computation of the determinant by expanding the first column.
It consumes n! multiplications.

<pre>

> determinantNaive :: (Num a) => [[a]] -> a
> determinantNaive [] = 1
> determinantNaive m  =
>    sum (alternate
>       (zipWith (*) (map head m)
>           (map determinantNaive (removeEach (map tail m)))))

</pre>

Compute the determinant with about n^4 multiplications
without division according to the clow decomposition algorithm
of Mahajan and Vinay, and Berkowitz
as presented by Günter Rote:
<a href="http://page.inf.fu-berlin.de/~rote/Papers/pdf/Division-free+algorithms.pdf">
Division-Free Algorithms for the Determinant and the Pfaffian:
Algebraic and Combinatorial Approaches</a>.

<pre>

> determinantClow :: (Num a) => [[a]] -> a
> determinantClow [] = 1
> determinantClow m =
>    let lm = length m
>    in  parityFlip lm (last (newClow m
>           (nest (lm-1) (longerClow m)
>               (take lm (iterate (0:) [1])))))

</pre>

Compute the weights of all clow sequences
where the last clow is closed and a new one is started.

<pre>

> newClow :: (Num a) => [[a]] -> [[a]] -> [a]
> newClow a c =
>    scanl (-) 0
>          (sumVec (zipWith (zipWith (*)) (List.transpose a) c))

</pre>

Compute the weights of all clow sequences
where the last (open) clow is extended by a new arc.

<pre>

> extendClow :: (Num a) => [[a]] -> [[a]] -> [[a]]
> extendClow a c =
>    map (\ai -> sumVec (zipWith scaleVec ai c)) a

</pre>

Given the matrix of all weights of clows of length l
compute the weight matrix for all clows of length (l+1).
Take the result of 'newClow' as diagonal
and the result of 'extendClow' as lower triangle
of the weight matrix.

<pre>

> longerClow :: (Num a) => [[a]] -> [[a]] -> [[a]]
> longerClow a c =
>    let diagonal = newClow a c
>        triangle = extendClow a c
>    in  zipWith3 (\i t d -> take i t ++ [d]) [0 ..] triangle diagonal

</pre>

Auxiliary functions for the clow determinant.

<pre>

> {- | Compositional power of a function,
>      i.e. apply the function n times to a value. -}
> nest :: Int -> (a -> a) -> a -> a
> nest 0 _ x = x
> nest n f x = f (nest (n-1) f x)
>
> {- successively select elements from xs and remove one in each result list -}
> removeEach :: [a] -> [[a]]
> removeEach xs =
>    zipWith (++) (List.inits xs) (tail (List.tails xs))
>
> alternate :: (Num a) => [a] -> [a]
> alternate = zipWith id (cycle [id, negate])
>
> parityFlip :: Num a => Int -> a -> a
> parityFlip n x = if even n then x else -x
>
> {-| Weight a list of numbers by a scalar. -}
> scaleVec :: (Num a) => a -> [a] -> [a]
> scaleVec k = map (k*)
>
> {-| Add corresponding numbers of two lists. -}
> {- don't use zipWith because it clips to the shorter list -}
> addVec :: (Num a) => [a] -> [a] -> [a]
> addVec x [] = x
> addVec [] y = y
> addVec (x:xs) (y:ys) = x+y : addVec xs ys
>
> {-| Add some lists. -}
> sumVec :: (Num a) => [[a]] -> [a]
> sumVec = foldl addVec []

</pre>



<p>
<hr>
<p>
<b>
        Similarity transformations and eigenvalues
</b>
<p>
        Two n-square matrices A and B are called similar if there
        exists a non-singular matrix S such that:
<pre>
        B = S<sup>-1</sup> A S
</pre>

        It can be proven that:
<ul>
<li>
        Any two similar matrices have the same eigenvalues
<li>
        Every n-square matrix A is similar to a triangular matrix
        whose diagonal elements are the eigenvalues of A.
</ul>
<p>
        If matrix A can be transformed to a triangular or a diagonal
        matrix Ak by a sequence of similarity transformations then
        the eigenvalues of matrix A are the diagonal elements of Ak.

<p>

        Let's construct the sequence of matrices similar to A
<pre>
        A, A1, A2, A3...
</pre>
        by the following iterations - each of which factorizes a matrix
        by applying the function 'factors_QR' and then forms a product
        of the factors taken in the reverse order:
<pre>
        A = Q R          = Q (R Q) Q<sup>-1</sup>    = Q A1 Q<sup>-1</sup>        =
          = Q (Q1 R1) Q<sup>-1</sup> = Q Q1 (R1 Q1) Q1<sup>-1</sup> Q<sup>-1</sup> = Q Q1 A2 Q1<sup>-1</sup> Q<sup>-1</sup> =
          = Q Q1 (Q2 R2) Q1<sup>-1</sup> Q<sup>-1</sup> = ...

</pre>
        We are hoping that after some number of iterations some matrix
        Ak would become triangular and therefore its diagonal
        elements could serve as eigenvalues of matrix A. As long as
        a matrix has real eigenvalues only, this method should work well.
        This applies to symmetric and hermitian matrices. It appears
        that general complex matrices -- hermitian or not -- can also
        be handled this way. Even more, this method also works for some
        nonsymmetric real matrices, which have real eigenvalues only.
<dd>
        The only type of matrices that cannot be treated by this algorithm
        are real nonsymmetric matrices, whose some eigenvalues are complex.
        There is no operation in the process that converts real elements
        to complex ones, which could find their way into diagonal
        positions of a triangular matrix. But a simple preconditioning
        of a matrix -- described in the next section -- replaces
        a real matrix by a complex one, whose eigenvalues are related
        to the eigenvalues of the matrix being replaced. And this allows
        us to apply the same method all across the board.
<dd>
        It is worth noting that a process known in literature as QR
        factorization is not uniquely defined and different algorithms
        are employed for this. The algorithms using QR factorization
        apply only to symmetric or hermitian matrices, and Q matrix
        must be either orthogonal or unitary.
<dd>
        But our transformation matrix Q is not orthogonal nor unitary,
        although its first row is orthogonal to all other rows. In fact,
        this factorization is only similar to QR factorization. We just
        keep the same name to help identify a category of the methods
        to which it belongs.
<dd>
        The same factorization is used for tackling two major problems:
        solving the systems of linear equations and finding the eigenvalues
        of matrices.
<dd>
        Below is the function 'similar_to', which makes a new matrix that is
        similar to a given matrix by applying our similarity transformation.
<dd>
        Function 'iterated_eigenvalues' applies this transformation n
        times - storing diagonals of each new matrix as approximations of
        eigenvalues.
<dd>
        Function 'eigenvalues' follows the same process but reports the last
        approximation only.
<pre>


> similar_to :: (Scalar a, Fractional a) => [[a]] -> [[a]]
> similar_to a =
>       --
>       -- List of columns of matrix A1 similar to A
>       -- obtained by factoring A as Q R and then
>       -- forming the product A1 = R Q = (inverse Q) A Q
>       -- where
>       --     a is list of columns of A
>       --
>       triangle_matrix' r q
>       where
>           (q,r) = factors_QR a

> iterated_eigenvalues :: (Scalar a1, Fractional a1, Eq a, Num a) => [[a1]] -> a -> [[a1]]
> iterated_eigenvalues a n
>       --
>       -- List of vectors representing
>       -- successive approximations of
>       -- eigenvalues of matrix A
>       -- where
>       --     a is a list of columns of A
>       --     n is a number of requested iterations
>       --
>       | n == 0 = []
>       | otherwise = (diagonals a)
>                     : iterated_eigenvalues (similar_to a) (n-1)

> eigenvalues :: (Scalar a1, Fractional a1, Eq a, Num a) => [[a1]] -> a -> [a1]
> eigenvalues a n
>       --
>       -- Eigenvalues of matrix A
>       -- obtained by n similarity iterations
>       -- where
>       --     a are the columns of A
>       --
>       | n == 0    = diagonals a
>       | otherwise = eigenvalues (similar_to a) (n-1)

</pre>
<p>
<hr>
<p>
<b>
        Preconditioning of real nonsymmetric matrices
</b>
<p>
        As mentioned above, our QR-like factorization method works
        well with almost all kind of matrices, but with the exception
        of a class of real nonsymmetric matrices that have
        complex eigenvalues.
<dd>
        There is no mechanism in that method that would be able to
        produce complex eigenvalues out of the real components of
        this type of nonsymmetric matrices. Simple trivial replacement
        of real components of a matrix by its complex counterparts
        does not work because zero-valued imaginary components do
        not contribute in any way to production of nontrivial
        imaginary components during the factorization process.
<dd>
        What we need is a trick that replaces real nonsymmetric matrix
        by a nontrivial complex matrix in such a way that the results
        of such replacements could be undone when the series of
        similarity transformations finally produced the expected
        effect in a form of a triangular matrix.
<dd>
        The practical solution is surprisingly simple:
        it's suffice to add any complex number, such as "i", to the
        main diagonal of a matrix, and when triangularization is done
        -- subtract it back from computed eigenvalues.
        The explanation follows.
<p>
        Consider the eigenproblem for real and nonsymmetric matrix A.
<pre>
        A | x > = a | x >
</pre>
        Let us now define a new complex matrix B, such that:
<pre>
        B = A + alpha I
        where
            I is a unit matrix and alpha is a complex scalar
</pre>
        It is obvious that matrices A and B commute; that is:
<pre>
        A B = B A
</pre>
        It can be proven that if two matrices commute then they
        have the same eigenvectors. Therefore we can use vector
        | x &gt; of matrix A as an eigenvector of B:
<pre>
        B | x > = b | x >
        B | x > = A | x > + alpha I | x >
                = a | x > + alpha | x >
                = (a + alpha) | x >
</pre>
        It follows that eigenvalues of B are related to the eigenvalues
        of A by:
<pre>
        b = a + alpha
</pre>
        After eigenvalues of complex matrix B have been succesfully
        computed, all what remains is to subtract "alpha" from them
        all to obtain eigenvalues of A. And nothing has to be done
        to eigenvectors of B - they are the same for A as well.
        Simple and elegant!
<p>
        Below is an auxiliary function that adds a scalar to the
        diagonal of a matrix:

<pre>

> add_to_diagonal :: Num a => a -> [[a]] -> [[a]]
> add_to_diagonal alpha a =
>       --
>       -- Add constant alpha to diagonal of matrix A
>       --
>       [f ai ni | (ai,ni) <- zip a [0..(length a -1)]]
>       where
>           f b k = p++[head q + alpha]++(tail q)
>               where
>                   (p,q) = splitAt k b
>


</pre>
<p>
<hr>
<p>
<b>
        Examples of iterated eigenvalues
</b>
<p>


        Here is an example of a symmetric real matrix with results
        of application of function 'iterated_eigenvalues'.
<pre>
        | 7  -2  1 |
        |-2  10 -2 |
        | 1  -2  7 |

         [[7.0,     10.0,    7.0],
          [8.66667, 9.05752, 6.27582],
          [10.7928, 7.11006, 6.09718],
          [11.5513, 6.40499, 6.04367],
          [11.7889, 6.18968, 6.02142],
          [11.8943, 6.09506, 6.01068],
          [11.9468, 6.04788, 6.00534],
          [11.9733, 6.02405, 6.00267],
          [11.9866, 6.01206, 6.00134],
          [11.9933, 6.00604, 6.00067],
          [11.9966, 6.00302, 6.00034],
          [11.9983, 6.00151, 6.00017],
          [11.9992, 6.00076, 6.00008],
          [11.9996, 6.00038, 6.00004],
          [11.9998, 6.00019, 6.00002],
          [11.9999, 6.00010, 6.00001],
          [11.9999, 6.00005, 6.00001]]

          The true eigenvalues are:
          12, 6, 6

</pre>
        Here is an example of a hermitian matrix. (Eigenvalues of hermitian
        matrices are real.) The algorithm works well and converges fast.
<pre>
        | 2   0     i|
        [ 0   1   0  |
        [ -i  0   2  |

        [[2.8     :+ 0.0, 1.0 :+ 0.0, 1.2     :+ 0.0],
         [2.93979 :+ 0.0, 1.0 :+ 0.0, 1.06021 :+ 0.0],
         [2.97972 :+ 0.0, 1.0 :+ 0.0, 1.02028 :+ 0.0],
         [2.9932  :+ 0.0, 1.0 :+ 0.0, 1.0068  :+ 0.0],
         [2.99773 :+ 0.0, 1.0 :+ 0.0, 1.00227 :+ 0.0],
         [2.99924 :+ 0.0, 1.0 :+ 0.0, 1.00076 :+ 0.0],
         [2.99975 :+ 0.0, 1.0 :+ 0.0, 1.00025 :+ 0.0],
         [2.99992 :+ 0.0, 1.0 :+ 0.0, 1.00008 :+ 0.0],
         [2.99997 :+ 0.0, 1.0 :+ 0.0, 1.00003 :+ 0.0],
         [2.99999 :+ 0.0, 1.0 :+ 0.0, 1.00001 :+ 0.0],
         [3.0     :+ 0.0, 1.0 :+ 0.0, 1.0     :+ 0.0],
         [3.0     :+ 0.0, 1.0 :+ 0.0, 1.0     :+ 0.0],
         [3.0     :+ 0.0, 1.0 :+ 0.0, 1.0     :+ 0.0]]

</pre>
        Here is another example: this is a complex matrix and it is not
        even hermitian. Yet, the algorithm still works, although its
        fluctuates around true values.
<pre>
        | 2-i   0      i |
        | 0     1+i  0   |
        |   i   0    2-i |

        [[2.0     :+ (-1.33333), 1.0 :+ 1.0, 2.0     :+ (-0.666667)],
         [1.89245 :+ (-1.57849), 1.0 :+ 1.0, 2.10755 :+ (-0.421509)],
         [1.81892 :+ (-1.80271), 1.0 :+ 1.0, 2.18108 :+ (-0.197289)],
         [1.84565 :+ (-1.99036), 1.0 :+ 1.0, 2.15435 :+ (-0.00964242)],
         [1.93958 :+ (-2.07773), 1.0 :+ 1.0, 2.06042 :+ 0.0777281],
         [2.0173  :+ (-2.06818), 1.0 :+ 1.0, 1.9827  :+ 0.0681793],
         [2.04357 :+ (-2.02437), 1.0 :+ 1.0, 1.95643 :+ 0.0243654],
         [2.03375 :+ (-1.99072), 1.0 :+ 1.0, 1.96625 :+ (-0.00928429)],
         [2.01245 :+ (-1.97875), 1.0 :+ 1.0, 1.98755 :+ (-0.0212528)],
         [1.99575 :+ (-1.98307), 1.0 :+ 1.0, 2.00425 :+ (-0.0169263)],
         [1.98938 :+ (-1.99359), 1.0 :+ 1.0, 2.01062 :+ (-0.00640583)],
         [1.99145 :+ (-2.00213), 1.0 :+ 1.0, 2.00855 :+ 0.00212504],
         [1.9968  :+ (-2.00535), 1.0 :+ 1.0, 2.0032  :+ 0.00535265],
         [2.00108 :+ (-2.00427), 1.0 :+ 1.0, 1.99892 :+ 0.0042723],
         [2.00268 :+ (-2.00159), 1.0 :+ 1.0, 1.99732 :+ 0.00158978],
         [2.00213 :+ (-1.99946), 1.0 :+ 1.0, 1.99787 :+ (-0.000541867)],
         [2.00079 :+ (-1.99866), 1.0 :+ 1.0, 1.9992  :+ (-0.00133514)],
         [1.99973 :+ (-1.99893), 1.0 :+ 1.0, 2.00027 :+ (-0.00106525)],
         [1.99933 :+ (-1.9996) , 1.0 :+ 1.0, 2.00067 :+ (-0.000397997)],
         [1.99947 :+ (-2.00013), 1.0 :+ 1.0, 2.00053 :+ 0.000134972]]

         The true eigenvalues are
         2 - 2i, 1 + i, 2
</pre>
        Some nonsymmetric real matrices have all real eigenvalues and
        our algorithm still works for such cases. Here is one
        such an example, which traditionally would have to be treated
        by one of the Lanczos-like algorithms, specifically designed
        for nonsymmetric real matrices. Evaluation of
<br>
<i>
        iterated_eigenvalues [[2,1,1],[-2,1,3],[3,1,-1::Double]] 20
</i>
<br>
        gives the following results
<pre>
        [[3.0,     -0.70818,-0.291815],
         [3.06743, -3.41538, 2.34795],
         [3.02238, -1.60013, 0.577753],
         [3.00746, -2.25793, 1.25047],
         [3.00248, -1.88764, 0.885154],
         [3.00083, -2.06025, 1.05943],
         [3.00028, -1.97098, 0.970702],
         [3.00009, -2.0148,  1.01471],
         [3.00003, -1.99268, 0.992648],
         [3.00001, -2.00368, 1.00367],
         [3.0,     -1.99817, 0.998161],
         [3.0,     -2.00092, 1.00092],
         [3.0,     -1.99954, 0.99954],
         [3.0,     -2.00023, 1.00023],
         [3.0,     -1.99989, 0.999885],
         [3.0,     -2.00006, 1.00006],
         [3.0,     -1.99997, 0.999971],
         [3.0,     -2.00001, 1.00001],
         [3.0,     -1.99999, 0.999993],
         [3.0,     -2.0,     1.0]]

         The true eigenvalues are:
         3, -2, 1
</pre>
        Finally, here is a case of a nonsymmetric real matrix with
        complex eigenvalues:
<pre>
        | 2 -3 |
        | 1  0 |
</pre>
        The direct application of "iterated_eigenvalues" would
        fail to produce expected eigenvalues:
<pre>
        1 + i sqrt(2) and 1 - i sqrt (2)
</pre>
        But if we first precondition the matrix by adding "i" to its diagonal:
<pre>
        | 2+i  -3|
        | 1     i|
</pre>
        and then compute its iterated eigenvalues:
<br>
<i>
        iterated_eigenvalues [[2:+1,1],[-3,0:+1]] 20
</i>
<br>
        then the method will succeed. Here are the results:
<pre>

        [[1.0     :+ 1.66667, 1.0     :+   0.333333 ],
        [0.600936 :+ 2.34977, 1.39906 :+ (-0.349766)],
        [0.998528 :+ 2.59355, 1.00147 :+ (-0.593555)],
        [1.06991  :+ 2.413,   0.93009 :+ (-0.412998)],
        [1.00021  :+ 2.38554, 0.99979 :+ (-0.385543)],
        [0.988004 :+ 2.41407, 1.012   :+ (-0.414074)],
        [0.999963 :+ 2.41919, 1.00004 :+ (-0.419191)],
        [1.00206  :+ 2.41423, 0.99794 :+ (-0.414227)],
        [1.00001  :+ 2.41336, 0.99999 :+ (-0.413361)],
        [0.999647 :+ 2.41421, 1.00035 :+ (-0.414211)],
        [0.999999 :+ 2.41436, 1.0     :+ (-0.41436) ],
        [1.00006  :+ 2.41421, 0.99993 :+ (-0.414214)],
        [1.0      :+ 2.41419, 1.0     :+ (-0.414188)],
        [0.99999  :+ 2.41421, 1.00001 :+ (-0.414213)],
        [1.0      :+ 2.41422, 1.0     :+ (-0.414218)],
        [1.0      :+ 2.41421, 0.99999 :+ (-0.414213)],
        [1.0      :+ 2.41421, 1.0     :+ (-0.414212)],
        [1.0      :+ 2.41421, 1.0     :+ (-0.414213)],
        [1.0      :+ 2.41421, 1.0     :+ (-0.414213)],
        [1.0      :+ 2.41421, 1.0     :+ (-0.414213)]]
</pre>
        After subtracting "i" from the last result, we will get
        what is expected.

<p>
<hr>
<p>
<b>
        Eigenvectors for distinct eigenvalues
</b>
<p>
        Assuming that eigenvalues of matrix A are already found
        we may now attempt to find the corresponding aigenvectors
        by solving the following homogeneous equation
<pre>
        (A - a I) | x > = 0
</pre>
        for each eigenvalue "a". The matrix
<pre>
        B = A - a I
</pre>
        is by definition singular, but in most cases it can be
        triangularized by the familiar "factors_QR" procedure.
<pre>
        B | x > = Q R | x > = 0
</pre>
        It follows that the unknown eigenvector | x &gt; is one of
        the solutions of the homogeneous equation:

<pre>
        R | x > = 0
</pre>
        where R is a singular, upper triangular matrix with at least one
        zero on its diagonal.
<dd>
        If | x &gt; is a solution we seek, so is its scaled version
        alpha | x &gt;. Therefore we have some freedom of scaling choice.
        Since this is a homogeneous equation, one of the components
        of | x &gt; can be freely chosen, while the remaining components
        will depend on that choice.
</pre>
        To solve the above, we will be working from the bottom up of
        the matrix equation, as illustrated in the example below:
<pre>
        | 0     1     1     3     | | x1 |
        | 0     1     1     2     | | x2 |      /\
        | 0     0     2     4     | | x3 | = 0  ||
        | 0     0     0     0     | | x4 |      ||
</pre>
        Recall that the diagonal elements of any triangular matrix
        are its eigenvalues.
        Our example matrix has three distinct eigenvalues:
        0, 1, 2. The eigenvalue 0 has degree of degeneration two.
        Presence of degenerated eigenvalues complicates
        the solution process. The complication arises when we have to
        make our decision about how to solve the trivial scalar equations
        with zero coefficients, such as
<pre>
        0 * x4 = 0
</pre>
        resulting from multiplication of the bottom row by vector | x &gt;.
        Here we have two choices: "x4" could be set to 0, or to any
        nonzero number 1, say. By always choosing the "0" option
        we might end up with the all-zero trivial vector --  which is
        obviously not what we want. Persistent choice of the "1" option,
        might lead to a conflict between some of the equations, such as
        the equations one and four in our example.
<p>
        So the strategy is as follows.
<p>
        If there is at least one zero on the diagonal, find the topmost
        row with zero on the diagonal and choose for it the solution "1".
        Diagonal zeros in other rows would force the solution "0".
        If the diagonal element is not zero than simply solve
        an arithmetic equation that arises from the substitutions of
        previously computed components of the eigenvector. Since certain
        inaccuracies acumulate during QR factorization, set to zero all
        very small elements of matrix R.
<p>
        By applying this strategy to our example we'll end up with the
        eigenvector
<pre>
        < x | = [1, 0, 0, 0]
</pre>

<p>
        If the degree of degeneration of an eigenvalue of A is 1 then the
        corresponding eigenvector is unique -- subject to scaling.
        Otherwise an eigenvector found by this method is one of many
        possible solutions, and any linear combination of such solutions
        is also an eigenvector. This method is not able to find more than one
        solution for degenerated eigenvalues. An alternative method, which
        handles degenerated cases, will be described in the next section.
<p>
        The function below calculates eigenvectors corresponding to
        distinct selected eigenvalues of any square matrix A, provided
        that the singular matrix B = A - a I can still be factorized as Q R,
        where R is an upper triangular matrix.

<pre>

> eigenkets :: (Scalar a, Fractional a) => [[a]] -> [a] -> [[a]]
> eigenkets a u
>       --
>       -- List of eigenkets of a square matrix A
>       -- where
>       --     a is a list of columns of A
>       --     u is a list of eigenvalues of A
>       --     (This list does not need to be complete)
>       --
>       | null u        = []
>       | not (null x') = x':(eigenkets a (tail u))
>       | otherwise     = (eigenket_UT (reverse b) d []):(eigenkets a (tail u))
>       where
>           a'  = add_to_diagonal (-(head u)) a
>           x'  = unit_ket a' 0 (length a')
>           b   = snd (factors_QR a')
>           d   = discriminant [head bk | bk <- b] 1
>           discriminant v n
>               | null v    = []
>               | otherwise = x : (discriminant (tail v) m)
>               where
>                   (x, m)
>                       | (head u) == 0     = (n, 0)
>                       | otherwise         = (n, n)
>           eigenket_UT c e xs
>               | null c    = xs
>               | otherwise = eigenket_UT (tail c) (tail e) (x:xs)
>               where
>                   x = solve_row (head c) (head e) xs
>
>           solve_row (v:vs) n x
>               | almostZero v = n
>               | otherwise    = q/v
>               where
>                   q
>                       | null x = 0
>                       | otherwise = -(sum_product vs x)
>
>           unit_ket b' m n
>               | null b'              = []
>               | all (== 0) (head b') = unit_vector m n
>               | otherwise            = unit_ket (tail b') (m+1) n

</pre>
<p>
<hr>
<p>
<b>
        Eigenvectors for degenerated eigenvalues
</b>
<p>
        Few facts:
<ul>
<li>
        Eigenvectors of a general matrix A, which does not have any
        special symmetry, are not generally orthogonal. However, they
        are orthogonal, or can be made orthogonal, to another set of
        vectors that are eigenvectors of adjoint matrix A<sup>+</sup>;
        that is the matrix obtained by complex conjugation and transposition
        of matrix A.
<li>
        Eigenvectors corresponding to nondegenerated eigenvalues of
        hermitian or symmetric matrix are orthogonal.
<li>
        Eigenvectors corresponding to degenerated eigenvalues are - in
        general - neither orthogonal among themselves, nor orthogonal
        to the remaining eigenvectors corresponding to other
        eigenvalues. But since any linear combination of such degenerated
        eigenvectors is also an eigenvector, we can orthogonalize
        them by Gram-Schmidt orthogonalization procedure.
</ul>
        Many practical applications deal solely with hermitian
        or symmetric matrices, and for such cases the orthogonalization
        is not only possible, but also desired for variety of reasons.
<dd>
        But the method presented in the previous section is not able
        to find more than one eigenvector corresponding to a degenerated
        eigenvalue. For example, the symmetric matrix
<pre>
            |  7  -2   1 |
        A = | -2  10  -2 |
            |  1  -2   7 |
</pre>
        has two distinct eigenvalues: 12 and 6 -- the latter
        being degenerated with degree of two. Two corresponding
        eigenvectors are:
<pre>
        < x1 | = [1, -2, 1] -- for 12
        < x2 | = [1,  1, 1] -- for 6
</pre>
        It happens that those vectors are orthogonal, but this is
        just an accidental result. However, we are missing a third
        distinct eigenvector. To find it we need another method.
        One possibility is presented below and the explanation
        follows.
<pre>

> eigenket' :: (Scalar a, Fractional a) => [[a]] -> a -> a -> [a] -> [a]
> eigenket' a alpha eps x' =
>       --
>       -- Eigenket of matrix A corresponding to eigenvalue alpha
>       -- where
>       --     a is a list of columns of matrix A
>       --     eps is a trial inaccuracy factor
>       --         artificially introduced to cope
>       --         with singularities of A - alpha I.
>       --         One might try eps = 0, 0.00001, 0.001, etc.
>       --     x' is a trial eigenvector
>       --
>       scaled [xk' - dk | (xk', dk) <- zip x' d]
>       where
>           b = add_to_diagonal (-alpha*(1+eps)) a
>           d = one_ket_solution b y
>           y = matrix_ket (transposed b) x'

</pre>
        Let us assume a trial vector | x' &gt;, such that
<pre>
        | x' > = | x > + | d >
        where
            | x > is an eigenvector we seek
            | d > is an error of our estimation of | x >
</pre>
        We first form a matrix B, such that:
<pre>
        B = A - alpha I
</pre>
        and multiply it by the trial vector | x' &gt;, which
        results in a vector | y &gt;
<pre>
        B | x' > = |y >
</pre>
        On another hand:
<pre>
        B | x' > = B | x > + B | d > = B | d >
        because
            B | x > = A | x > - alpha | x > = 0
</pre>
        Comparing both equations we end up with:
<pre>
        B | d > = | y >
</pre>
        that is: with the system of linear equations for unknown error | d &gt;.
        Finally, we subtract error | d &gt; from our trial vector | x' &gt;
        to obtain the true eigenvector | x &gt;.
<p>
        But there is some problem with this approach: matrix B is
        by definition singular, and as such, it might be difficult
        to handle. One of the two processes might fail, and their failures
        relate to division by zero that might happen during either the
        QR factorization, or the solution of the triangular system of equations.
<p>
        But if we do not insist that matrix B should be exactly singular,
        but almost singular:
<pre>
        B = A - alpha (1 + eps) I
</pre>
        then this method might succeed. However, the resulting eigenvector
        will be the approximation only, and we would have to experiment
        a bit with different values of "eps" to extrapolate the true
        eigenvector.
<p>
        The trial vector | x' &gt; can be chosen randomly, although some
        choices would still lead to singularity problems. Aside from
        this, this method is quite versatile, because:
<ul>
<li>
        Any random vector | x' &gt; leads to the same eigenvector
        for nondegenerated eigenvalues,
<li>
        Different random vectors | x' &gt;, chosen for degenerated
        eigenvalues, produce -- in most cases -- distinct eigenvectors.
        And this is what we want. If we need it, we can the always
        orthogonalize those eigenvectors either internally (always
        possible) or externally as well (possible only for hermitian
        or symmetric matrices).
</ul>
        It might be instructive to compute the eigenvectors for
        the examples used in demonstration of computation of eigenvalues.
        We'll leave to the reader, since this module is already too obese.
<p>
<hr>
<p>
<b>
        Auxiliary functions
</b>
<p>
        The functions below are used in the main algorithms of
        this module. But they can be also used for testing. For example,
        the easiest way to test the usage of resources is to use easily
        definable unit matrices and unit vectors, as in:

<pre>
        one_ket_solution (unit_matrix n::[[Double]])
                         (unit_vector 0 n::[Double])
        where n = 20, etc.


> unit_matrix :: Num a => Int -> [[a]]
> unit_matrix m =
>       --
>       -- Unit square matrix of with dimensions m x m
>       --
>       [ [ if j==k then 1 else 0 | j <- [0 .. m-1] ] | k <- [0 .. m-1]]

> unit_vector :: Num a => Int -> Int -> [a]
> unit_vector i m =
>       --
>       -- Unit vector of length m
>       -- with 1 at position i, zero otherwise
>       map (\k -> if k==i then 1 else 0) [0 .. m-1]

> diagonals :: [[a]] -> [a]
> diagonals a =
>       --
>       -- Vector made of diagonal components
>       -- of square matrix a
>       --
>       diagonals' a 0
>       where
>           diagonals' b n
>               | null b = []
>               | otherwise =
>                   (head $ drop n $ head b) : (diagonals' (tail b) (n+1))


</pre>

<pre>
-----------------------------------------------------------------------------
--
-- Copyright:
--
--      (C) 1998 Numeric Quest Inc., All rights reserved
--
-- Email:
--
--      jans@numeric-quest.com
--
-- License:
--
--      GNU General Public License, GPL
--
-----------------------------------------------------------------------------
</pre>
</ul>
</body>

<SCRIPT language="Javascript">
<!--

// FILE ARCHIVED ON 20010628005806 AND RETRIEVED FROM THE
// INTERNET ARCHIVE ON 20030626101500.
// JAVASCRIPT APPENDED BY WAYBACK MACHINE, COPYRIGHT INTERNET ARCHIVE.
// ALL OTHER CONTENT MAY ALSO BE PROTECTED BY COPYRIGHT (17 U.S.C.
// SECTION 108(a)(3)).

   var sWayBackCGI = "http://web.archive.org/web/20010628005806/";

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
