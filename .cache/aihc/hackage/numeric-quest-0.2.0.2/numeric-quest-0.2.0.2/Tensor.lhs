<html>
<head>
<BASE HREF="http://www.numeric-quest.com/haskell/Tensor.html">

<title>
        N-dimensional tensors
</title>
</head>
<body>
<ul>
<center>
<h1>
                        ***
</h1>
<h1>
        N-dimensional tensors
</h1>
<b>
<br>
        Literate Haskell module <i>Tensor.lhs</i>
</b>
<p>
        Jan Skibinski, <a href="http://www.numeric-quest.com/news/">
        Numeric Quest Inc.</a>, Huntsville, Ontario, Canada
<p>
        1999.10.08, last modified 1999.10.16

</center>
<p>
<hr>
<p>
<i>
        This is a quick sketch of what might be a basis of a real
        Tensor module. This module has quite a few limitations (listed below).
        I'd like to get some feedback on what should be a better
        way to design it properly. Nevertheless, this module works
        and is able to tackle complex and mundane manipulations
        in the very straightforward way.
<p>
        There are few arbitrary decisions we have taken. For example,
        we consider a scalar to be a tensor of rank 0. This forces us to
        do conversions between true scalars and such tensors, but it also
        saves us a lot of headache related to typing restrictions. This
        is a typical price paid for (too much?) generalization.
<p>
        To get rid of those awful sums appearing in multiplications
        of tensors we do introduce Einstein's summation convention by the way of
        text examples -- followed by the equivalent Haskell examples.
        Hopefully it is clear and be well appreciated for its economy
        of notation, which is standard in the tensor calculus.
<p>
        Datatype <code>Tensor</code> defined here is an instance
        of class <code>Eq</code>, <code>Show</code> and <code>Num</code>.
        That means that one can compare tensors for equality and perform
        basic numerical calculations, such as addition, negation,
        subtraction, multiplication, etc. -- using standard notation
        <code>(==), (/=), (+), (-), (*)</code>. In addition, several
        customized operations, such as <code> (&lt;*&gt;)</code>
        and <code>(&lt;&lt;*&gt;&gt;)</code> are defined for
        variety of inner products.

<p>
        Limitations of this module:
<ul>

<p>
<li>
        Tensor components are Doubles. Why not Fraction, Complex, etc?
        For a moment we will leave this question aside, and
        return to it some time later. But we consider it
        the important question -- which is evident from the attempts of
        such generalization in some of our other modules:
        <a href="http://www.numeric-quest.com/haskell/Orthogonals.html">
        Orthogonals</a> and
        <a href="http://www.numeric-quest.com/haskell/fractions.html">
        Fraction</a>.
<p>
<li>
        We are well aware that the decision to represent tensors
        as nested objects will have significant impact on access
        (and update -- if supported) of such data structure. Linear
        arrays seem to be better suited for such tasks, where all
        indices must be explicitely computed first, but the access
        time is linear. In contrary, the hierarchical data structure
        defined here require very little effort in index computing
        but the access time depends on the depth of the data tree.
<p>
        But speed has not been tested yet, so we really do not know
        how inefficient this module is and all of the above is
        just a pure speculation. Certain operations of this module
        seem to be quite well matched with this tree-like data structure,
        and because of it this design decision might be not so bad
        after all.

<p>
<li>
        The shape of tensors defined here involves two parameters:
        dimension and rank. Rank is associated with the
        depth of the tensor tree and corresponds to a total number
        of indices by which you can access the individual components.
        No limits are imposed on ranks and there are binary operations
        which involve tensors of different ranks.
        Dimension is associated with the breadth of the tree and
        correspond to a number of values each index can take.
        Dimension is fixed via constant <code>dims</code>. At first it might
        seem as a severe limitation, but in fact one should never
        mix tensors with different dimensions. One usually works
        either with three-dimensional tensors (classical mechanics,
        electrodynamics, elasticity, etc.) or the four-dimentional
        tensors (relativity theory).
</ul>
<p>
</i>
<p>
<hr>
<p>
<b>
        Tensor datatype
</b>
<p>
<pre>

> module Tensor where
> import Data.Array(inRange)
> import Prelude2010
> import Prelude ()
>
> infixl 9 #      -- used for tensor indexing
> infixl 9 ##     -- used for indices expressed as lists
> infixl 7 <*>    -- inner product with one bound
> infixl 7 <<*>>  -- inner product with two bounds

</pre>
        Indices will assume values from range (1,dims) (defined below).
<p>

        Tensor can contain a scalar value or a list of tensors.
        This recursively defines tensor of any rank in n-D space.

<pre>

> data Tensor = S Double
>             | T [Tensor]


</pre>
        There is no way we could specify the length of the list
        <code>[Tensor]</code> in the data declaration. Typing is not
        concerned with shapes.
        We could of course use more specific representation of
        this data structure, such as:
<pre>
        data Tensor = S Double | T Tensor Tensor Tensor
</pre>
        but then we would severily limit ourselves to three-dimensional
        tensors.
<p>

        Rank is either 0 (scalars), 1 (vectors), or higher: 2, 3, 4 ...
<pre>

> rank :: Tensor -> Int
> rank t = rank' 0 t where
>       rank' n (S _)     = n
>       rank' n (T xs)    = rank' (n+1) (head xs)

</pre>
        Here we define our tensor dimension as constant for this
        module. All binary operations on tensors require the
        same dimensions, so it makes sense to treat dimensions
        as constants. But ranks can be different.
<pre>

> dims :: Int
> dims = 3

</pre>

<p>
<hr>
<p>
<b>
        Showing
</b>
<p>
        Tensors are printed as recursive lists with a word "Tensor"
        prepended

<pre>

> instance Show Tensor where
>       showsPrec 0 (S a)     = showString "Tensor " . showsPrec 0 a
>       showsPrec n (S a)     = showsPrec n a

>       showsPrec 0 (T xs)    = showString "Tensor " . showList' 0 xs
>       showsPrec n (T xs)    = showList' n xs

> showList' :: (Show t) => Int -> [t] -> String -> String
> showList' _ [] = showString "[]"
> showList' n (x:xs) = showChar '[' . showsPrec (n+1) x . showRem (n+1) xs
>       where
>               showRem _ [] = showChar ']'
>               showRem o (y:ys) = showChar ',' . showsPrec o y . showRem o ys

</pre>
<p>
<hr>
<p>
<b>
        Input
</b>
<p>

        Although tensors are printed as structured list
        it is easier to input data via flat lists.
        But make sure that the length of the list is one
        of: dims^0, dims^1, dims^2, dims^3, dims^4, etc.
<p>
        This function is quite inefficient for ranks higher than 4.
        Compare, for example, timings of:
<pre>
        tensor [1..3^6]
        tensor [1..3^3] * tensor [1..3^3]
</pre>
        Although both expressions create tensors of the same rank 6,
        but the execution of the latter is much faster. This is
        because the function <code>tensor</code> spends much
        of its effort on recursively restructuring the flat lists
        into the lists-of-lists-of-lists...
<pre>


> tensor :: [Double] -> Tensor
> tensor xs
>       | size == 1 = S (head xs)
>       | q /= 0    = error "Length is not a power of dims"
>       | otherwise = T (tlist p xs)
>       where
>           (p,q) = rnk 1 (quotRem size dims)
>           rnk m (1, v) = (m, v)
>           rnk m (u, 0) = rnk (m+1) (quotRem u dims)
>           rnk m (_, v) = (m, v)
>           size   = length xs
>           group n ys = group' n ys [] where
>               group' o zs as
>                   | length zs == 0 = reverse as
>                   | length zs < o  = reverse (zs:as)
>                   | otherwise      = group' o (drop o zs) ((take o zs):as)
>
>           tlist :: Int -> [Double] -> [Tensor]
>           tlist 1 zs   = map S zs
>           tlist rnl zs = tlist' (rnl-1) (map S zs)
>               where
>                   tlist' 0 fs = fs
>                   tlist' o fs = tlist' (o-1) $ map T $ group dims fs

</pre>
<p>
<hr>
<p>
<b>
        Extraction and conversion
</b>
<p>

        Tensor components are also tensors and can be extracted
        via (#) operator

<pre>

> ( # ) :: Tensor -> Int -> Tensor
> (S a1) # 1  = S a1
> (S _) # _  = error "out of range"
> (T xs) # i  = xs!!(i-1)

> ( ## ) :: Tensor -> [Int] -> Tensor
> a ## [] = a
> a ## (x:xs) = (a#x) ## xs

</pre>

        Tensors of rank 0 can be converted to scalars; i.e.,
        simple numbers of type Double.
<pre>

> scalar :: Tensor -> Double
> scalar (S a)  = a
> scalar (T _) = error "rank not 0"

</pre>

        Tensors of rank 1 can be converted to vectors; i.e.,
        lists with "dims" components of type Double
<pre>

> vector :: Tensor -> [Double]
> vector (S _)         = error "rank not 1"
> vector a@(T xs)
>       | rank a /= 1  = error "rank not 1"
>       | otherwise    = map scalar xs

</pre>
<p>
<hr>
<p>
<b>
        Useful tensors: epsilon and delta
</b>
<p>
        Function "epsilon' i j k" emulates values of the pseudo-tensor Eijk.
        It is valid only for three-dimensional tensors.
        It takes three indices i,j,k from the range (1,3)
        and returns one of the three values:
        0.0, 1.0, -1.0 -- depending on the rules specified below:
<pre>

> epsilon' :: Int -> Int -> Int -> Double
> epsilon' i j k
>       | dims /= 3 = error "not 3-dims"
>       | outside (1,3) i j k = error "Not in range"
>       | (i == j) || (i == k) || (j == k)   =  0
>       | otherwise = epsilon1 i j k
>       where
>               epsilon1 m n o
>                       | (m == 1) && (n == 2) && (o == 3)   =  1
>                       | (m == 3) && (n == 2) && (o == 1)   = -1
>                       | otherwise = epsilon1 n o m
>               outside (p,q) a b c =
>                       (not $ inRange (p,q) a) ||
>                       (not $ inRange (p,q) b) ||
>                       (not $ inRange (p,q) c)

</pre>
        Function "delta' i j" emulates Kronecker's delta:
<pre>

> delta' :: Int -> Int -> Double
> delta' i j
>       | i == j    = 1
>       | otherwise = 0

</pre>

        Delta' and epsilon' can be converted to tensors

<pre>

> delta, epsilon :: Tensor
> delta   = tensor [delta' i j     | i <- [1..dims], j <- [1..dims]]
> epsilon = tensor [epsilon' i j k | i <- [1..3], j <- [1..3], k <- [1..3]]

</pre>
        The components delta[ij] and epsilon[i,j,k] can be extracted
        and converted to numbers. For example:
<pre>
        scalar (epsilon#1#2#3) = 1
        scalar (epsilon#1#1#3) = 0,
        scalar (epsilon#3#2#1) = -1
</pre>
<p>
<hr>
<p>
<b>
        Dot product
</b>
<p>
        Dot product of two tensors of rank 1 could be defined as
        tensor of rank 0. This is not the most efficient implementation
        but we still want the dot product to be recognised as
        tensor, so we loose on speed here:
<pre>

> dot :: Tensor -> Tensor -> Tensor
> dot a b = S (sum [scalar (a#i) * scalar (b#i) | i <- [1..dims]])

</pre>

<p>
<hr>
<p>
<b>
        Cross product - valid for 3D space only
</b>
<p>
        The cross product of two vectors is another vector:
        C = A x B. The pseudotensor Eijk is used to compute
        such cross product.
<p>
        First, here are numerical components of C, C[i]:
<pre>

> cross'       :: Tensor -> Tensor -> Int -> Double
> cross' a b i = sum [(epsilon' i j k)* scalar (a#j) * scalar (b#k)|
>                       j<-[1..3],k<-[1..3], j/=k]

</pre>
        And here is the full vector C (as tensor of rank 1):

<pre>

> cross     :: Tensor -> Tensor -> Tensor
> cross a b = tensor (map (cross' a b) [1..3])

</pre>

        Example:
<pre>
        cross (tensor [1..3]) (tensor [1,8,1]) ==> Tensor [-22.0, 2.0, 6.0]
</pre>

<p>
<hr>
<p>
<b>
        Equality of tensors
</b>
<p>
        Tensor can be admitted to class <code>Eq</code>. We only need to
        define either equality or nonequality operation. We've chosen
        to define the former: two tensors are equal if they have the same
        rank and equal components:
<pre>

> instance Eq Tensor where
>       (==) a b
>               | ranka /= rank b = False
>               | ranka == 0      = scalar a == scalar b
>               | otherwise       = and [(a#i) == (b#i) | i <- [1..dims]]
>               where
>                       ranka = rank a
>

</pre>


<p>
<hr>
<p>
<b>
        Tensor as instance of class Num
</b>
<p>
        To admit tensors to class <code>Num</code> we have to
        support all the operations from that class. Here is
        the class Num declaration taken from the Prelude:
<pre>
class (Eq a, Show a) => Num a where
    (+), (-), (*)  :: a -> a -> a
    negate         :: a -> a
    abs, signum    :: a -> a
    fromInteger    :: Integer -> a

    -- Minimal complete definition: All, except negate or (-)
    x - y           = x + negate y
    negate x        = 0 - x
</pre>
        All operations but <code>(*)</code> are straightforward,
        meaningful and easy to implement. The semantics of multiplication
        <code>(*)</code> is, however, not so obvious and it is up to us
        how to define it: as an inner product or as an outer
        product. We have chosen the latter, which means that the
        operation <code>c = a * b</code> produces a new tensor <code>c</code>
        whose rank is a sum of the ranks of tensors being
        multiplied:
<pre>
        rank c = rank a + rank b
</pre>

        Suffice to add that tensor products are generally not
        commutative; that is:
<pre>
        a * b /= b * a

</pre>
        That said, here is the instantiation of <code>Num</code>
        for datatype Tensor:
<pre>

> instance Num Tensor where
>       (+) a b
>               | ranka /= rank b = error "different ranks"
>               | ranka == 0      = S (scalar a  + scalar b)
>               | otherwise       = T [a#i + b#i | i <- [1..dims]]
>               where
>                       ranka = rank a

>       negate (S a1)           = S (negate a1)
>       negate (T xs)           = T (map negate xs)

>       abs (S a1)              = S (abs a1)
>       abs (T xs)              = T (map abs xs)

>       signum (S a1)           = S (signum a1)
>       signum (T xs)           = T (map signum xs)

>       fromInteger n             = S (fromInteger n)

>       (*) (S a1) (S b1)     = S (a1*b1)
>       (*) a@(S _) (T xs)     = T (map (a*) (take dims xs))
>       (*) (T xs) b            = T (map (*b) (take dims xs))

</pre>
        Having defined the operation <code>(*)</code> as an outer product
        such operation will generally increase the rank of the outcome.
        For example, if <code>a</code> is a tensor of rank 2 (matrix) and
        <code>b</code> is a tensor of rank 1 (vector) then the result is
        a tensor of rank 3:
<pre>
        c = a * b, that is
        c[ijk] = a[ij] b[k]
</pre>
        But this is not what is typically considered a multiplication
        of tensors; we are more often than not interested in the inner
        products, informally described below.

<p>
<hr>
<p>
<b>
        Contraction
</b>
<p>
<p>
        Eistein's indexing convention of tensors is based on
        the distinction between free indices and bound indices.
        Free indices appear in the tensorial expressions, such
        as <code>A[ijkl]</code>, once only and they indicate
        a freedom for substitution of any specific index
        from the range of valid indices. This range is (1,3)
        for 3D tensors. The expression <code>A[ijkl]</code>
        represents in fact one of 3^4 possible components
        of the tensor <code>A</code>.
<p>
        Bound indices, on the other hand, appear in pairs
        (and only in pairs) and they indicate the summation of
        tensor expression over the valid range. For example,
<pre>
        A[kkj] = A[11j] + A[22j] + A[33j]
</pre>
        Note that the index "j" is still free, and that means
        that the above represents three equations for j = 1,2,3.
<p>
        A process of converting of a pair of free indices
        to a pair of bound indices is called contraction. As
        a result a rank of a tensor (or expression involving
        several tensors) is being reduced
        by two.
<p>
        The function <code>contract</code> below accepts a tensor of a
        rank bigger or equal 2 and two integers m,n from the range (1,rank a)
        which indicate positions of the two indices to be used for
        contraction. The result is a tensor with its rank reduced
        by two.

<pre>


> contract :: Int -> Int -> Tensor -> Tensor
> contract m n a
>    | m >= n      = error "wrong ordering"
>    | outside m n = error "not in range"
>    | ranka <  2  = error "cannot contract"
>    | ranka == 2  = S (sum [scalar (a#i#i) | i <- [1..dims]])
>    | ranka >  2  = tensor [summa m n us a | us <- freeIndices (ranka-2)]
>    where
>        ranka = rank a
>
>        outside p q = (not $ inRange (1,ranka) p)
>                            ||(not $ inRange (1,ranka) q)
>        summa p q xs b = sum [scalar (b##(insert p q xs r)) |
>               r <- [1..dims]]

>        -- Insert element r at positions m n to the list
>        -- of indices xs
>        insert o p xs r = us++[r]++ws++[r]++zs
>               where
>                       (us,vs) = splitAt (o-1) xs
>                       (ws,zs) = splitAt (p - o - 1) vs
>
>        freeIndices 1 = [[x] | x <- [1..dims]]
>        freeIndices o = [x:y | x <- [1..dims], y <- freeIndices (o-1)]


</pre>

        Let's take for example tensor <code>delta</code> and contract
        it in its two indices:
<pre>
        delta [kk] = delta[1,1] + delta[2,2] + delta[3,3] = 1 + 1 + 1 = 3
</pre>
        The same can be done in Haskell:
<pre>
        contract 1 2 delta        ==> Tensor 3.0
        rank (contract 1 2 delta) ==> 0
</pre>


<p>
<hr>
<p>
<b>
        Inner product
</b>
<p>
        The inner product of two tensors can be considered
        as two-phase process: first the outer product is
        formed and then a contraction is applied to a selected
        pair of indices. There are countless possibilities
        of defining such inner products, since we can choose
        any pair, or even more than one pair, of indices
        to become bound.
<p>
        How do we usually multiply tensors? Here is one example,
        which is equivalent to matrix-vector multiplication:
<pre>
        C[i] = A[ij] B[j]
</pre>
        Notice two types of indices: index "i" is free since
        it appears only once on both sides of the equation. It means
        that you can freely substitute 1,2 or 3 for "i". So in fact
        we have here three equations:
<pre>
        C[1] = A[1j] B[j]
        C[2] = A[2j] B[j]
        C[3] = A[3j] B[j]
</pre>
        Index "j" is bound - it appears two times on the right hand
        side, but not on the left side. Bound indices signify summation
        from 1 to 3. So the above in fact means:
<pre>
        C[1] = A[11] B[1] + A[12] B[2] + A[13] B[3]
        C[2] = A[21] B[1] + A[22] B[2] + A[23] B[3]
        C[3] = A[31] B[1] + A[32] B[2] + A[33] B[3]
</pre>
        The economy of notation is evident in our first form above.
        How will we do it in Haskell?
<p>
        To obtain the above result we will first form the outer product
        of matrix A and vector B, obtain a tensor of rank 3,
        and then contract it in indices 2 and 3 to obtain a
        the final expected result (inner product):
<pre>
        c = contract 2 3 (a * b)
</pre>
        This approach is quite inefficient storage-wise and
        speed-wise and a direct customized encoding which avoids creating
        outer products is recommended instead.
<p>
        The system of equations
<pre>
        C[i] = A[ij] B[j]
</pre>
        could obviously be represented explicite as:
<pre>
        c i = sum [scalar(a#i#j) * scalar(b#j) | j <- [1..dims]]
        -- valid for i = 1..dims
</pre>
        But when efficiency is not a premium we could still
        take advantage of function <code>contract</code>
        to write clear code that avoids the explicit sums. The
        operator <code> &lt;*&gt;</code>, introduced below, allows
        us to write the same function as:
<pre>
        c      = a <*> b              -- the output is a tensor of rank 1
        c'  i  = (a <*> b)#i          -- the output is a tensor of rank 0
        c'' i  = scalar ((a <*> b)#i) -- the output is a number
</pre>

<p>
<hr>
<p>
<b>
        Convenience operators for inner products
</b>
<p>
        Variety of specialized functions for inner products
        could be defined. We will show few examples here
        and introduce specialized convenience operators
        for most common types of inner products. Please
        note that the proposed operators are not standard
        in any way, and we are not trying to suggest that
        they are important. Just treat them as examples.
<p>
        The semantics of operator <code> <*> </code> has
        been chosen to support matrix-vector or vector-matrix
        multiplications. But this operator is more general
        than that, because it also handles products with scalars
        (tensors of rank 0), and generally any products
        of any two tensors with bounds imposed on one pair
        of indices: last index of the first tensor and first
        index of the second tensor.

<pre>

> (<*>) :: Tensor -> Tensor -> Tensor
> a <*> b
>       | (ranka == 0) || (rankb == 0) = a * b
>       | otherwise = contract ranka (ranka + 1) (a * b)
>       where
>               ranka = rank a
>               rankb = rank b

</pre>

        Take for example a classical identity:
<pre>
        A[i] = delta[ij] B[j], where delta is a Kronecker's delta
</pre>
        Here is an example of how we can use it in Haskell:
<pre>
        delta <*> tensor [4,5,6])    ==> Tensor [4.0, 5.0, 6.0]
        (delta <*> tensor [4,5,6])#1 ==> Tensor 4.0
</pre>

        Let's try something more complex, for example a constitutive equation
        relating the stress tensor S[ij] with the deformation tensor G[kl].
        The tensor C[ijkl] is an anisotropic tensor of material constants:
        81 altogether. In fact, due to all sorts of symmetries this number
        could be reduced to twenty-something for the most complex crystals,
        and to two independent components for the isotropic materials.
        Anyway, the relation is linear and can be written as follows:
<pre>
        S[ij] = C[ijkl] G[kl]
</pre>
        This represents 9 equations (i,j->1,2,3) and expands heavily
        to sums over k and l on the right-hand side.
        We need to impose two bounds in two pairs of indices to
        support above example. Here is another specialized operator
        for inner product with two specificly selected bounds.
<pre>

> (<<*>>) :: Tensor -> Tensor -> Tensor
> a <<*>> b
>       | (ranka < 2) || (rankb < 2) = error "rank too small"
>       | otherwise = contract (ranka-1) ranka
>               (contract ranka (ranka+2) (a * b))
>       where
>               ranka = rank a
>               rankb = rank b

</pre>
        Here is a dummy, but easy to generate example of the above:

<pre>
        tensor [1..81] <<*>> tensor [1..9]

                ==> s = Tensor [[ 285.0,  690.0, 1095.0],
                                [1500.0, 1905.0, 2310.0],
                                [2715.0, 3120.0, 3525.0]]

        (tensor [1..81] <<*>> tensor [1..9])#1#1 = Tensor 285.0
</pre>
<p>
<hr>
<p>
<b>
        Double cross products
</b>
<p>
        Here is another useful example of tensor multiplication.
        Say you want to compute a cross product of three vectors:
<pre>
        D = C X (A x B )
</pre>
        In index notation this could be expressed as:
<pre>
        D[i] = E[ijk] C[j] E[kpq] A[p] B[q]
</pre>
        This represents three equations for i=1,2,3. All other indices
        j,k,p,q are bound; that is, they appear in pairs on the right
        hand side, indicating four sums. Although you can calculate
        it directly, and this Haskell module can do it easily, we can
        simplify this equation by organizing it differently and
        using this identity:
<pre>
        E[ijk] = E[kij]
</pre>
        (Even permutation of indices does not change a sign of pseudo-tensor
        E.)
<pre>
        D[i] = E[kij] E[kpq] C[j] A[p] B[q]
</pre>
        Now here is another useful identity, which gets rid of the
        bound index "k" (sitting in the first position above):
<pre>
        E[kij] E[kpq] = delta[ip] delta[jq] - delta[iq] delta[jp]
</pre>
        After substitution and using identity <code>delta[ij] G[j] = G[i]</code>
        the <code>C x (A x B)</code> transforms to:
<pre>
        D[i] = C[j] B[j] A[i] - C[j] A[j] B[i]
</pre>
        We still have three scalar equations, but they are less complex:
        there is only one summation (over the "j") on the right hand side.
<p>
        You should easily recognize that <code>C[j] B[j]</code>
        represents the scalar product. Therefore our double cross product
        can be represented as a difference of two vectors:
<pre>
        D = C x (A x B) = (C o B) A - (C o A) B
</pre>

        Now, let us see how this module handles this. Let's take an
        example of three randomly chosen vectors A, B, C. The direct
        method is straightforward, although it involves quite a lot
        of multiplications and summations (which would not be so
        evident if we have not done all those preliminary examinations
        above).
<pre>

> d_standard :: Tensor
> d_standard  = cross c (cross a b) where
>       a = tensor [1,2,3]
>       b = tensor [3,1,8]
>       c = tensor [5,2,4]

</pre>
        On the other hand we could encode the equivalent equation:
<pre>
        D = (C o B) A - (C o A) B
</pre>
        as:
<pre>

> d_simpler :: Tensor
> d_simpler =
>       tensor [n1 * scalar (a#i) - n2 * scalar (b#i) | i <- [1..dims]] where
>
>               a = tensor [1,2,3]
>               b = tensor [3,1,8]
>               c = tensor [5,2,4]
>               n1 = scalar (c `dot` b)
>               n2 = scalar (c `dot` a)

</pre>

        Both <code>d_standard</code> and <code>d_simpler</code>
        lead to the same result:
<pre>
        ==> Tensor [-14.0, 77.0, -21.0]
</pre>
<p>
<hr>
<p>
<b>
        Vector transformation
</b>
<p>
        A vector can be decomposed in any system of reference. The best
        choice is any orthogonal system of reference, where all base
        unit vectors are mutually perpendicular (orthogonal), since this
        simplifies the computations. The base vectors <code>e[1], e[2], e[3]</code>
        are usually chosen as vectors of length one (we say that they are
        normalized to one), and hence they are called "orthonormal".
        They obey the orthonormality relations for their scalar products:
<pre>
        e[i] o e[j] = delta[ij]
</pre>
        where the Kronecker's "delta" has been defined before.
<p>
        Here is an example of the vector decomposition:
<pre>
        A = A[i] e[i]     (summation over "i"!)
</pre>
        The components A[i] of the vector A obviously depend on the choice
        of the base system. The same vector A will have different
        components in two different systems of references:
<pre>
        A'[i] e'[i] = A[i] e[i]
</pre>
        where primes refer to the new system. Now, if we multiply both
        sides of the above equation by a base vector <code>e'[k]</code>,
        using the scalar (dot) product definition, we will get:
<pre>
        A'[i] e'[k] o e'[i] = A[i] e'[k] o e[i]
</pre>
        The new base vectors are mutually orthonormal, so
<pre>
        e'[k] o e'[i] = delta[ki]
</pre>
        and the left hand side will be transformed to:
<pre>
        A'[i] delta[ki] = A'[k]
</pre>
        But the base vectors on the right hand side are taken from
        two different systems, and therefore they are not mutually
        orthonormal. All such nine scalar products form the components of the
        transormation tensor, R:
<pre>
        R[ki] = e'[k] o e[i]
</pre>
        As a result, our original equation can be expressed as
        a new equation defining transformation of the vector A:
<pre>
        A'[k] = R[ki] A[i]
</pre>
        This gives us a rule how to compute new components A'[k] of vector
        A from its old components and transformation tensor R[ki].
<p>
        You might want to run some exercise choosing the old
        system with the base vectors:
<pre>
        e#1=tensor [1,0,0]
        e#2=tensor [0,1,0]
        e#3=tensor [0,0,1],
</pre>
        where "e" can be considered a tensor of rank 2:
<pre>
        e = tensor [1,0,0,
                    0,1,0,
                    0,0,1]
</pre>
        and the new system obtained from the old one by rotation
        around the axis 3 (x3, or z) by an angle "alpha". Some
        trigonometry will be involved to compute the new base
        vectors, e'[i]. The next step is to compute tensor R[ki]
<pre>

        r     = tensor [scalar (e'#k `dot` e#i)|k<-[1..dims], i<-[1..dims]]

</pre>
        and finally use operator <code> <*></code> to compute new components
        of vector A:
<pre>
        a' = r <*> a
</pre>
<p>
<hr>
<p>
        Related page on this site:
        <a href="http://www.numeric-quest.com/haskell/index.html">
        Collection of Haskell modules</a>

<pre>
-----------------------------------------------------------------------------
--
-- Copyright:
--
--      (C) 1999 Numeric Quest Inc., All rights reserved
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

// FILE ARCHIVED ON 20010630021753 AND RETRIEVED FROM THE
// INTERNET ARCHIVE ON 20030626102305.
// JAVASCRIPT APPENDED BY WAYBACK MACHINE, COPYRIGHT INTERNET ARCHIVE.
// ALL OTHER CONTENT MAY ALSO BE PROTECTED BY COPYRIGHT (17 U.S.C.
// SECTION 108(a)(3)).

   var sWayBackCGI = "http://web.archive.org/web/20010630021753/";

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
