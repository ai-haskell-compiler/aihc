{-# LANGUAGE TemplateHaskell, FunctionalDependencies, MultiParamTypeClasses #-}
{-# OPTIONS -Wall #-}

-- | Note: One-tuples are currently understood as just the original type by Template Haskell
-- (though this could be an undefined case which is not guaranteed to work this way?), so for example, we get
--
-- @ $('catTuples' 1 2) = \\x (y,z) -> (x,y,z) @
module TupleTH(
    -- * Transformation
        mapTuple, mapTuple', filterTuple, filterTuple', reindexTuple, reverseTuple, rotateTuple, subtuples, deleteAtTuple, takeTuple, dropTuple, safeDeleteTuple, updateAtN,
    -- * Combination
        zipTuple, catTuples, uncatTuple, splitTupleAt,
    -- ** ZipWith
        zipTupleWith, zipTupleWith',
    -- * Construction
        safeTupleFromList, tupleFromList, constTuple,
    -- * Deconstruction
        proj, proj', elemTuple, tupleToList, sumTuple,
        findSuccessiveElementsSatisfying,
    -- ** Right folds
        foldrTuple, foldrTuple',
        foldr1Tuple, foldr1Tuple',
    -- ** Left folds
        foldlTuple, foldlTuple',
        foldl1Tuple, foldl1Tuple',
    -- ** Predicates
        andTuple, orTuple,
        anyTuple, anyTuple',
        allTuple, allTuple',
    -- * Monadic/applicative
        sequenceTuple, sequenceATuple,
    -- * Types
        htuple,
    ) where

import Control.Applicative ( Applicative((<*>), pure) )
import Control.Exception ( assert )
import Control.Monad
import Data.Functor((<$>))
import Data.Maybe(fromMaybe)
import Data.Set(member)
import Language.Haskell.TH
import qualified Data.Set as Set
import Data.List


-- | Makes a homogenous tuple type of the given size and element type
--
-- > $(htuple 2) [t| Char |] = (Char,Char)
htuple ::  Int -> TypeQ -> TypeQ
htuple n t = foldl appT (tupleT n) (replicate n t)


withxs ::  Int -> (PatQ -> [ExpQ] -> Q b) -> Q b
withxs = withNames "_x"
withys ::  Int -> (PatQ -> [ExpQ] -> Q b) -> Q b
withys = withNames "_y"

newNames ::  String -> Int -> Q [Name]
newNames stem n = sequence [newName (stem++show i) | i <- [ 1::Int .. n ]]

withNames :: String -> Int -> (PatQ -> [ExpQ] -> Q a) -> Q a
withNames stem n body = withNames' stem n (body . tupP)

withNames' :: String -> Int -> ([PatQ] -> [ExpQ] -> Q a) -> Q a
withNames' _ n _ | n < 0 = fail ("Negative tuple size: "++show n)
withNames' stem n body = do
    names <- newNames stem n
    body (fmap varP names) (fmap varE names)


withNames2
  :: String
     -> String
     -> Int
     -> (PatQ -> [ExpQ] -> PatQ -> [ExpQ] -> Q a)
     -> Q a
withNames2 stem1 stem2 n body =
    withNames stem1 n (\xsp xes -> withNames stem2 n (body xsp xes))


appE2 ::  ExpQ -> ExpQ -> ExpQ -> ExpQ
appE2 f x y = f `appE` x `appE` y

-- | Converts an expression-level function to a function expression
liftExpFun ::  String -> (ExpQ -> ExpQ) -> Q Exp
liftExpFun argNameStem f = do
    argName <- newName argNameStem
    lam1E (varP argName) (f (varE argName))



-- | Like 'zip'.
--
-- Type of the generated expression:
--
-- > (a1, a2, ..) -> (b1, b2, ..) -> ((a1,b1), (a2,b2), ..)
zipTuple ::  Int -> Q Exp
zipTuple n = zipTupleWith' n (conE (tupleDataName 2))

-- | Like 'zipWith'.
--
-- Type of the generated expression:
--
-- > (a -> b -> c) -> (a, ..) -> (b, ..) -> (c, ..)
zipTupleWith ::  Int -> ExpQ
zipTupleWith n = liftExpFun "f" (zipTupleWith' n)


-- | Takes the zipping function as a quoted expression. See 'mapTuple'' for how this can be useful.
zipTupleWith' :: Int -> ExpQ -> ExpQ
zipTupleWith' n f =
    withNames2 "x" "y" n
        (\xsp xes ysp yes ->
            lamE [xsp,ysp] (tupE (zipWith (appE2 f) xes yes)))



-- | > Generate a projection (like 'fst' and 'snd').
proj ::  Int -- ^ Size of tuple
      -> Int -- ^ 0-based index of component to retrieve
      -> ExpQ
proj n i = do
    x <- newName "_x"
    lam1E (tupP (replicate i wildP ++ [ varP x ] ++ replicate (n-i-1) wildP)) (varE x)

-- | Type of the generated expression:
--
-- > (a -> r -> r) -> r -> (a, ..) -> r
foldrTuple ::  Int -> ExpQ
foldrTuple n = liftExpFun "c" (foldrTuple' n)

-- | Takes the folding function (but not the seed element) as a quoted expression. See 'mapTuple'' for how this can be useful.
foldrTuple' :: Int -> ExpQ -> ExpQ
foldrTuple' n c = do
    z <- newName "_z"
    withxs n (\xsp xes -> lamE [varP z, xsp] (foldr (appE2 c) (varE z) xes))

-- | Type of the generated expression:
--
-- > (a -> a -> a) -> (a, ..) -> a
foldr1Tuple ::  Int -> ExpQ
foldr1Tuple n = liftExpFun "c" (foldr1Tuple' n)



-- | Takes the folding function as a quoted expression. See 'mapTuple'' for how this can be useful.
foldr1Tuple' ::  Int -> ExpQ -> Q Exp
foldr1Tuple' n c = withxs n (\xsp xes -> lam1E xsp (foldr1 (appE2 c) xes))

-- | Type of the generated expression:
--
-- > (r -> a -> r) -> r -> (a, ..) -> r
foldlTuple ::  Int -> ExpQ
foldlTuple n = liftExpFun "c" (foldlTuple' n)


-- | Takes the folding function (but not the seed element) as a quoted expression. See 'mapTuple'' for how this can be useful.
foldlTuple' :: Int -> ExpQ -> ExpQ
foldlTuple' n c = do
    z <- newName "_z"
    withxs n (\xsp xes -> lamE [varP z, xsp] (foldl (appE2 c) (varE z) xes))

-- | Type of the generated expression:
--
-- > (a -> a -> a) -> (a, ..) -> a
foldl1Tuple ::  Int -> ExpQ
foldl1Tuple n = liftExpFun "c" (foldl1Tuple' n)


-- | Takes the folding function as a quoted expression. See 'mapTuple'' for how this can be useful.
foldl1Tuple' ::  Int -> ExpQ -> Q Exp
foldl1Tuple' n c = withxs n (\xsp xes -> lam1E xsp (foldl1 (appE2 c) xes))

-- | Type of the generated expression:
--
-- > (a -> Bool) -> (a, ..) -> [a]
filterTuple ::  Int -> ExpQ
filterTuple n = liftExpFun "p" (filterTuple' n)


-- | Takes the predicate as a quoted expression. See 'mapTuple'' for how this can be useful.
filterTuple' ::  Int -> ExpQ -> ExpQ
filterTuple' n p = withxs n (\xsp xes -> lamE [xsp] (go xes))
    where
        go []       = [| [] |]
        go [x]      = [| if $(p) $(x) then [$(x)] else [] |]
        go (x:xs)   = [| (if $(p) $(x) then ($(x) :) else id) $(go xs) |]



-- | Type of the generated expression:
--
-- > (a -> b) -> (a, ..) -> (b, ..)
mapTuple :: Int -> ExpQ
mapTuple n = liftExpFun "f" (mapTuple' n)


-- | Takes the mapping as a quoted expression. This can sometimes produce an expression that typechecks when the analogous expression using 'filterTuple' does not, e.g.:
--
-- > $(mapTuple 2) Just        ((),"foo") -- Type error
-- > $(mapTuple' 2 [| Just |]) ((),"foo") -- OK
mapTuple' ::  Int -> ExpQ -> Q Exp
mapTuple' n f = withxs n (\xsp xes ->
        lamE [xsp] (tupE [f `appE` x  | x <- xes ]))


-- | Simple 'match'
smatch ::  PatQ -> ExpQ -> MatchQ
smatch p e = match p (normalB e) []

-- | Type of the generated expression:
--
-- > [a] -> Maybe (a, ..)
safeTupleFromList ::  Int -> Q Exp
safeTupleFromList n = do
    xns <- newNames "_x" n
    let xps = varP <$> xns
        xes = varE <$> xns
    xs <- newName "_xs"
    lam1E (varP xs) (caseE (varE xs)
                       [ smatch (listP xps) (conE 'Just `appE` (tupE xes))
                       , smatch wildP (conE 'Nothing)
                       ])


-- | Type of the generated expression:
--
-- > [a] -> (a, ..)
--
-- The generated function is partial.
tupleFromList ::  Int -> Q Exp
tupleFromList n = [| \xs0 -> fromMaybe (error (msg ++ show (length xs0))) ( $(safeTupleFromList n) xs0 ) |]
    where
        msg = "tupleFromList "++show n++" called on a list of length "



-- | Like 'or'.
orTuple ::  Int -> Q Exp
orTuple 0 = [| False |]
orTuple n = foldl1Tuple' n [| (||) |]

-- | Like 'and'.
andTuple ::  Int -> Q Exp
andTuple 0 = [| True |]
andTuple n = foldl1Tuple' n [| (&&) |]

-- | Like 'any'.
anyTuple ::  Int -> Q Exp
anyTuple n = liftExpFun "p" (anyTuple' n)

-- | Like 'all'.
allTuple ::  Int -> Q Exp
allTuple n = liftExpFun "p" (allTuple' n)

anyTuple' ::  Int -> Q Exp -> Q Exp
anyTuple' n p = [| $(orTuple n) . $(mapTuple' n p) |]

allTuple' ::  Int -> Q Exp -> Q Exp
allTuple' n p = [| $(andTuple n) . $(mapTuple' n p) |]

-- | Like 'elem'.
--
-- Type of generated expression:
--
-- > Eq a => a -> (a, ..) -> Bool
elemTuple ::  Int -> Q Exp
elemTuple n = do
    z <- newName "_z"
    lam1E (varP z) (anyTuple' n [| (== $(varE z)) |])


tupleToList ::  Int -> Q Exp
tupleToList n = [| $(foldrTuple' n (conE '(:))) [] |]


-- | Type of the generated expression:
--
-- > (a1, ..) -> (b1, ..) -> (a1, .., b1, ..)
catTuples :: Int -> Int -> Q Exp
catTuples n m = withxs n (\xsp xes -> withys m (\ysp yes ->
    lamE [xsp,ysp] (tupE (xes ++ yes))))

-- | @uncatTuple n m = 'splitTupleAt' (n+m) n@
--
-- @uncatTuple n m@ is the inverse function of @uncurry (catTuples n m)@.
uncatTuple :: Int -> Int -> Q Exp
uncatTuple n m = splitTupleAt (n+m) n

-- | @splitTupleAt n i@ => @\\(x_0, ..., x_{n-1}) -> ((x_0, ..., x_{i-1}),(x_i, ..., x_{n-1})@
splitTupleAt :: Int -> Int -> Q Exp
splitTupleAt n i =
 withxs n (\xsp xes ->
    case splitAt i xes of
         (l,r) -> lam1E xsp (tupE [tupE l, tupE r]))



-- | @reindexTuple n js@ =>
--
-- > \(x_0, ..., x_{n-1}) -> (x_{js !! 0}, x_{js !! 1}, ... x_{last js})
--
-- For example,
--
-- > $(reindexTuple 3 [1,1,0,0]) ('a','b','c') == ('b','b','a','a')
--
-- Each element of @js@ must be nonnegative and less than @n@.
reindexTuple :: Int -> [Int] -> Q Exp
reindexTuple n is = withNames' "x" n (\xps xes ->
    let
        iset = Set.fromList is
        xsp' = fmap (\(p,i) -> if i `member` iset then p else wildP)
                    (zip xps [0..])

    in
        lam1E (tupP xsp') (tupE (fmap (xes !!) is)))


-- | Like 'reverse'.
reverseTuple ::  Int -> Q Exp
reverseTuple n = reindexTuple n (reverse [0..n-1])

-- | @rotateTuple n k@ creates a function which rotates an @n@-tuple rightwards by @k@ positions (@k@ may be negative or greater than @n-1@).
rotateTuple ::  Int -> Int -> Q Exp
rotateTuple n k = reindexTuple n (fmap (`mod` n) [n-k, n-k+1 .. 2*n-k-1])


sumTuple ::  Int -> Q Exp
sumTuple 0 = litE (integerL 0)
sumTuple n = foldl1Tuple' n (varE '(+))

constTuple ::  Int -> Q Exp
constTuple n = reindexTuple 1 (replicate n 0)

-- | Like 'sequence'.
sequenceTuple ::  Int -> Q Exp
sequenceTuple 0 = [| return () |]
sequenceTuple 1 = [| id :: Monad m => m a -> m a |]
sequenceTuple n =
    withxs n (\xsp xes ->
        lam1E xsp (foldl (\x y -> [| $(x) `ap` $(y) |])
                         [| $(conE $ tupleDataName n) `liftM` $(head xes) |]
                         (tail xes)))

-- | Like 'sequenceA'.
sequenceATuple ::  Int -> Q Exp
sequenceATuple 0 = [| pure () |]
sequenceATuple 1 = [| id :: Applicative f => f a -> f a |]
sequenceATuple n =
    withxs n (\xsp xes ->
        lam1E xsp (foldl (\x y -> [| $(x) <*> $(y) |])
                         [| $(conE $ tupleDataName n) <$> $(head xes) |]
                         (tail xes)))

descendingMultiindices :: Int -> Int -> [[Int]]
descendingMultiindices _ 0 = [[]]
descendingMultiindices n 1 = fmap (:[]) [0..n-1]
descendingMultiindices n k | k < 0 = error ("Internal error in tuple-th: descendingMultiindices "++show n++" "++show k)
descendingMultiindices n k = [ i:is | is <- descendingMultiindices (n-1) (k-1),
                                      i <- [head is+1,head is+2 .. n-1] ]


-- | Generates the function which maps a tuple @(x_1, ..., x_n)@ to the tuple of all its subtuples of the form @(x_{i_1}, ..., x_{i_k})@, where @i_1 < i_2 < ... < i_k@.
subtuples :: Int -> Int -> Q Exp
subtuples n k = withxs n (\xsp xes ->
    let
        subtupleE :: [Int] -> ExpQ
        subtupleE = tupE . fmap (xes !!)
    in
        lam1E xsp (tupE (fmap (subtupleE . reverse) (descendingMultiindices n k))))

-- class Tuple as a | as -> a where
--     filterTuple :: (a -> Bool) -> as -> [a]
--
-- class MapTuple as a bs b | as -> a, bs -> b where
--     mapTuple :: (a -> b) -> as -> bs

-- mkTuple :: Int -> DecsQ
-- mkTuple n = do
--   let a = varT (mkName "a")
--
--
--   sequence
--     [ instanceD (cxt []) (conT ''Tuple `appT` ht n a `appT` a)
--                 [valD (varP 'filterTuple) (normalB (filterTuple n)) []]
--     ]
--


-- | Generates a function which takes a 'Num' @i@ and a homogenous tuple of size @n@ and deletes the @i@-th (0-based) element of the tuple.
deleteAtTuple :: Int -> Q Exp
deleteAtTuple n = do
    i <- newName "_i"
    lam1E (varP i) $
        withxs n (\xsp xes ->

                let
                    matches0 = [ match
                                    (litP (integerL j))
                                    (normalB . tupE . deleteAt j $ xes)
                                    []
                                | j <- [0 .. fromIntegral n -1] ]

                    errmsg1 = "deleteAtTuple "++show n++" "
                    errmsg2 = ": index out of bounds"

                    matches = matches0 ++ [
                                match wildP (normalB
                                    [| error (errmsg1 ++ show $(varE i) ++ errmsg2) |])
                                            [] ]
                in
                    lam1E xsp $ caseE (varE i) matches)


    where
        deleteAt 0 (_:xs) = xs
        deleteAt i (x:xs) = x : deleteAt (i-1) xs
        deleteAt _ _ = assert False undefined


-- | @takeTuple n i = \\(x_0, ..., x_{n-1}) -> (x_0, ..., x_{i-1})@
takeTuple :: Int -> Int -> Q Exp
takeTuple n i = reindexTuple n [0..i-1]

-- | @dropTuple n i = \\(x_0, ..., x_{n-1}) -> (x_i, ..., x_{n-1})@
dropTuple :: Int -> Int -> Q Exp
dropTuple n i = reindexTuple n [i..n-1]


cond :: [Q (Guard, Exp)] -> ExpQ -> ExpQ
cond branches otherwiseE =
    caseE [|()|] [match wildP (guardedB (branches ++ [normalGE [|otherwise|] otherwiseE])) []]

-- | @safeDeleteTuple n@ generates a function analogous to 'delete' that takes an element and an @n@-tuple and maybe returns an @n-1@-tuple (if and only if the element was found).
safeDeleteTuple :: Int -> Q Exp
safeDeleteTuple n = do
    e <- newName "_deletee"
    withxs n (\xsp xes ->
        lamE [varP e, xsp] (

            let
                ixes = zip [0::Int ..] xes

                ges = map (\(i,xe) ->
                            normalGE
                                [| $(varE e) == $(xe) |]
                                [| Just $((tupE . map snd . filter ((/= i) . fst)) ixes) |]
                          )
                           ixes

            in
                cond ges [|Nothing|]))



-- | Like 'proj', but takes the index argument as the first argument at runtime and returns a @Maybe@.
--
-- >>> :t $(proj' 3)
-- $(proj' 3) :: Num a => (a1, a1, a1) -> a -> Maybe a1
--
proj' :: Int -> Q Exp
proj' n = do
    i <- newName "_i"
    withxs n (\xsp xes ->
        lamE [varP i,xsp]
            (caseE (varE i)
                ([ smatch (litP . integerL . fromIntegral $ j) [| Just $(xes !! j) |]
                    | j <- [0..n-1] ]

                 ++ [smatch wildP [|Nothing|]])))




-- | Generates a function that takes a binary relation and a tuple @xs@, and returns 'Just' the first index @i@ such that the relation holds for @x_i@, @x_{i+1}@, or 'Nothing'.
--
-- >>> :t $(findSuccessiveElementsSatisfying 4)
-- $(findSuccessiveElementsSatisfying 4)
--   :: (t -> t -> Bool) -> (t, t, t, t) -> Maybe Int
findSuccessiveElementsSatisfying :: Int -> Q Exp
findSuccessiveElementsSatisfying n = do
    r <- newName "_r"
    withxs n (\xsp xes ->
        lamE [varP r, xsp]
            (cond
                [ normalGE [| $(varE r) $(x0) $(x1) |]
                           [| Just ($(litE . integerL $ i) :: Int) |]

                    |
                        (i,x0,x1) <- zip3 [0..] xes (drop 1 xes) ]
                [|Nothing|]))


-- | Generates a function modifying a single element of a tuple.
updateAtN :: Int -- ^ Length of the input tuple
          -> Int -- ^ 0-based index of the element to be modified
          -> Q Exp -- ^ (b -> c) -> (a1,a2,b,a3,a4) -> (a1,a2,c,a3,a4)
updateAtN n element = do
  mapFunction <- newName "_f"
  withxs n (\xsp xes -> do

    when (element < 0 || element >= n) $
      fail ("updateAtN "++show n++" "++show element++": Element index out of range")

    let
      (start, x:xs) = splitAt element xes
      results = start ++ appE (varE mapFunction) x : xs

    lamE [varP mapFunction] ( lamE [ xsp ] $ tupE results ))
