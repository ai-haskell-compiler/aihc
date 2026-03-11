# pipes-ordered-zip

A function to tie together two sorted Haskell Iterators (Producers from the [pipes library](http://hackage.haskell.org/package/pipes)).

Example:

    import Pipes (runEffect, (>->), each)
    import qualified Pipes.Prelude as P
    import Pipes.OrderedZip (orderedZip)

    main = do
        let a = each [1,  3,4,  6,8] -- assumed to be ordered
            b = each [  2,3,4,5,  8] -- assumed to be ordered
        let mergedProd = orderedZip compare a b
        _ <- runEffect $ mergedProd >-> P.print
        return ()

prints:

    (Just 1,Nothing)
    (Nothing,Just 2)
    (Just 3,Just 3)
    (Just 4,Just 4)
    (Nothing,Just 5)
    (Just 6,Nothing)
    (Just 8,Just 8)

and

    import Pipes (runEffect, (>->), each)
    import qualified Pipes.Prelude as P
    import Pipes.OrderedZip (orderedZipAll)

    main = do
        let a = each ([1,  3,4,  6,  8] :: [Int])
            b = each ([  2,3,4,5,    8] :: [Int])
            c = each ([  2,3,  5,  7,8] :: [Int])
            mergedProd = orderedZipAll compare [a, b, c]
        _ <- runEffect $ mergedProd >-> P.print
        return ()

prints 

    [Just 1,Nothing,Nothing]
    [Nothing,Just 2,Just 2]
    [Just 3,Just 3,Just 3]
    [Just 4,Just 4,Nothing]
    [Nothing,Just 5,Just 5]
    [Just 6,Nothing,Nothing]
    [Nothing,Nothing,Just 7]
    [Just 8,Just 8,Just 8]
