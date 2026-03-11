[![Build Status](https://travis-ci.org/github/obsidiansystems/prim-uniq.svg)](https://travis-ci.org/obsidiansystems/prim-uniq)

Unique values and an ad-hoc "unique-tag GADT"
=============================================

This library defines 2 types - `Uniq` and `Tag`.

`Uniq` is a traditional "unique value" type, extended with a state-token type parameter so it works in both `IO` and `ST`.

`Tag` is like `Uniq` with the addition of a phantom type parameter.  The type of that parameter is fixed at the time the `Tag` is created, so the uniqueness of the tag means that equality of tag values witnesses equality of their phantom types.  In other words, given two `Tag`s, if they are equal then their phantom type parameters are the same - just like pattern matching on a GADT constructor.  The `GEq` and `GCompare` classes from the `dependent-sum` package can be used to discover that type equality, allowing `Tag` to be used for a pretty cool semi-dynamic typing scheme.  For example (using the `dependent-map` library):

    import Data.Unique.Tag
    import Data.Dependent.Map
    
    main = do
        x <- newTag
        y <- newTag
        z <- newTag
        let m1 = fromList [x :=> 3,  y :=> "hello"]
            m2 = fromList [x :=> 17, z :=> (True, x)]
            -- the type checker would (rightly) reject this line:
            -- m3 = singleton y ("foo", "bar")
        
        print (m1 ! x)
        print (m1 ! y)
        print (m2 ! x)
        print (m1 ! snd (m2 ! z))

Which would print:

    3
    "hello"
    17
    3
