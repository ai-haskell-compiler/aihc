You need some language extensions to be able to
define Interval Instances:

> {-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

In most cases, you should use the value-strict version:

> import qualified Data.IntervalMap.Generic.Strict as IM

Make tuples an instance of Interval:

> instance Ord e => IM.Interval (e,e) e where
>   lowerBound (a,_) = a
>   upperBound (_,b) = b
>   rightClosed _ = False

By using `rightClosed _ = False` we have defined tuples to be half-open
intervals - they include the starting value, but not the end value.

Now we can put them into a Map:

> type MyMap = IM.IntervalMap (Int,Int) String
>
> sample :: MyMap
> sample = IM.fromList [((1,6), "Foo"), ((2,4), "Bar"), ((4,7), "Baz")]

Lookup intervals containing a given point ("stabbing query"):

> main = print (IM.toAscList $ sample `IM.containing` 3)

