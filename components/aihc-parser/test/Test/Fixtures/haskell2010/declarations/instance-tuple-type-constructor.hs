{- ORACLE_TEST
id: decls-instance-tuple-type-constructor
category: declarations
expected: pass
reason: parser now supports tuple type constructors like ((,,) w s) in instance heads
-}
module M where
class Extractable f where
  runSingleton :: f a -> a
instance Extractable ((,,) w s) where
  runSingleton (_,_,x) = x
