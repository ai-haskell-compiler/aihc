{- ORACLE_TEST pass -}
{-# LANGUAGE TransformListComp #-}
module MultipleTransforms where
import GHC.Exts (the, groupWith, sortWith)
output = [ (the dept, sum salary)
         | (name, dept, salary) <- employees
         , then group by dept using groupWith
         , then sortWith by (sum salary)
         , then take 5 ]
