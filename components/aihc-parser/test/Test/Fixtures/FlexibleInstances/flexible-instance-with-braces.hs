{- ORACLE_TEST
id: flexible-instance-with-braces
category: FlexibleInstances
expected: pass
reason: parser supports flexible instances with explicit braces
-}
{-# LANGUAGE FlexibleInstances #-}
instance Validity Scientific where {}
