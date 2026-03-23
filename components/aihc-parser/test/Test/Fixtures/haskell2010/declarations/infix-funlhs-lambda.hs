module InfixFunlhsLambda where
mb ?> x = mb >>= \b -> when b x
