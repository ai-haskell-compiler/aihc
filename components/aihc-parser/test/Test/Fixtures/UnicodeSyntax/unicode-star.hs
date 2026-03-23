{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module UnicodeSyntaxStar where

-- Using Unicode star for kinds
data Proxy (a ∷ ★) = Proxy
