{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Text.StringTemplate.Renderf ((|=), SEType(..)) where
import Text.StringTemplate.Base

class Stringable b => SEType b a where
    renderf :: StringTemplate b -> a
instance Stringable a => SEType a a where
    renderf = render
instance Stringable a => SEType a (StringTemplate a) where
    renderf = id
instance (ToSElem a, SEType b r) => SEType b ((String, a) -> r) where
    renderf x (k, v) = renderf $ setAttribute k v x

(|=) :: (Monad m) => a -> m a1 -> m (a, a1)
k |= v = return . (,) k =<< v
infixl 5 |=
