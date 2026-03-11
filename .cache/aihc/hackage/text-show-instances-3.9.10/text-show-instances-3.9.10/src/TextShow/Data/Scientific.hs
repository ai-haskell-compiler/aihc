{-# OPTIONS -Wno-orphans #-}
module TextShow.Data.Scientific () where
import Data.Scientific
import Data.Text.Lazy.Builder.Scientific ( scientificBuilder )
import TextShow

instance TextShow Scientific where
        showbPrec d s
                | coefficient s < 0 = showbParen (d > prefixMinusPrec) (scientificBuilder s)
                | otherwise         = scientificBuilder s
                where prefixMinusPrec :: Int
                      prefixMinusPrec = 6
