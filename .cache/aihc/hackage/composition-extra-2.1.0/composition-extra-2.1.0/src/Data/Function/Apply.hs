module Data.Function.Apply where

import Data.Function.Slip


(-$) = flip

infixl 8 -$

(--$) = (<<~>)

infixl 8 --$

(---$) = (<<~~>)

infixl 8 ---$

(----$) = (<<~~~>)

infixl 8 ----$
