module First (module Hidden, first) where

import Hidden

identity x = x

first x = identity (same x)
