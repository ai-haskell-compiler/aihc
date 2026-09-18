module Second (second) where

import Hidden

second :: Proxy (a :: K) -> Proxy a
second = same
