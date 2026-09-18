module Public (throughBoth, selectBoth, selectAny) where

import Reexport
import Second

throughBoth :: Proxy (a :: K) -> Proxy a
throughBoth x = first (second x)

selectBoth :: Box (Proxy (a :: K)) -> Proxy a
selectBoth x = throughBoth (select x)

selectAny :: Select a => Box a -> a
selectAny x = select x
