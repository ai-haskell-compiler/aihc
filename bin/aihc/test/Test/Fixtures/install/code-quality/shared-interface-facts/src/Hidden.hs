module Hidden where

data K = K

data Proxy (a :: k) = Proxy

data Payload = Payload

data Box a = Box a Payload

class Select a where
  select :: Box a -> a

instance Select (Proxy (a :: K)) where
  select (Box x _) = x

same :: Proxy (a :: K) -> Proxy a
same x = x
