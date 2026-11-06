-- | The one way in which aihc-heap reports an operation that AIHC cannot
-- implement.
module GHC.Exts.Heap.Unsupported
  ( unsupported,
  )
where

import Prelude

-- | Raise an exception that names an operation of ghc-heap. AIHC has no
-- GHC heap layout, so the operation cannot give a true result.
unsupported :: String -> a
unsupported name =
  errorWithoutStackTrace
    ("aihc-heap: " ++ name ++ " is not supported by AIHC")
