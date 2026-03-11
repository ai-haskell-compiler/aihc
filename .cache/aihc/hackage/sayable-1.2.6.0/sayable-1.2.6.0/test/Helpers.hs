module Helpers where

import qualified Language.Haskell.TH as TH

foo2Filter :: TH.Name -> Bool
foo2Filter nm = and [ "HiddenValue" /= TH.nameBase nm
                    , maybe False ("Main" ==) $ TH.nameModule nm
                    ]
