module Public where

import Reexport

create :: Output -> IO (FunPtr Output)
create = wrap

call :: FunPtr Output -> Output
call = invoke
