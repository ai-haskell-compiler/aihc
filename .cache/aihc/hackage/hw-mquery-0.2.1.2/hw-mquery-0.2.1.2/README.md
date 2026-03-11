# hw-mquery
[![master](https://circleci.com/gh/haskell-works/hw-mquery/tree/master.svg?style=svg)](https://circleci.com/gh/haskell-works/hw-mquery/tree/master)

```
import Control.Monad
import Data.Function
import Data.List
import HaskellWorks.Data.Json.Load
import HaskellWorks.Data.Json.PartialValue
import HaskellWorks.Data.Micro
import HaskellWorks.Data.MQuery
import HaskellWorks.Data.Row
import HaskellWorks.Diagnostics
import Text.PrettyPrint.ANSI.Leijen

import qualified Data.DList as DL
```

```
!json <- loadJsonPartial "../data/78mb.json"
!json <- loadJsonWithIndex "../data/78mb.json"
!json <- loadJsonWithPoppy512Index "../data/78mb.json"
!json <- loadJsonWithPoppy512SMinMaxIndex "../data/78mb.json"
let q = MQuery (DL.singleton json)
```

```
putPretty $ q >>= item & limit 10
putPretty $ q >>= item & page 10 1
putPretty $ q >>= item >>= hasKV "founded_year" (JsonPartialNumber 2005) & limit 10
putPretty $ q >>= item >>= entry
putPretty $ q >>= item >>= entry >>= named "name" & limit 10
putPretty $ q >>= item >>= entry >>= satisfying (\(k, _) -> k == "name") >>= value & limit 10
putPretty $ q >>= item >>= entry >>= satisfying ((== "name") . fst) >>= value & limit 10
putPretty $ q >>= (item >=> entry >=> key) & limit 10
putPretty $ q >>= item >>= entry >>= key & limit 100 & onList (uniq . sort)
putPretty $ (q >>= item >>= entry & limit 1) >>= field "name" & limit 10
putPretty $ do {j <- q; e <- item j; (k, v) <- entry e; return k}
putPretty $ do {j <- q; e <- item j; (k, v) <- entry e; guard (k == "name"); return v}
```

### Decoding
#### Line separated base 64 encoded gzipped json
while read in; do echo "$in" | base64 --decode | gunzip; echo ""; done < file.lgz > firehose.json

### Profiling with stack traces
```
mafia build -p
cabal repl --ghc-options='-fexternal-interpreter -prof'
```

```
import HaskellWorks.Data.Succinct.BalancedParens
import HaskellWorks.Data.Succinct.RankSelect.Binary.Poppy512
import HaskellWorks.Data.Positioning
import qualified Data.Vector.Storable as DVS
import HaskellWorks.Data.IndexedSeq
(jsonBS, jsonIb, jsonBp) <- loadJsonRawWithIndex "firehose.json"
let bp1 = SimpleBalancedParens jsonBp
let bp2 = SimpleBalancedParens (makePoppy512 jsonBp)
let bp3 = makePoppy512 jsonBp
```
