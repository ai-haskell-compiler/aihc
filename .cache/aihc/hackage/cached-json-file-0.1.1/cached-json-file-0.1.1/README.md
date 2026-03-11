# cached-json-file

A Haskell library providing a cached json file.

Useful for frequently used programs that use some remote json data
which changes rather slowly (like in hours, days, weeks or months),
where it is not critical to have always the latest data locally immediately.

## Usage

`getCachedJSON dir file url minutes` caches the json obtained from `url` in
`~/.cache/dir/file` for `minutes`.

eg:

```haskell
import System.Cached.JSON

getSnapshots :: IO Object
getSnapshots =
  getCachedJSON "stackage-snapshots" "snapshots.json" "http://haddock.stackage.org/snapshots.json" 180

main = getSnapshots >>= print
```

Each time you run this program within 3 hours the data will be read
from the local cache file `~/.cache/stackage-snapshots/snapshots.json`
rather than re-downloading it each time,
which helps to speed up the program and avoid unnecessary web queries.

There is also `getCachedJSONQuery prog jsonfile webquery minutes`
which uses `webquery :: (FromJSON a, ToJSON a) => IO a` to download
the json data.

The shortest cache time is 1 minute.
