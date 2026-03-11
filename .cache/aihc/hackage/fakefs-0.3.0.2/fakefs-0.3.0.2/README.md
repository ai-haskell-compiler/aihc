# fakefs

Extensible fake file system for testing.

Provides a simple Monad transformer `FileSystemT`, intended to make a fake file system.  
Against its name, `FileSystemT` imitates *very limited* feature of a file system:  
It's essentially just a `newtype` of `StateT (Map FilePath contents) m a`,
where the type variable `contents` can be any type suitable for your tests.  
For example:

- If your tests have to handle only binary contents of a file, `contents` should be `ByteString`.
- If your tests have to handle only text contents of a file, `contents` should be `Text`.
- If your tests have to handle binary contents of a file with its permisson, `contents` should be `(Permission, ByteString)`.
- etc.

## Example

```haskell
-- BEGINNING OF EXAMPLE

import Control.Monad
import Control.Monad.Catch.Pure
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Test.Hspec
import Test.FileSystem.Fake

import Prelude hiding (readFile, appendFile)

type YourFsM = FileSystemT String Catch

-- | Record of functions to stub IO actions
data FsActions m = FsActions
    { readFile :: FilePath -> m String
    , appendFile :: FilePath -> String -> m ()
    }

fsActionsForTesting :: FsActions YourFsM
fsActionsForTesting = FsActions
    { readFile = readPath
    , appendFile = \path contents ->
        modifyPath path (Just . (++ contents) . fromMaybe "")
    }


-- | The function you test.
collectMatchedPaths :: MonadThrow m => FsActions m -> (String -> Bool) -> FilePath -> [FilePath] -> m ()
collectMatchedPaths acts p outPath = mapM_ $ \path -> do
    contents <- readFile acts path
    when (p contents) $
        appendFile acts outPath $ path ++ "\n"


main :: IO ()
main = hspec $
    describe "collectMatchedPaths" $
        it "collect paths whose content matches the given condition" $ do
            let initialFs = M.fromList
                    [ ("a.txt", "aaaaa\n")
                    , ("b.hs", "main = undefined\n")
                    , ("c.c", "#include <stdio.h>\n")
                    ]
                outPath = "result.txt"
                args = M.keys initialFs
                Right resultFs = runCatch . (`execFileSystemT` initialFs)
                    $ collectMatchedPaths fsActionsForTesting (isInfixOf "in") outPath args

            resultFs M.! outPath `shouldBe` "b.hs\nc.c\n"

-- END
```
