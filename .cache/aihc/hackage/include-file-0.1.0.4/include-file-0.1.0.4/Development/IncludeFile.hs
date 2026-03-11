
-- | Inclusion of files in source code via Template Haskell.
--
--   When distributing executables, sometimes it is required to attach some other resources in
--   files. Using 'includeFileInSource' you avoid this problem by including those files inside
--   the executable at compile time.
--
-- = Example
--
--   A quick example. I want to include a small image (@foo.png@) in the executable. I would do:
--
-- > {-# LANGUAGE TemplateHaskell #-}
-- >
-- > import Development.IncludeFile
-- >
-- > $(includeFileInSource "foo.png" "myImage")
-- >
--
--   This defines the @myImage@ value with type 'B.ByteString' and with the content of the file @foo.png@.
--
-- = Using 'includeFileInSource'
--
--   The following entities must be in scope when calling 'includeFileInSource':
--
-- * A @ByteString@ type.
-- * A @Word8@ type, instance of the 'Num' class (at least with the 'fromInteger' method implemented).
-- * A @pack :: [Word8] -> ByteString@ function.
--
--   This module re-export these for convenience, but you can use your own types (for example, using
--   /lazy/ bytestrings instead of /strict/ bytestrings).
--
-- = Using lazy bytestrings
--
--   To use lazy bytestrings, instead of importing this full module, import it like this:
--
-- > import Data.ByteString.Lazy (ByteString,pack)
-- > import Development.IncludeFile (includeFileInSource,Word8)
--
--   Needless to say, if you have already imported any of those entities, you don't have to do it again.
--
-- = Performance impact
--
--   Benchmarks confirm that it is /much/ faster to use an included bytestring than reading it from a file.
--
-- > benchmarking include-file
-- > time                 1.814 ns   (1.799 ns .. 1.826 ns)
-- >                      1.000 R²   (0.999 R² .. 1.000 R²)
-- > mean                 1.808 ns   (1.797 ns .. 1.819 ns)
-- > std dev              37.48 ps   (31.27 ps .. 46.78 ps)
-- > 
-- > benchmarking read-file
-- > time                 4.869 μs   (4.798 μs .. 4.938 μs)
-- >                      0.998 R²   (0.998 R² .. 0.999 R²)
-- > mean                 4.911 μs   (4.857 μs .. 4.968 μs)
-- > std dev              178.8 ns   (150.1 ns .. 212.5 ns)
--
-- = Large files
--
--   Do not use 'includeFileInSource' with large files. The limit between what /is/ and what is /not/ a large
--   file remains uncertain.
--
module Development.IncludeFile (
    -- * Including files
    includeFileInSource
    -- * Convenient re-exports
  , B.ByteString
  , Word8
  , B.pack
  ) where

import qualified Data.ByteString as B
import Data.Word (Word8)
import Language.Haskell.TH

-- | Define a value of type @ByteString@ (where @ByteString@ is whatever @ByteString@ type is in scope)
--   with the content of a file.
includeFileInSource :: FilePath -- ^ Path to file
                    -> String -- ^ Haskell value name
                    -> Q [Dec]
includeFileInSource fp n = do
  b <- runIO $ B.readFile fp
  let ws = B.unpack b
      wtype = ConT $ mkName "Word8"
      btype = ConT $ mkName "ByteString"
  return
    [ SigD (mkName n) btype
    , FunD (mkName n) [Clause [] (NormalB $
         AppE (SigE (VarE $ mkName "pack") $ ArrowT `AppT` (ListT `AppT` wtype) `AppT` btype)
           $ ListE $ fmap (LitE . IntegerL . fromIntegral) ws
                                   ) []]
      ]
