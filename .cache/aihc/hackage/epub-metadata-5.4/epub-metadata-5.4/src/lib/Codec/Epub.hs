{- |
   This is a library for parsing and manipulating epub document metadata. Almost all of the data stored in the epub Package Document can be worked with using this API. This includes the most useful block of data, the epub metadata. This library supports epub versions 2 and 3.

   This library was constructed by studying the IDPF specifications for epub documents found here <http://www.idpf.org/epub/20/spec/OPF_2.0.1_draft.htm> and here <http://www.idpf.org/epub/30/spec/epub30-publications.html>

   Consumers of epub-metadata will likely need this module, which re-exports the most useful parts concerning disk IO, parsing of the XML into Codec.Epub.Data.* data structures and formatting these data structures to be pretty-printed. Please also see Codec.Epub.Data.Metadata etc.
-}
module Codec.Epub (
   -- * Epub
   module Codec.Epub.Format,
   module Codec.Epub.IO,
   module Codec.Epub.Parse

   -- * Example
   -- $example1
   )
   where

import Codec.Epub.Format
import Codec.Epub.IO
import Codec.Epub.Parse


{- $example1
>  import Codec.Epub
>  import Codec.Epub.Data.Package
>  import Control.Monad.Error
>  
>  
>  main :: IO ()
>  main = do
>     -- epub-metadata actions are in MonadIO and MonadError, so we're
>     -- using ErrorT here
>  
>     result <- runErrorT $ do
>  
>        -- Use the getPkgXmlFromZip action to extract the Package
>        -- Document as an XML string. There are also other actions
>        -- for reading from ByteStringS and directories.
>        --
>        -- See Codec.Epub.IO
>  
>        xmlString <- getPkgXmlFromZip "/path/to/book.epub"
>  
>        -- Now the sections of meta-information about the book can
>        -- be extracted from that XML using functions like getPackage,
>        -- getMetadata, etc.
>        --
>        -- See Codec.Epub.Parse
>  
>        pkg <- getPackage xmlString  -- :: Codec.Epub.Data.Package
>        meta <- getMetadata xmlString  -- :: Codec.Epub.Data.Metadata
>  
>        -- Parts of these data structures can be used from here
>        -- as needed
>        --
>        -- See Codec.Epub.Data.Package for pkgVersion below
>        -- and the others in Codec.Epub.Data.*
>  
>        liftIO $ putStrLn $ pkgVersion pkg
>  
>        -- There is also pretty-print formatting of these data types
>        -- in Codec.Epub.Data through the Formattable typeclass
>        --
>        -- See Codec.Epub.Format
>  
>        liftIO $ putStr $ format meta
>  
>     either putStrLn return result
-}
