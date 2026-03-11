
Codec.Text.IConv
================

This is a Haskell binding to the `iconv()` C library function.

The only module exported is `Codec.Text.IConv`, which provides a single
function:

    -- | Convert fromCharset toCharset input output
    convert :: EncodingName -> EncodingName -> Lazy.ByteString -> Lazy.ByteString

where `fromCharset` and `toCharset` are the names of the input and output
character set encodings, and input and output are the input and output text
as lazy ByteStrings. For example:

     import qualified Codec.Text.IConv as IConv
     import qualified Data.ByteString.Lazy as BS
     
     main = do
       -- read UTF8, but convert to UTF32 internally
       content <- fmap (IConv.convert "UTF-8" "UTF-32") (BS.readFile file)
       ...

An [example program], similar to the iconv program, is included.

[example program]: examples/hiconv.hs

Character set encodings
-----------------------

To see a list of encoding names which are known by your operating system,
run `iconv --list` in a shell. Likely encodings are listed on the [libiconv]
web site.

[libiconv]: http://www.gnu.org/software/libiconv/

Availability of `iconv()`
-------------------------

The `iconv(3)` function conforms to POSIX.1-2001. It is provided by the [GNU C
library].

[GNU C library]: http://www.gnu.org/software/libc/manual/html_node/Character-Set-Handling.html

On systems which do not have a native iconv() implementation you may need to
install [libiconv].

