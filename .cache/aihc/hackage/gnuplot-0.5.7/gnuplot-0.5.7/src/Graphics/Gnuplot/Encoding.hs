{- |
Support for special characters

Gnuplot has no universal Unicode escaping mechanism,
you can only work with encodings.
However, not all terminals support all encodings,
not all terminals even support utf-8.
Some terminals seem to support only one encoding.
E.g. WX seems to support only UTF-8,
X11 seems to support only Latin-1.
Postscript, SVG, PNG seem to support both UTF-8 and Latin-1.

The @gnuplot@ Haskell bindings
always write using the system-wide default encoding.
Thus it is better not to set an encoding other than 'locale' explicitly.
However, if you write the files yourself in a certain encoding
you should use the @encoding@ option of the according terminal.
-}
module Graphics.Gnuplot.Encoding (
   T,
   locale, deflt,
   iso_8859_1, iso_8859_15, iso_8859_2, iso_8859_9,
   koi8r, koi8u,
   cp437, cp850, cp852, cp950, cp1250, cp1251, cp1254,
   sjis, utf8,
   ) where

import Graphics.Gnuplot.Private.Encoding (T(Cons))


locale, deflt,
   iso_8859_1, iso_8859_15, iso_8859_2, iso_8859_9,
   koi8r, koi8u,
   cp437, cp850, cp852, cp950, cp1250, cp1251, cp1254,
   sjis, utf8 :: T

locale      = Cons "locale"
deflt       = Cons "default"
iso_8859_1  = Cons "iso_8859_1"
iso_8859_15 = Cons "iso_8859_15"
iso_8859_2  = Cons "iso_8859_2"
iso_8859_9  = Cons "iso_8859_9"
koi8r       = Cons "koi8r"
koi8u       = Cons "koi8u"
cp437       = Cons "cp437"
cp850       = Cons "cp850"
cp852       = Cons "cp852"
cp950       = Cons "cp950"
cp1250      = Cons "cp1250"
cp1251      = Cons "cp1251"
cp1254      = Cons "cp1254"
sjis        = Cons "sjis"
utf8        = Cons "utf8"
