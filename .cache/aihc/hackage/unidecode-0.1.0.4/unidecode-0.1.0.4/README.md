Unidecode: a transliterator of last resort
==========================================

So, for some reason you're stuck in the 90s and can't use UTF text. Punycode isn't cutting it?
Fine: Unidecode will help you murder some carefully constructed text into something vaguely readable in good ol'
"I don't wanna learn anything"-ASCII.

There's one function, unidecode :: Char -> String. If you have more text, I'm sure you can foldMap & pack it.

(This is sourced ultimately from Unicode.PM, but I scavenged the tables from Carter Schonwald, who graciously
allowed usage.)

Why did you write a bunch of C, Mark? Don't you like Haskell?
=============================================================

GHC compiled a big Haskell literal list fine, then crashed at runtime with some weaksauce excuse
about "i can't handle something that big, the compiler should have rejected it". Cons cells are for
closers, RTS.

The other approach that Carter took was to build an IntMap on top of the C data structures, but for reasons
I still can't quite comprehend, it didn't pass the stack-leak detection trick at 
https://neilmitchell.blogspot.de/2015/09/detecting-space-leaks.html

quick bsearch did the job (and I learned in the process that a Haskell project is not a bad way
to build C code).

issues/PRs all welcome, but hopefully there's not anything missing.
