# nowdoc

Package nowdoc contain 3 QuasiQuotes.

* nowdoc: Simplest here document with only two transformation.
* txtfile: It incerts text file contents as string.
* binfile: It incerts binary file contents as string.

### QuasiQuoter nowdoc

Simplest here document.
Only two transformation.

* remove head newline if exist
* remove one space from '|', space, space, ..., ']'

### short examples

```hs
hello = [nowdoc|
abc
def
ghi
|]
```

is

```hs
hello = "abc\ndef\nghi\n"
```

,

```hs
hello = [nowdoc|hello|]
```

is

```hs
hello = "hello"
```

and

```hs
hello = [nowdoc|
hello | ]
world |  ]
! |   ]
|]
```

is

```hs
hello = "hello |]\nworld| ]\n! |  ]"
```

### run perl

```hs
{-# LANGUAGE QuasiQuotes #-}

import System.Process
import Text.Nowdoc

main :: IO ()
main = () <$ rawSystem "perl" ["-e", [nowdoc|
use strict;
use warnings;

my $hello = "Hello, world!\n";
print $hello;
|]]
```

### ASCII art

```hs
{-# LANGUAGE QuasiQuotes #-}

import Text.Nowdoc

main :: IO ()
main = putStr [nowdoc|
            ______             
       .d$$$******$$$$c.       
    .d$P"            "$$c      
   $$$$$.           .$$$*$.    
 .$$ 4$L*$$.     .$$Pd$  '$b   
 $F   *$. "$$e.e$$" 4$F   ^$b  
d$     $$   z$$$e   $$     '$. 
$P     `$L$$P` `"$$d$"      $$ 
$$     e$$F       4$$b.     $$ 
$b  .$$" $$      .$$ "4$b.  $$ 
$$e$P"    $b     d$`    "$$c$F 
'$P$$$$$$$$$$$$$$$$$$$$$$$$$$  
 "$c.      4$.  $$       .$$   
  ^$$.      $$ d$"      d$P    
    "$$c.   `$b$F    .d$P"     
      `4$$$c.$$$..e$$P"        
          `^^^^^^^`
|]
```

### QuasiQuoter txtfile

It incerts file contents as string without transformation.
It read file as text file (with default encoding on your system).

```hs
main :: IO ()
main = putStr [txtfile|foo.txt|]
```

### QuasiQuoter binfile

It incerts file contents as string without transformation.
It read file as binary file.

```hs
main :: IO ()
main = print [binfile|foo.dat|]
```
