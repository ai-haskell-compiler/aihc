lucid [![Hackage](https://img.shields.io/hackage/v/lucid.svg?style=flat)](https://hackage.haskell.org/package/lucid) [![Build Status](https://travis-ci.org/chrisdone/lucid.png)](https://travis-ci.org/chrisdone/lucid)
=====

Clear to write, read and edit DSL for writing HTML

**Table of Contents**

- [Introduction](#introduction)
- [Rendering](#rendering)
- [Good to know](#good-to-know)
- [Transforming](#transforming)

## Introduction

HTML terms in Lucid are written with a postfix ‘`_`’ to indicate data
rather than code. Some examples:

`p_`, `class_`, `table_`, `style_`

See `Lucid.Html5` for a complete list of Html5 combinators.

Plain text is written using the `OverloadedStrings` and
`ExtendedDefaultRules` extensions, and is automatically escaped:

``` haskell
λ> "123 < 456" :: Html ()
```

``` html
123 &lt; 456
```

Elements nest by function application:

``` haskell
λ> table_ (tr_ (td_ (p_ "Hello, World!"))) :: Html ()
```

``` html
<table><tr><td><p>Hello, World!</p></td></tr></table>
```

Elements are juxtaposed via monoidal append:

``` haskell
λ> p_ "hello" <> p_ "sup" :: Html ()
```

``` html
<p>hello</p><p>sup</p>
```

Or monadic sequencing:

``` haskell
λ> div_ (do p_ "hello"; p_ "sup") :: Html ()
```

``` html
<div><p>hello</p><p>sup</p></div>
```

Attributes are set by providing an argument list:

``` haskell
λ> p_ [class_ "brand"] "Lucid Inc" :: Html ()
```

``` html
<p class="brand">Lucid Inc</p>
```

Here is a fuller example of Lucid:

``` haskell
table_ [rows_ "2"]
       (tr_ (do td_ [class_ "top",colspan_ "2",style_ "color:red"]
                    (p_ "Hello, attributes!")
                td_ "yay!"))
```

``` html
<table rows="2">
  <tr>
    <td style="color:red" colspan="2" class="top">
      <p>Hello, attributes!</p>
    </td>
    <td>yay!</td>
  </tr>
</table>
```

## Rendering

For proper rendering you can easily run some HTML immediately with:

``` haskell
λ> renderText (p_ "Hello!")
```

``` html
"<p>Hello!</p>"
```

Or to bytes:

``` haskell
λ> renderBS (p_ [style_ "color:red"] "Hello!")
```

``` html
"<p style=\"color:red\">Hello!</p>"
```

For ease of use in GHCi, there is a `Show` instance, as
demonstrated above.

If the above rendering functions aren't suited for your purpose, you
can run the monad directly via `execHtml` and use the more low-level
blaze `Builder`, which has a plethora of output modes in
Blaze.ByteString.Builder.

See the documentation for the `Lucid` module for information about
using it as a monad transformer.

## Good to know

* Attributes are escaped, so you cannot write arbitrary JavaScript in attributes. Instead, do something like `onclick_ "foo()"`.
* Attributes are rendered in the order that they are written in your Haskell code.

## Transforming

You can use `lift` to call parent monads.

``` haskell
λ> runReader (renderTextT (html_ (body_ (do name <- lift ask
                                            p_ [class_ "name"] (toHtml name)))))
             ("Chris" :: String)
```
``` html
"<html><body><p class=\"name\">Chris</p></body></html>"
```
