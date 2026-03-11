# Zenacy HTML

[![hackage-shield][]][hackage-version]
[![stackage-shield][]][stackage-version]
[![linux-shield][]][linux-build]
[![packdeps-shield][]][packdeps]

Zenacy HTML is an HTML parsing and processing library that implements the
WHATWG HTML parsing standard.  The standard is described as a state machine
that this library implements exactly as spelled out including all the error
handling, recovery, and conformance checks that makes it robust in handling
any HTML pulled from the web.  In addition to parsing, the library provides
many processing features to help extract information from web pages or
rewrite them and render the modified results.

## Introduction

The Zenacy HTML parser is an implementation of the HTML parsing standard
defined by the WHATWG.

https://html.spec.whatwg.org/multipage/parsing.html

The standard defines a parsing state machine, so it is very prescriptive
on how HTML is handled including many edge cases and error recovery.
This library aims to follow the standard closely in such a way to match the
code back to the standard and make future updates straightforward.

One of the main uses an a HTML parser is for extracting information from
the web.  Having a parser that can handle all the nuances of poorly
formatted HTML helps to make this extraction as robust as possible.
This was a key motivation in deciding to implement a parser in this fashion.
Additionally, the standard describes the algorithms needed to produce the
correct document structure.  Applications that are sensitive to the
document structure, such as extracting and rewriting large portions of
a web page, may benefit from Zenacy HTML.

The library provides a wide variety of features including:

* A fully standard compliant HTML parser
* HTML Fragment parsing
* Document rendering
* A zipper type for document traversal
* An iterator type for document walking
* Various functions for processing aspects of HTML
* Lightweight queries for rewriting

## Parsing

The library is designed to be imported unqualified.

```haskell
import Zenacy.HTML
```

The `htmlParseEasy` function can be used to parse an HTML document string
and return the document model.

```haskell
htmlParseEasy "<div>HelloWorld</div>"
```

Note that some of the missing elements where automatically added to
the document structure as required by the standard.

```haskell
HTMLDocument ""
  [ HTMLElement "html" HTMLNamespaceHTML []
    [ HTMLElement "head" HTMLNamespaceHTML [] []
    , HTMLElement "body" HTMLNamespaceHTML []
      [ HTMLElement "div" HTMLNamespaceHTML []
        [ HTMLText "HelloWorld" ] ] ] ]
```

The parsed result can also be rendered using `htmlRender`.

```haskell
htmlRender $ htmlParseEasy "<div>HelloWorld</div>"
```

The resulting rendered document appears like so.

```html
<html><head></head><body><div>HelloWorld</div></body></html>
```

## Rewriting

This example illustrates a function that converts span elements to divs.

```haskell
rewrite :: Text -> Text
rewrite = htmlRender . htmlMapElem f . fromJust . htmlDocHtml . htmlParseEasy
  where
    f x
      | htmlElemHasName "span" x = htmlElemRename "div" x
      | otherwise = x

rewrite "<span>Hello</span><span>World</span>"
```

Running the above gives the modified document.

```html
<html><head></head><body><div>Hello</div><div>World</div></body></html>
```

## Extraction

The next example shows one way to find all the hyperlinks in a document.
This solution recurses over the document elements while ignoring fragments
and templates.

```haskell
extract :: Text -> [Text]
extract = go . htmlParseEasy
  where
    go = \case
      HTMLDocument n c ->
        concatMap go c
      e @ (HTMLElement "a" s a c) ->
        case htmlElemAttrFind (htmlAttrHasName "href") e of
          Just (HTMLAttr n v s) ->
            v : concatMap go c
          Nothing ->
            concatMap go c
      HTMLElement n s a c ->
        concatMap go c
      _otherwise ->
        []

extract "<a href=\"https://example1.com\"></a><a href=\"https://example2.com\"></a>"
```

The extract function will give the following list.

```haskell
[ "https://example1.com"
, "https://example2.com"
]
```

## Queries

The library includes a basic query facility implemented as a thin wrapper
around an `HTMLZipper`.  Queries match patterns in HTML structures and can
be used to extract information or update documents.  As a first example,
consider the following HTML.

```html
<p>
  <span id="x" class="y z"></span>
  <br>
  <a href="bbb">AAA</a>
  <img>
</p>
```

The HTML can be parsed as normal.  Note though the additional step of
whitespace removal, which is often important in documents that include
indentation such as above.

```haskell
fromJust . htmlSpaceRemove . fromJust . htmlDocBody . htmlParseEasy
```

Now a query function can be defined.  This function expects to be given
a `body` element whose first child is a `p` element whose first child
has an id of `x` whose second sibling is an anchor element.  If all of
those conditions are met, the the text contents of the anchor is returned.

```haskell
query :: HTMLNode -> Maybe Text
query = htmlQueryExec $ do
  htmlQueryName "body"
  htmlQueryFirst
  htmlQueryName "p"
  htmlQueryFirst
  htmlQueryId "x"
  htmlQueryNext
  htmlQueryNext
  htmlQueryName "a"
  a <- htmlQueryNode
  htmlQuerySucc $
    fromMaybe "" $ htmlElemText a
```

Running the query on the parsed document will give the result.

```haskell
Just "AAA"
```

Queries can also be used to modifiy documents.  In the next example, let's
say we would like to find any `img` that is the only content in a `div` and
replace the `div` with a link.  The document could look as follows.

```html
<section><div><img src="aaa"></div></section>
<section><div><img src="bbb"></div></section>
<section><div><img src="ccc"></div></section>
```

A query function can be defined to match the desired pattern and return the
modified element.

```haskell
query2 :: HTMLNode -> HTMLNode
query2 = htmlQueryTry $ do
  htmlQueryName "div"
  htmlQueryOnly "img"
  a <- htmlQueryNode
  let Just b = htmlElemGetAttr "src" a
  htmlQuerySucc $
    htmlElem "a" [ htmlAttr "href" b ]
      [ htmlText b ]
```

The query can then be applied to the entire document using `htmlMapElem`.

```haskell
htmlMapElem query2
```

Rendering the mapped query with give the updated content.

```html
<section><a href="aaa">aaa</a></section>
<section><a href="bbb">bbb</a></section>
<section><a href="ccc">ccc</a></section>
```

## Samples

The unit tests include the above samples as well as many other example
usages of the library.

## Origin

Zenacy HTML was originally developed for Zenacy Reader Technologies LLC
starting around 2015 and used in a web reading SaaS for a few years.
The need to understand and handle the wide variety and sublties of HTML
found on the web lead to the development of library that closely followed
the standard.  The library was tweaked and optimized a bit and though
there is room for more improvements the result worked quite well in
production (a lot of credit goes to the GHC team and Haskell community
for providing such great, fast functional programming tools).

[hackage-shield]: https://img.shields.io/hackage/v/zenacy-html.svg?label=Hackage
[hackage-version]: https://hackage.haskell.org/package/zenacy-html
[stackage-shield]: https://www.stackage.org/package/zenacy-html/badge/nightly?label=Stackage
[stackage-version]: https://www.stackage.org/package/zenacy-html
[linux-shield]: https://img.shields.io/travis/com/mlcfp/zenacy-html?label=Linux%20build
[linux-build]: https://travis-ci.org/mlcfp/zenacy-html
[packdeps-shield]: https://img.shields.io/hackage-deps/v/zenacy-html.svg?maxAge=3600
[packdeps]: http://packdeps.haskellers.com/feed?needle=zenacy-html

