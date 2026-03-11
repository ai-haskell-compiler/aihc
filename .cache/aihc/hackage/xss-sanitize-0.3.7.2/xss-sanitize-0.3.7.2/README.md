# Summary

[![Tests](https://github.com/yesodweb/haskell-xss-sanitize/actions/workflows/tests.yml/badge.svg)](https://github.com/yesodweb/haskell-xss-sanitize/actions/workflows/tests.yml)

xss-sanitize allows you to accept html from untrusted sources by first filtering it through a white list.
The white list filtering is fairly comprehensive, including support for css in style attributes, but there are limitations enumerated below.

Sanitizing allows a web application to safely use a rich text editor, allow html in comments, or otherwise display untrusted HTML.

If you trust the HTML (you wrote it), you do not need to use this.
If you don't trust the html you probably also do not trust that the tags are balanced and should use the sanitizeBalance function.

# Usage

provides 2 functions in the module Text.HTML.SanitizeXSS

* sanitize - filters html to prevent XSS attacks.
* sanitizeBalance - same as sanitize but makes sure there are no lone opening/closing tags - useful to protect against a user's html messing up your page


# Details

This is not escaping! Escaping html does prevent XSS attacks. Strings (that aren't meant to be HTML) should be HTML escaped to show up properly and to prevent XSS attacks. However, escaping will ruin the display of actual HTML.

This function removes any HTML tags or attributes that are not in its white-list. This may sound picky, but most HTML should make it through unchanged, making the process unnoticeable to the user but giving us safe HTML. 


## Integration

It is recommended to integrate this so that it is automatically used whenever an application receives untrusted html data (instead of before it is displayed). See the Yesod web framework as an example.


## Limitations

### Lowercase

All tag names and attribute names are converted to lower case as a matter of convenience. If you have a use case where this is undesirable let me know.

### Balancing - sanitizeBalance

The goal of this function is to prevent your html from breaking when (unknown) html with unbalanced tags are placed inside it. I would expect it to work very well in practice and don't see a downside to using it unless you have an alternative approach. However, this function does not at all guarantee valid html. In fact, it is likely that the result of balancing will still be invalid HTML. There is no guarantee for how a browser will display invalid HTML, so there is no guarantee that this function will protect your HTML from being broken by a user's html. Other possible approaches would be to run the HTML through a library like libxml2 which understands HTML or to first render the HTML in a hidden iframe or hidden div at the bottom of the page so that it is isolated, and then use JavaScript to insert it into the page where you want it.

### TagSoup Parser

TagSoup is used to parse the HTML, and it does a good job. However TagSoup does not maintain all white space. TagSoup does not distinguish between the following cases:

    <a href="foo">, <a href=foo>
    <a   href>, <a href>
    <a></a>, <a/>

In the third case, img and br tags will be output as a single self-closing tags. Other self-closing tags will be output as an open and closing pair. So `<img /> or <img><img>` converts to `<img />`, and `<a></a> or <a/>` converts to `<a></a>`.  There are future updates to TagSoup planned so that TagSoup will be able to render tags exactly the same as they were parsed.


## Security

### Where is the white list from?

Ultimately this is where your security comes from. I would expect that a faulty white list would act as a strong deterrent, but this library strives for correctness.

The [source code of html5lib](https://github.com/html5lib/html5lib-python/blob/master/html5lib/filters/sanitizer.py) is the source of the white list and my implementation reference. If you feel a tag is missing from the white list, check to see if it has been added there.

If anyone knows of better sources or thinks a particular tag/attribute/value may be vulnerable, please let me know.
[HTML Purifier](http://htmlpurifier.org/live/smoketests/printDefinition.php) does have a more permissive and configurable (yet safe) white list if you are looking to add anything.

### Where is the code from?

Original code was taken from John MacFarlane's Pandoc (with permission), but modified by Greg Weber to be faster and with parsing redone using TagSoup, and to use html5lib's white list.
Michael Snoyman added the balanced tags functionality and released css-text specifically to help with css parsing.
html5lib's sanitizer.py is used as a reference implementation, and most of the code should look the same. The css parsing is different: as mentioned we use a css parser, not regexes like html5lib.

### style attribute

style attributes are now parsed with the css-text and autoparsec-text dependencies. They are then ran through a white list for properties and keywords. Whitespace is not preserved. This code was again translated from sanitizer.py, but uses attoparsec instead of regexes. If you don't care about stripping css you can avoid the attoparsec dependendcy by using the older < 0.3 version of this library.

### data attributes

data attributes are not on the white list.
The href and style attributes are white listed, but its values must pass through a white list also. This is how the data attributes could work also.

### svg and mathml

A mathml white list is fully implemented. There is some support for svg styling. 
There is a full white list for svg elements and attributes. However, some elements are not included because they need further filtering (just like the data attributes) and this has not been done yet.
