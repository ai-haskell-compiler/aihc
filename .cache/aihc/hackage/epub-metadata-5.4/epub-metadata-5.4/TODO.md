## 2023-06-30

### Need to support EPUB 3.x date elements

This from EPUB 2.x

    <dc:date opf:event="original-publication">2003</dc:date>
    <dc:date opf:event="publication">2023-01-02</dc:date>
    <dc:date>2023-06-30</dc:date>  <!-- Meaning of this is ambiguous though -->

becomes in EPUB 3.x

    <meta property="dcterms:issued">2003</meta>  <!-- This is original-publication -->
    <dc:date>2023-01-02</dc:date>  <!-- When this EPUB document was created -->
    <meta property="dcterms:modified">2023-06-30</meta>  <!-- This is the last time this EPUB document was modified -->

All of this needs to be documented in the usage as well.

### Use optparse-applicative for epubmeta, the parsing is primitive and shit

### Support meta tags with their textual values in either the content attribute or between the begin/end meta tags

### On the subject of dates in epub

It's kind of ugly to still be storing the flat string date from the XML but
it's unclear how useful it would be to parse these into something like UTCTime.
And in fact there are at least 6(!) shapes of dates depending on what they're
used for.D

I think there would have been some contextual information in the type

    data Date = Date
      { DateYear = UTCTime                -- YYYY
      , DateDay = UTCTime                 -- YYYY-MM-DD
      , DateTime = ZonedTime              -- YYYY-MM-DDTHH:MM:SSZ
      , DateTimeStart = ZonedTime         -- YYYY-MM-DDTHH:MM:SSZ/
      , DateTimeEnd = ZonedTime           -- /YYYY-MM-DDTHH:MM:SSZ
      , DateTimeRange ZonedTime ZonedTime -- YYYY-MM-DDTHH:MM:SSZ/YYYY-MM-DDTHH:MM:SSZ
      }

Use of some of these date shapes hasn't been widely observed in books yet.


## older notes

- May need to add a special function that will try to locate modified
  even in the metaDate field (which is what an epub2 might look like)
- Need more API docs. Detail the differences between epub2 and
  epub3 in the data structures, where the fields come from and which
  are only one or the other.
- Separate some of the Metadata sub-types into their own modules,
  starting with Title. We've got some utility functions now for this,
  will help to clean it up some more.
- Better errors on parse failure, this will emerge along with more unit testing
- More unit testing
