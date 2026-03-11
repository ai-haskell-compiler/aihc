# Usage

The most simple call is:
~~~~
gs-parallel input.pdf page%04d.png -j4 -- -sDEVICE=png16m -dNOPAUSE -dBATCH
~~~~
This converts the document `input.pdf` to a series of PNGs, one for each page.
It splits the range of all pages in four almost equally sized ranges
and forks a ghostscript instance for each of these ranges.


If there is a cluster of pages that requires an especially long render time,
then it might be useful to chop the page set into more smaller chunks.
~~~~
gs-parallel input.pdf page%04d.ppm -j4 --chunk-size 100 \
    -- -sDEVICE=ppmraw -dNOPAUSE -dBATCH
~~~~
This will process a document with 910 pages
in chunks of sizes 100, 100, ..., 100, 10.

If you want a more balanced series of chunk sizes,
use the option `--max-chunk-size` instead:
~~~~
gs-parallel input.pdf page%04d.tif -j4 --max-chunk-size 100 \
    -- -sDEVICE=tif24nc -dNOPAUSE -dBATCH
~~~~
It will process 10 chunks of size 91.


For selecting a subrange of pages,
you must use the options `--first-page` and `--last-page`
instead of the `gs` options `-dFirstPage` and `-dLastPage`,
because `gs-parallel` must know the selected pages.


# Known issues

We do not escape the pdf file path for page counting, this may cause problems.
