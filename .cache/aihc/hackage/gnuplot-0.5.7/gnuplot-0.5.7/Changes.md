# Change log for the `gnuplot` Haskell binding

## 0.5.7:

 * `Gnuplot.Terminal.QT`

 * `Gnuplot.Execute`:
   Do longer call `pgnuplot` on Windows.
   It is gone in gnuplot-5.0 in January 2015.

## 0.5.5:

 * `Gnuplot.Utility.quote` bugfix:
   Do not escape printable non-ASCII characters
   because gnuplot has no universal escaping support for Unicode codepoints.
