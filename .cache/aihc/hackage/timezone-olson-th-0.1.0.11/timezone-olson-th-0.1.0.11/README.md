# timezone-olson-th
Template Haskell to load a TimeZoneSeries from an Olson file at compile time.

For Example:

    myTimeZoneSeries :: TimeZoneSeries
    myTimeZoneSeries = $(loadTZFile "/usr/share/zoneinfo/Europe/Stockholm")

