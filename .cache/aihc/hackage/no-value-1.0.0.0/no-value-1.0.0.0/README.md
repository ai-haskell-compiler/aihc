# no-value

This package provides the `NoValue` class for selecting sentinel-like values
for each member type. These are 'default' values that are not supposed to
represent proper values. They are typically used where you would normally
use an error value but cannot. The idea is that they should be recognisable
as non-values when encountered in logs or diagnostics.
