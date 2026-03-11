Ranged Sets for Haskell
=======================

Ranged sets allow programming with sets of values that are described
by a list of ranges.  A value is a member of the set if it lies within
one of the ranges.  The ranges in a set are ordered and
non-overlapping, so the standard set operations can be implemented by
merge algorithms in O(n) time.

License
-------

Currently the Ranged Set library is under the BSD 3 license.  This is
a very permissive license.  I am hoping that Ranged Sets will
eventually become part of the Base library, and at that point the
implementation will have to be issued under the same license as the
rest of the library (which in practice probably means different
licenses for different compiler versions).  As I understand it the BSD
3 license will allow this without me having to get either assignment
of copyright or explicit permission from everyone who submits
contributions.


Boundaries
----------

Module Data.Ranged.Boundaries defines the Boundary type.  A boundary
divides an ordered type into values above and below the boundary.  No
value can ever sit on a boundary.

Two boundaries are equal if they divide the values at the same point.
This definition of equality causes an implementation problem because
some types are "discrete".  For instance there is no value between the
characters 'a' and 'b', or between the integers 3 and 4.  However
there are values between 3.0 and 4.0.  Similarly for strings, there
are values between "a" and "b" such as "aa", "ab", and so on.  This is
important because "BoundaryAbove 3" is equal to "BoundaryBelow 4".  3
is below both boundaries, and 4 is above both.  Hence they divide the
integers at the same place.  But on the other hand "BoundaryAbove 3.0"
and "BoundaryBelow 4.0" are not equal because 3.5 is above the first
and below the second.

To solve this the DiscreteOrdered class is defined, which provides a
function "adjacent".  Two values x1 and x3 are adjacent if x1 < x3 and
there does not exist an x2 such that x1 < x2 < x3.  This provides the
distinction necessary for boundary equality to be defined for all
ordered types.  The ordered types from the prelude are instances of
DiscreteOrdered, and others can be added by defining "adjacent".  The
functions "enumAdjacent" and "boundedAdjacent" are provided for
instances of Enum and Bounded.  Lists and tuples of DiscreteOrdered
types are also instances of DiscreteOrdered.

This approach was suggested by Ben Rudiak-Gould on
comp.lang.functional.

In theory the Float and Double types should be treated as enumerated
because they are held in fixed-length data fields, and hence must have
pairs of values that are adjacent.  However they are treated as
continuous here for two reasons:

   * The Float and Double types are practical approximations to Real
     numbers, which are continuous.  Hence it makes sense for Float
     and Double to pretend to share this property.

   * There is no standard way to determine the adjacency of Float and
     Double values in Haskell.  "succ 3.0" returns 4.0, which is not
     appropriate here.


Ranges
------

Module Data.Ranged.Ranges defines the Range type.  A range has a lower and an
upper Boundary.

Set-like operations are defined on ranges, but they return variable
numbers of results, and hence return either Maybe Range or [Range].


RangedSet
---------

Module Data.Ranged.RangedSet defines the RSet type.  This is the
actual ranged set type.  It is constructed from a list of ranges.
There are two functions to do this:

   * makeRangedSet takes a finite list of ranges that may overlap or be
     out of order.  It sorts them and merges overlapping ranges using
     the normaliseRangeList function.
     
   * unsafeRangedSet takes a list of ranges that must be in order and not
     overlapping.  The behaviour of the resulting set is not defined if this
     precondition is not met.
     
In theory the standard QuickCheck generator for RSet could generate an
arbitrary list of ranges and then normalise them, but in practice this
tends to produce a very small number of ranges because of the high
probability of overlaps.  So instead an arbitrary list of boundaries
is generated and these are then sorted and paired off into
non-overlapping ranges.


Infinite Sets
-------------

In theory, thanks to lazy evaluation ranged sets can handle infinite
lists of ranges.  These are known as "infinite sets".  Note that this
is not the same as a set with a final upper bound of "AboveAll".

Unfortunately there is no simple way to guarantee that computations on
infinite sets will terminate.  So infinite sets are not supported.

QuickCheck and Tests
--------------------

All the types in the Ranged Set library are instances of Arbitrary from
the QuickCheck library, and the source code includes a number of
important properties for Ranges and RSets defined using QuickCheck.  These 
can be treated as a formal specification of the properties of these types.

The tests can be run by going into the "tests" directory and saying
"make all".  A coverage report is generated, and detailed HTML coverage will
be found in "tests/Report".  "make clean" to delete all the generated files.
