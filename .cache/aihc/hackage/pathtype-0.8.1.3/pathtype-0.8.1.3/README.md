# Construction of type-safe paths

You can use the construction functions as follows:

    f :: Path.RelFile
    f = relDir "tmp" </> relFile "someFile" <.> "ext"

or ...

    f :: Path.RelFile
    f = dirPath "tmp" </> filePath "someFile" <.> "ext"

or ...

    f :: Path.RelFile
    f = path "tmp" </> path "someFile" <.> "ext"

or just ...

    f :: Path.RelFile
    f = relFile "tmp/someFile.ext"

The first and the last implementations force the most specific types
and thus should be prefered.

Overloaded string literals are no longer supported,
since this extension is intended for alternative text storage types.
It would also decrease the type safety
if you could omit the path type and let the compiler guess its type.


# Modules

You will typically want to import as follows:

    import qualified System.Path.Directory as Dir
    import qualified System.Path.IO as PathIO
    import qualified System.Path as Path
    import System.Path ((</>))

`System.Path.Generic` provides all functions with the OS as type parameter.
`System.Path.Posix` and `System.Path.Windows`
offers only path constructors and destructors
fixed to the corresponding operating system.
`System.Path` exports either `System.Path.Posix` or `System.Path.Windows`
depending on the host system
and additionally the manipulation functions from `System.Path.Generic`.
This mix should be appropriate for the average use
and should free the user from writing type annotations.


# How to choose proper type arguments

The `ar` and the `fd` type parameters have quite different meaning.
The types `Abs` and `Rel` refer to a property of the path,
whereas the type `File` and `Dir` refers to a property of a disk object.
You can decide whether a path is absolute or relative
by just watching (the beginning of) the path string.
In contrast to that, you have to access the disk
in order to check the existence and type of a disk object.
Even more, the disk object might change at any time,
e.g. the user might delete a file and create a directory of the same name,
or the disk object might not exist,
and the purpose of the path is to create an according file or directory.
That's why even if you have a path of type `FilePath ar`,
every function accessing the file must check
that the refered object exists and is a file.
Conversely, there is not much sense in checking the disk object type
and then chosing the path accordingly.
Instead, you must choose the path type according
to what type of disk object your application needs.
The reality check must be performed
and is performed by the standard functions
for every access to the object.
If a disk object is not of the type required by the path type
then this is a runtime exception that must be handled at runtime
but it is not a programming error.

Sometimes you have to change the type of a path
as an intermediate step to construct a path for an object of different type.
E.g. you may convert the path "pkg" from `DirPath` to `FilePath`
because in the next step you like to extend it to "pkg.tar.gz".
This is valid use of the `Path` type.
E.g. the function `dropExtensions`
reduces the `FilePath` "pkg.tar.gz" to the new `FilePath` "pkg"
although no-one expects that there is or will be a file with name "pkg".
Thus, if a function has a `FilePath` parameter
then there is no warranty that it accesses the according file
and does not touch related disk objects.
It may well be that the function derives other file and directory names
from the path and accesses them.
That is, a `FilePath` or `DirPath` parameter
is mainly for documentation purposes
but it cannot prevent you seriously from any damage.


# How to cope with user input

You may get a path from the user, e.g. as command-line argument.
It might be either absolute or relative
and it might refer to an actual file or directory or
to something yet non-existing.
In most cases it will not be important
whether the path is absolute or relative,
thus you should choose the `AbsRel` type parameter.
If somewhere in the program an `Abs` path is needed
then you can assert that the path is actually absolutized somewhere
e.g. by `dynamicMakeAbsolute`.
If you prevent usage of `genericMakeAbsolute`
then you avoid to absolutize a path that is already absolutized.

The choice of the `fd` type parameter follows a different reasoning:
Often you have a clear idea of
whether the user must pass a file or directory path.
The rule is: Just give the path the type you expect
but do not perform any checking
(unless you want to warn the user earlier about imminent danger).
The disk object type must be checked for every access to the object, anyway,
so there is no point in checking it immediately.
With your choice of the `fd` parameter
you just document its intended use.

It might be that the path is only a base name
used to construct other directory and file names.
E.g. for an Audacity project named `music`
you have to create the directory `music_data` and the file `music.aup`.
In this case we recommend to give `music` the type `FilePath`.
This type warrants that there is at least one final path component
in contrast to a directory path that might be empty.
You can easily convert a file path to a directory path
using `Path.dirFromFile`.
The reverse conversion is partial.


# AbsRel vs. `ar` type parameter

In your application you will often know
whether your path denotes a file or directory,
thus you will use the `Path.File` or `Path.Dir` type synonym.
In contrast to that, you will often not know
whether the path is relative or absolute
and often it does not even matter.
You can express this either by using the monomorphic type `Path.AbsRelFile`
or by using the parameterized type `Path.File ar`
with constraint `PathClass.AbsRel ar`.
We recommend to use the first variant for command-line option parsing
and the second one for the application functions.
When you parse options it is the time where you know that you do not know
whether the path is absolute or relative.
That is, you could neither parse it as `Path.Abs` nor as `Path.Rel`.
Thus a type variable makes no sense.
You should always parse to type `Path.AbsRel`.
Conversely, your application would work equally well with
`Path.Abs`, `Path.Rel` and `Path.AbsRel`.
Thus, using an `ar` type variable is the way to go.
You must make sure to use different `ar`-like variables for independent paths.
By equality of `ar`-like type variables
you can also document dependencies of paths for the programmers.


# Command-line argument parsing with `optparse-applicative`

For parsing of path options with the `optparse-applicative` package
you might use the following helper functions:

    module Option where

    import qualified System.Path.PartClass as PathC
    import qualified System.Path as Path
    import qualified Options.Applicative as OP

    path :: (PathC.FileDir fd) => OP.ReadM (Path.AbsRel fd)
    path = OP.eitherReader Path.parse

    pathArgument ::
       PathC.FileDir fd =>
       OP.Mod OP.ArgumentFields (Path.AbsRel fd) -> OP.Parser (Path.AbsRel fd)
    pathArgument = OP.argument path

    pathOption ::
       PathC.FileDir fd =>
       OP.Mod OP.OptionFields (Path.AbsRel fd) -> OP.Parser (Path.AbsRel fd)
    pathOption = OP.option path

It performs minimal checking of path strings as part of option parsing.
E.g. it will report a user error if the user passes "abc/" as a file path.


# File system links

This package does not explicitly handle file system links.
We treat a file path containing links like any other file path.
The same holds for directory paths.
A link is handled like any other path component.


# Drive-relative paths on MS Windows

We use the `Rel` type for paths that can be relative to any directory.
We use the `Abs` type for all other paths,
i.e. for paths with explicit locations or
with restrictions on the set of locations.
Windows has a notion of drives and
maintains a current directory for every drive.
E.g. the path `"c:text.txt"` refers to the current directory of drive `C`.
Since it cannot be freely combined with other directories
we treat this path like an absolute path.
This is consistent with the behaviour of the `filepath` package.
E.g. `filepath` evaluates all of the expressions
`"\\abs" </> "c:driverel"`, `"c:\\abs" </> "c:driverel"`,
`"d:\\abs" </> "c:driverel"` to `"c:driverel"`.
In our package you would have to use `genericMakeAbsolute`
but we recommend to avoid its use.


# Path format strings

You might allow a user to pass a format string such as
`"page%06d.png"` to your program.
How to integrate this into the type-safe path handling?
We recommend not to use the `Path` for the format string
but instead to define a custom type:

    newtype Format ar fd = Format String

    printf ::
       (PathC.AbsRel ar) =>
       Format ar fd -> Int -> Either String (Path ar fd)
    printf (Format fmt) n = Path.parse (Pf.printf fmt n)

This makes sure that you will access a path on disk
only after formatting the path string.


# Known problems

## Mac

Currently we select the Posix module on Mac systems.
On the one hand this choice is right
since Mac uses slashes for path component separation.
On the other hand it is wrong
since Mac ignores case when comparing filepaths.
