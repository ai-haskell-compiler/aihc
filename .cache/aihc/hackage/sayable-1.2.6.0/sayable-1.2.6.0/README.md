This module provides a set of data structures, classes, and operators that
facilitate the construction of a Prettyprinter `Doc` object.

# Motivation

Standard prettyprinting is a monotonic conversion that does not allow for
customization for different uses or environments.  For example, when debugging,
full and explicit information about a structure should be generated, but for
checkpoint logging, a simple overview is usually more appropriate.

This library provides for an additional type parameter that can be used to
control the conversion to a suitably verbose Prettyprinter Doc representation.

This is also highly useful in conjunction with logging to generate successively
more verbose information as the logging verbosity increases.

## Usage

Typical usage is to create a sayable message using the operators defined here and
then extract Prettyprinter `Doc` from the saying and convert it to a printable
format (here, simply using `show` for the default Prettyprinter rendering).

```
import qualified Prettyprinter as PP

foo :: Members '[ Logging SayMessage, Config ] r -> a -> b -> Eff r [b]
foo arg1 arg2 =
   do putStrLn $ show $ saying $ sayable @info "Entering foo with" &- arg1 &- "and" &- arg2
      rslt <- something arg1 arg2
      case rslt of
        Right vals ->
          do putStrLn $ show $ saying $ sayable @"verbose"
                 $ "Foo successfully returning" &% length vals &- "results:" &- vals
             return vals
        Left err ->
          do putStrLn $ show $ saying $ sayable @"error"
                 $ "Foo error (" &- arg1 &- PP.comma &- arg2 &- ") is" &- err
             throwError err
```

There are three messages printed: one on entry and one on either the success or
failure paths.  Each message may have different levels of information reported
for the various arguments.

## The `saytag` type parameter

Each sayable message uses a `TypeApplication` to specify a `saytag` which should
be used for controlling the rendering of that message.  This parameter is
polykinded to provide maximum flexibility, but the most common kind is `Symbol`
(e.g. `"info"`, `"verbose"`, `"error"`, etc.).

Another frequent kind used for the `saytag` is `GHC.TypeNats.Nat`, allowing for
an ordering of saytag types.  However be aware that any instance constraints
(e.g. `saytag <= 9`) are only resolved __after__ the instance head is matched, so
if the constraints do not match no other instances will be tried an an error is
generated.  Thus, rather than use constraints for selecting between instances,
the maximum value for each "range" should be an instance, along with the minimum
extremum:

```
instance {-# OVERLAPPING #-} Sayable (9::Nat) Foo where sayable f = ...[sayable for 9+]
instance {-# OVERLAPPING #-} Sayable (3::Nat) Foo where sayable f = ...[sayable for 3-8]
instance {-# OVERLAPPING #-} Sayable (0::Nat) Foo where sayable f = ...[sayable for 0-2]
instance {-# OVERLAPPABLE #-} (0 <= prevVer, prevVer ~ (ver - 1), Sayable prevVer Foo)
   => Sayable ver Foo where
   sayable = Saying . saying . sayable @Nat @prevVer
```

As a developer, it is encouraged to use whatever saytag makes sense relative to
the current context and type of information being processed.  Most of this
documentation will use the preferred `Symbol` kind for the `saytag`.

## Individual Arguments

The arguments passed to the sayable should be instances of the `Sayable` class.
There are a number of standard instances of `Sayable`, but an instance can be
declared for any object that might be output.  The `Sayable` class has two class
parameters: the second is object to be converted, and the first is the "saytag".
This allows different Sayable instances for an object to be used in different
saytag scenarios.  For example:

```
import Network.URL

instance Sayable "verbose" URL where
  sayable url =
    let newline = PP.line :: PP.Doc SayableAnn
        prettyShow x = PP.viaShow x :: PP.Doc SayableAnn
    in "URL {"
        &- "url_type=" &- prettyShow (url_type url) &- newline
        &- "url_path=" &- url_path url &- newline
        &- "url_params=" &* url_params url
        &- "}"
instance Sayable saytag URL where
  sayable = Sayable . PP.viaShow . exportURL
```

The above would cause a url emitted via a "verbose" saytag to be
expanded into a report on each individual field, whereas all other
saytags would simply output the `exportURL` representation of the `URL`.

```
>>> let host = Host (HTTP True) "github.com" Nothing
>>> url' = URL (Absolute host) "by/one"
>>> saying $ sayable @"verbose" url'
URL { url_type= Absolute (Host {protocol = HTTP True, host= "github.com", port= Nothing})
 url_path= by/one
 url_params= }
>>> saying @"info" $ sayable url'
https://github.com:442/by/one
```

Note that there are several pre-declared `Sayable` instances for common
datatypes for convenience.

## Operators

In the logging lines above, there are several operators used, each of which
starts with the `&` character.  These are described in detail in the 'Helper
operators' section below, but the general mnemonic for these is:

  * A dash is a space between sayable elements

  * A plus is immediately adjacent sayable elements

  * A colon is a separator specification

  * An asterisk is applied to a foldable (i.e. a list)

  * A percent sign preceeds a Pretty object

  * An exclamation follows a Pretty function, which is applied to the following
    argument.

  * A question mark is followed by a Maybe, with no output for a Nothing

  * A less-than character means newline (i.e. return to the left)

These characters will be combined for operators with combination effects.
