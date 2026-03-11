{- |
Module: Text.Sayable

This module provides a set of data structures, classes, and operators that
facilitate the construction of a Prettyprinter Doc object.

= Motivation

Standard prettyprinting is a monotonic conversion that does not allow for
customization for different uses or environments.  For example, when debugging,
full and explicit information about a structure should be generated, but for
checkpoint logging, a simple overview is usually more appropriate.

This library provides for an additional type parameter that can be used to
control the conversion to a suitably verbose Prettyprinter Doc representation.

This is also highly useful in conjunction with logging to generate successively
more verbose information as the logging verbosity increases.

= Usage

Typical usage is to create a sayable message using the operators defined here and
then extract Prettyprinter Doc from the saying and convert it to a printable
format (here, simply using @show@ for the default Prettyprinter rendering).

@
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
@

[Note: if viewing via Haddock HTML, the ampersand in front of @"info"@,
@"verbose"@, and @"error"@ on the putStrLn lines above may not be
visible.]

There are three messages printed: one on entry and one on either the success or
failure paths.  Each message may have different levels of information reported
for the various arguments.

== The @saytag@ type parameter

Each sayable message uses a @TypeApplication@ to specify a "saytag" which should
be used for controlling the rendering of that message (e.g. "info", "verbose", "error", etc.).

As a developer, it is encouraged to use whatever saytag makes sense relative to the current context and type of information being processed.

== Individual Arguments

The arguments passed to the sayable should be instances of the 'Sayable' class.
There are a number of standard instances of 'Sayable', but an instance can be
declared for any object that might be output.  The 'Sayable' class has two class
parameters: the second is object to be converted, and the first is the "saytag".
This allows different Sayable instances for an object to be used in different
saytag scenarios.  For example:

@
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
@

The above would cause a url emitted via a "verbose" saytag to be
expanded into a report on each individual field, whereas all other
saytags would simply output the @exportURL@ representation of the @URL@.

>>> let host = Host (HTTP True) "github.com" Nothing
>>> url' = URL (Absolute host) "by/one"
>>> saying $ sayable @"verbose" url'
URL { url_type= Absolute (Host {protocol = HTTP True, host= "github.com", port= Nothing})
 url_path= by/one
 url_params= }
>>> saying @"info" $ sayable url'
https://github.com:442/by/one

There are some tricky elements to the above however; see "Unfortunate
Details" below.

Note that there are several pre-declared Sayable instances for common
datatypes for convenience.

== Operators

In the logging lines above, there are several operators used, each of which
starts with the @&@ character.  These are described in detail in the 'Helper
operators' section below, but the general mnemonic for these is:

  * A dash is elements separated by a space

  * A plus indicates immediately adjacent elements

  * A colon is designates a separator

  * An asterisk is applied to a foldable (i.e. a list)

  * A percent sign preceeds a Pretty object

  * An exclamation follows a Pretty function, which is applied to the following
    argument.

  * A question mark is followed by a Maybe, with no output for a Nothing

  * A less-than character means newline (i.e. return to the left)

These characters will be combined for operators with combination effects.


== Convenience/other

  * This module also provides an instance to convert a Sayable back
    into a Prettyprinter.Pretty.

  * This module provides a helper function: 't'' which can be useful
    when @OverloadedStrings@ is active to designate its argument as
    being a Text string.

    If the following:

@
saying @"error" $ "This is an error:" &- err
@

    results in an error @Could not deduce (Data.String.IsString m0)
    arising from the literal '"This is an error:"'@ then this helper
    can fix that:

@
saying @"error" $ t'"This is an error:" &- err
@

  * This module provides a helper function: 'd'' which can be useful
    when creating a PP.Doc SayableAnn for inclusion into a 'Sayable'
    by fixing the @ann@ of 'PP.Doc ann' to 'SayableAnn'.

    Fixes the error:

@
    • Overlapping instances for Sayable saytag (PP.Doc ann1)
        arising from a use of ‘&-’
      Matching instances:
        instance [overlappable] Sayable tag (PP.Doc ann)
          -- Defined in ‘Taphos.Say’
        instance Sayable tag (PP.Doc SayableAnn)
          -- Defined in ‘Taphos.Say’
      (The choice depends on the instantiation of ‘saytag, ann1’
       To pick the first instance above, use IncoherentInstances
       when compiling the other instance declarations)
@

    This is similar to the '&%' operator except it takes a single
    argument rather than the two arguments passed to the operator.

  * The pattern of converting a saying into a String (e.g. for passing to
    putStrLn) is fairly common, so the simplistic operations of that is provided
    by the 'sez' function.


== Generating final output

  The 'sayable' method of the 'Sayable' class generates instances of
  the 'Saying' data object.  The 'saying' function can be used to
  extract the 'Prettyprinter.Doc' from the 'Saying' object.  This Doc
  can then be converted to a 'Lumberjack.LogMessage' or to a plain
  Text format for display.

== Unfortunate Details

  The use of the Sayable class to translate individual objects is
  fairly straightforward, but the management of the phantom @saytag@
  type parameter is a bit tricky.  As described above (with the
  Network.URL example), it's possible to provide different output
  generation by providing specialized instances for specific saytags.
  The determination of which instance GHC will use has some
  idiosyncrasies that make lead to unexpected instance selection when
  used transitively (viz.
  https://ghc.gitlab.haskell.org/ghc/doc/users_guide/extsinstances.html).

  For instance:

@
import Network.URL ( URL )
newtype Foo = Foo URL
data Bar a  = Bar String a

-- [previous instances for Sayable URL here...]

instance Sayable "loud" Foo where sayable (Foo url) = t'"{!" &- url &- t'"!}"
instance Sayable saytag Foo where sayable (Foo url) = sayable url

instance (Sayable saytag a) => Sayable saytag (Bar a) where
  sayable (Bar b a) = b &- t'"is" &- a

let host = Host (HTTP True) "github.com" Nothing
let url' = URL (Absolute host) "by/one"
let foo = Foo url'
let bar = Bar "bar" foo
@

  will generate:

>>> putStrLn $ sez @"info" $ t'"INFO:" &- bar &- "via" &- foo
INFO: bar is "https://github.com/by/one" via "https://github.com/by/one"
>>> putStrLn $ sez @"loud" $ t'"LOUD:" &- bar &- "via" &- foo
LOUD: bar is {! "https://github.com/by/one" !} via {! "https://github.com/by/one" !}

  which is expected.  However, if the calls to 'sez' are moved
  to a separate file from the instance declarations, the compilation
  error will be:

  @Overlapping instances for Sayable "loud" Foo arising from a use of &-@

  for the last (loud) line.  To resolve this, use OVERLAPPING and/or
  OVERLAPPABLE specifications on the instance declarations.  Usually
  it's sufficient (and easiest) to add the OVERLAPPABLE to the generic
  instance:

> instance Sayable "loud" Foo where sayable (Foo s) = t'"{!" &- s &- t'"!}"
> instance {-# OVERLAPPABLE #-} Sayable saytag Foo where sayable (Foo s) = sayable s

  [Note: if you are viewing the above via Haddock HTML, the second
  line has the @instance@ keyword, followed by an open comment
  directive (open curly brace, dash, hash) , @OVERLAPPABLE@ and a
  closing comment directive (hash, dash, close curly brace), followed
  by the @Sayable@ keyword, but that doesn't render under HTML Haddock
  (circa 2022).]

=== Sub-Element Constraints

  There's another annoyance to address when using Sayable: the need for explicit
  contraints for data structure sub-elements.  To observe this new twist, add a
  @Baz@ datastructure and its generic 'Sayable' instance to the previous
  section's example:

  > data Baz = Baz Foo
  > instance Sayable saytag Baz where sayable (Baz a) = t'"BAZ :=" &- foo

  Now the following calls and corresponding output can be observed:

>>> putStrLn $ sez @"info" $ t'"INFO:" &- bar &- t'"and" &- baz
INFO: bar is "https://github.com/by/one" and BAZ := "https://github.com/by/one"
>>> putStrLn $ sez @"loud" $ t'"LOUD:" &- bar &- t'"and" &- baz
LOUD: bar is {! "https://github.com/by/one" !} and BAZ := "https://github.com/by/one"

  Notice how the @foo@ value in @bar@ changes when the '@"loud"' saytag is
  used, but the same @foo@ value in @baz@ does not change!

  The difference here is in the mechanism GHC uses to select instances
  (as described on the referenced link above).  In short, for @bar@, the
  generic 'Sayable' instance has a constraint for the inner element,
  which causes GHC to wait until the final use case to determine what
  the specific type parameters are; it sees the @"loud"@ @saytag@ value
  and selects the @"loud"@ @Foo@ 'Sayable' instance as the most specific.
  However, the @baz@ 'Sayable' instance does not have a constraint, so GHC
  takes the conservative approach and uses the most general instance,
  which means that it transitively selects the generic @Foo@ 'Sayable'
  instance instead of the @"loud"@ instance.

  There are two ways to fix this:

  1. Provide explicit @"loud"@ 'Sayable' instance for @Baz@.  This is
     problematic, because this must be done for *each* saytag for
     which there is a variation and it must be done for *each* upper
     level 'Sayable' instance.

  2. Provide 'Sayable' constraints for each sub-element.  This generates
     larger type signatures, but is preferrable to solution 1 because
     it makes no assumptions about current or future saytags and
     variations.

     This 'Sayable' constraint was already present on the @Bar@
     'Sayable' instance because of the parameterized type for @Bar@;
     the @Baz@ type has no type parameter, but a constraint can still
     be added for each interior type:

> instance Sayable saytag Foo => Sayable saytag Baz where
>   sayable (Baz a) = t'"BAZ :=" &- foo

     To facilitate generating the needed set of constraints for sub-elements
     (including ensuring that a sub-element isn't missed when writing these by
     hand), there is a Template Haskell helper that will automatically generate
     these constraints:

> instance $(sayableSubConstraints (const True) ''Baz "tag" []) => Sayable tag Baz where ...

     See the 'sayableSubConstraints' documentation for more information on using
     this Template Haskell helper.


  Using either of the above solutions, the new output is fully
  specialized as desired:

>>> putStrLn $ sez @"info" $ t'"INFO:" &- bar &- t'"and" &- baz
INFO: bar is "https://github.com/by/one" and BAZ := "https://github.com/by/one"
>>> putStrLn $ sez @"loud" $ t'"LOUD:" &- bar &- t'"and" &- baz
LOUD: bar is {! "https://github.com/by/one" !} and BAZ := {! "https://github.com/by/one" !}

-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Text.Sayable
  (
    -- * Primary Class
    Sayable(sayable)
    -- * Result Datatype
  , Saying(Saying, saying)
    -- * Helper operators
    --
    -- | These are typically used to facilitate the expression of a sayable
    -- phrase containing multiple elements.
  , t'
  , d'
  , (&-)
  , (&+)
  , (&%)
  , (&*)
  , (&+*)
  , (&:*)
  , (&?)
  , (&+?)
  , (&<)
  , (&<*)
  , (&<?)
  , (&!)
  , (&!?)
  , (&!*)
  , (&!$*)
  , (&!:*)
    -- * Annotation used in Sayables
    --
    -- | Generating a 'Prettyprinter.Doc' requires the identification of an @ann@
    -- type parameter.  For 'Sayable', this type parameter is the 'SayableAnn',
    -- although the 'Prettyprinter.reAnnotate' function can be used to switch
    -- from this abstract annotation to a functional annotation
    -- (e.g. 'Prettyprinter.Render.Terminal.AnsiStyle')
  , SayableAnn(SayableAnn)
    -- * Simple String Extraction
  , sez
  , sez_
    -- * Sub-element Sayable constraints
  , sayableSubConstraints
  , ConstrM
  , ofType
  , tagVar
  , tagSym
  , subWrapper
  , subElemFilter
  , paramVar
  , paramSym
  , paramNat
  , paramTH
  )
where

import           Control.Applicative ( liftA )
import           Control.Monad ( ap )
import qualified Control.Monad.Catch as X
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Either ( rights )
import qualified Data.Int as I
import qualified Data.Map as Map
import           Data.Text ( Text, pack )
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TEL
import qualified Data.Word as W
import           GHC.Exts ( Proxy#, proxy# )
import           GHC.OverloadedLabels
import           GHC.TypeLits ( Symbol, KnownSymbol, symbolVal' )
import           Language.Haskell.TH as TH
import           Language.Haskell.TH.Datatype
import           Numeric.Natural ( Natural )
import           Prettyprinter ( (<+>) )
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.String as PPS


-- | The main class of things that can be passed to 'sayable'.  Arguments
-- provided to 'sayable' or 'sez' will be converted to the sayable form by
-- automatically applying the appropriate instance of this class.  The
-- default implementation is:
--
-- > sayable = Saying . Prettyprinter.pretty

class Sayable (tag :: Symbol) v where
  sayable :: v -> Saying tag

  default sayable :: PP.Pretty v => v -> Saying tag
  sayable = Saying . PP.pretty


-- | The result of applying the sayable method of the Sayable class is
-- the Saying object.  This object is internal to the Say module and
-- is mostly used for subsequently combining with additional Saying
-- objects to produce the final Saying object that is converted to a
-- SayMessage for actual logging.  A Sayable supports a Semigroup
-- combinator to allow composition of messages.

newtype Saying (tag :: Symbol) = Saying { saying :: PP.Doc SayableAnn }

instance Semigroup (Saying tag) where
  Saying sm1 <> Saying sm2 = Saying $ sm1 <+> sm2


-- | Inputs that are 'Sayable', i.e. that can be converted to a Saying

instance {-# OVERLAPPING #-} (tagA ~ tagB) => Sayable tagA (Saying tagB) where sayable = id
instance Sayable tag Text
instance Sayable tag String
instance Sayable tag Char
instance Sayable tag Bool
instance Sayable tag Int
instance Sayable tag Integer
instance Sayable tag I.Int32
instance Sayable tag I.Int64
instance Sayable tag W.Word8
instance Sayable tag W.Word16
instance Sayable tag W.Word32
instance Sayable tag W.Word64
instance Sayable tag Natural
instance Sayable tag Float
instance Sayable tag Double
instance Sayable tag TL.Text
instance Sayable tag BS.ByteString where sayable = sayable . TE.decodeUtf8
instance Sayable tag BSL.ByteString where sayable = sayable . TEL.decodeUtf8
instance Sayable tag X.SomeException where sayable = sayable . X.displayException
instance Sayable tag (PP.Doc SayableAnn) where sayable = Saying
instance {-# OVERLAPPABLE #-} Sayable tag (PP.Doc ann) where sayable = Saying . PP.unAnnotate

-- | A Saying can be converted back into a PP.Pretty instance
-- representation.  Just saying... :-)
--
-- This can be convenient to apply Prettyprinter formatting elements.
-- For example:
--
-- > instance Sayable saytag Foo where
-- >   sayable foo = sayable @saytag $ Prettyprinter.group $ Prettyprinter.pretty
-- >                 $ field1 foo &- sayable @saytag PP.line &- field2 foo
--
-- This uses 'Prettyprinter.group' and 'Prettyprinter.line' formatters to show
-- the two fields on the same line if they will fit, otherwise stacked on top of
-- each other.  Note that the second portion needs an explicit @TypeApplication@
-- (applied here to the 'Prettyprinter.line') because the 'Prettyprinter.group'
-- and 'Prettyprinter.pretty' functions do not propagate that outer @saytag@ to
-- the inner portion.

instance PP.Pretty (Saying tag) where pretty = PP.unAnnotate . saying


-- | A helper operator allowing two Sayable items to be composed into
-- a Saying.  This is the most common operator used to construct
-- composite Sayable messages.  The two Sayable items are separated by
-- a space.
--
-- >>> sez @"info" $ t'"hello" &- t'"world"
-- "hello world"
--
(&-) :: forall saytag m n . (Sayable saytag m, Sayable saytag n)
     => m -> n -> Saying saytag
m &- n = sayable m <> sayable n
infixl 1 &-

-- | A helper operator allowing two Sayable items to be composed into
-- a Saying by placing the two Sayable items immediately adjacent with
-- no intervening spaces.  This is the high-density version of the
-- more common '&-' operator.
--
-- >>> sez @"info" $ t'"hello" &+ t'"world"
-- "helloworld"
--
(&+) :: forall saytag m n . (Sayable saytag m, Sayable saytag n)
     => m -> n -> Saying saytag
m &+ n = Saying $ (saying $ sayable @saytag m) <> (saying $ sayable @saytag n)
infixl 1 &+

-- | A helper operator allowing a Sayable item to be composed with a
-- Pretty item into a Saying.  This is infrequently used and primarily
-- allows the composition of a data object which has a "Prettyprinter"
-- instance but no 'Sayable' instance.
--
-- >>> sez @"info" $ t'"hello" &% (t'"world", t'"!")
-- "hello (world, !)"
--
(&%) :: (Sayable tag m, PP.Pretty n) => m -> n -> Saying tag
m &% n = sayable m <> sayable (PP.pretty n :: PP.Doc SayableAnn)
infixl 1 &%

-- | A helper operator to /apply/ a "Prettyprinter" (@Doc ann -> Doc
-- ann@) function (the first argument) to the Sayable in the second
-- argument.  This is different from the '&%' operator in that the
-- former uses 'Prettyprinter.hcat' to join two independent
-- 'Prettyprinter.Doc' 'Saying' values, whereas this operator applies
-- a transformation (e.g. @Prettyprinter.annotate AnnValue@ or
-- @Prettyprinter.align . Prettyprinter.group@) to the
-- 'Prettyprinter.Doc' in the second 'Saying' argument.
--
-- >>> sez @"info" $ PP.group &! t'"hi"
-- "hi"
--
(&!) :: forall tag m . Sayable tag m
     => (PP.Doc SayableAnn -> PP.Doc SayableAnn) -> m -> Saying tag
pf &! m = Saying $ pf $ saying $ sayable @tag m
infixl 2 &!

-- | A helper operator allowing a Sayable item to be composed with a
-- Foldable series of Sayable items.  This can be used when the second
-- argument is a List, Sequence, Set, etc. to add all elements of the
-- set (comma-separated).
--
-- Note: this instance makes it easy to output lists, Sequence,
-- NonEmpty.List, etc., but it can have undesireable effects for data
-- structures whose Foldable (Functor) is irregular... for example,
-- folding over a tuple only returns the 'snd' value of a tuple.
-- Consider wrapping tuples in a newtype with an explicit Sayable to
-- avoid this.
--
-- >>> sez @"info" $ t'"three:" &* [1, 2, 3::Int]
-- "three: 1, 2, 3"
--
-- If the second argument is a null collection then no output is generated for
-- it.
(&*) :: forall tag m e t
        . (Sayable tag m, Sayable tag e, Foldable t) => m -> t e -> Saying tag
m &* l = let addElem e (s, Saying p) =
               ("," <> PP.softline, Saying $ saying (sayable @tag e) <> s <> p)
         in sayable m <> (snd $ foldr addElem ("", Saying PP.emptyDoc) l)
infixl 1 &*

-- | A helper operator that generates a sayable from a foldable group (e.g. list)
-- of sayable items.  This helper is linke the '&*' operator except that the
-- folded output is immediately adjacent to the preceeding sayable output instead
-- of separated by a space; this is useful for situations where the folded output
-- has delimiters like parentheses or brackets.
--
-- >>> sez @"info" $ t'"three:" &- '(' &+* [1,2,3::Int] &+ ')'
-- "three: (1, 2, 3)"
--
-- If the second argument is an empty collection then no output is generated for
-- it.

(&+*) :: forall tag m e t
        . (Sayable tag m, Sayable tag e, Foldable t) => m -> t e -> Saying tag
m &+* l = let addElem e (s, Saying p) =
               ("," <> PP.softline, Saying $ saying (sayable @tag e) <> s <> p)
         in Saying (saying (sayable @tag m)
                    <> saying (snd $ foldr addElem ("", Saying PP.emptyDoc) l))
infixl 1 &+*

-- | A helper operator that generates a sayable from a list of sayable items,
-- separated by the first sayable argument (instead of the ", " that use used by
-- the '&*' operator).
--
-- >>> sez @"info" $ t'"three:" &- t'".." &:* [1, 2, 3::Int]
-- "three: 1..2..3"
--
(&:*) :: forall tag m e t
         . (Sayable tag m, Sayable tag e, Foldable t) => m -> t e -> Saying tag
m &:* l = let addElem e (s, Saying p) = (Just m,
                                         case s of
                                           Nothing -> sayable @tag e &+ p
                                           Just s' -> sayable @tag e &+ s' &+ p
                                        )
          in snd $ foldr addElem (Nothing, Saying PP.emptyDoc) l
infixl 2 &:*

-- | A helper operator that is a combination of the '&!' and '&*' operators.  It
-- applies the first argument (which converts an array of 'Prettyprinter.Doc ann'
-- elements into a single 'PrettyPrinter.Doc ann' element) to the second argument
-- (which is a Foldable collection of 'Sayable' items).
--
-- >>> sez @"info" $ t'"three:" &- PP.align . PP.vsep &!* [1, 2, 3::Int]
-- "three: 1, \n       2, \n       3"
--
(&!*) :: forall tag m t
         . (Sayable tag m, Foldable t)
      => ([PP.Doc SayableAnn] -> PP.Doc SayableAnn) -> t m -> Saying tag
pf &!* l = let addElem e (s, p) = ("," <> PP.softline
                                  , saying (sayable @tag e) <> s : p)
           in Saying $ pf $ snd $ foldr addElem ("", []) l
infixl 2 &!*


-- | A helper operator that applies the first argument (a Prettyprinter
-- adaptation function) to the result of a Foldable collection of 'Sayable'
-- items.  This is essentially a combination of the '&!' and '&*' operators where
-- the first operation is applied to the entire list, rather than each element of
-- the list (as with `&!*`).
--
-- >>> sez @"info" $ t'"three:" &- PP.align &!$* [1, 2, 3::Int]
-- "three: 1, 2, 3"
--
-- As with the '&!*' operator (and unlike the '&*' operator), a null collection
-- is passed to the converter first argument.
--
-- @since: 1.1.0.0
(&!$*) :: forall tag m t
         . (Sayable tag m, Foldable t)
      => (PP.Doc SayableAnn -> PP.Doc SayableAnn) -> t m -> Saying tag
pf &!$* l = let addElem e (s, p) = ("," <> PP.softline
                                   , saying (sayable @tag e) <> s <> p)
           in Saying $ pf $ snd $ foldr addElem ("", mempty) l
infixl 2 &!$*


-- | A helper operator that applies the first argument (which converts
-- an array of 'Prettyprinter.Doc ann' elements to a single
-- 'PrettyPrinter.Doc ann' element) to the second argument, which is a
-- Foldable collection of 'Sayable' items.  This is essentially a
-- combination of the '&!' and '&:*' operators.
--
-- Unlike the other operators defined in this package, this is a trinary operator
-- rather than a binary operator.  Because function application (whitespace) is
-- the highest precedence, the last argument will typically need a preceeding $
-- to prevent applying the second argument to the third argument before applying
-- this operator.
--
-- >>> sez @"info" $ t'"three:" &- (PP.align . PP.vsep &!:* (t'" or")) [1, 2, 3::Int]
-- "three: 1 or\n       2 or\n       3"
--
(&!:*) :: forall tag m t b . (Sayable tag b, Sayable tag m, Foldable t)
       => ([PP.Doc SayableAnn] -> PP.Doc SayableAnn) -> b -> t m -> Saying tag
pf &!:* b = let addElem e (s, p) =
                  (Just b, (case s of
                               Nothing -> saying (sayable @tag e)
                               Just x -> saying (sayable @tag e &+ x)
                           ) : p)
            in Saying . pf . snd . foldr addElem (Nothing, [])
infixl 2 &!:*


-- | A helper operator allowing a Sayable item to be wrapped in a
-- 'Maybe'.  This adds the 'Sayable' of the first argument to the
-- 'Sayable' of the second argument in the 'Just' case, or just emits
-- the 'Sayable' of the first argument if the second argument is
-- 'Nothing'.
--
-- >>> sez @"info" $ t'"It's" &? Just (t'"something") &- t'"or" &? (Nothing :: Maybe Text)
-- "It's something or"
--
(&?) :: forall tag m e
        . (Sayable tag m, Sayable tag e) => m -> Maybe e -> Saying tag
m &? Nothing = sayable m
m &? (Just a) = sayable m <> sayable a
infixl 1 &?


-- | A helper operator allowing a Sayable item to be wrapped in a 'Maybe' and a
-- prettyprinter conversion as the first argument.  This is a combination of the
-- `&!` and `&?` operators.
--
-- >>> sez @"info" $ PP.group &!? Just (t'"hi")
-- "hi"
--
-- @since: 1.1.0.0
(&!?) :: forall tag e . (Sayable tag e)
      => (PP.Doc SayableAnn -> PP.Doc SayableAnn) -> Maybe e -> Saying tag
_ &!? Nothing = Saying mempty
pf &!? (Just a) = Saying $ pf $ saying $ sayable @tag a
infixl 1 &!?


-- | A helper operator that generates a newline between its two arguments.  Many
-- times the '&-' operator is a better choice to allow normal prettyprinter
-- layout capabilities, but in situations where it is known that multiple lines
-- will or should be generated, this operator makes it easy to separate the
-- lines.
--
-- >>> sez @"info" $ t'"Hello" &< t'"world"
-- "Hello\nworld"
--
-- @since: 1.1.0.0
(&<) :: forall saytag m n . (Sayable saytag m, Sayable saytag n)
     => m -> n -> Saying saytag
m &< n = Saying
         $ (saying $ sayable @saytag m)
         <> (PP.line :: PP.Doc SayableAnn)
         <> (saying $ sayable @saytag n)
infixl 1 &<


-- | A helper operator that combines '&<' and '&*' which will generate a newline
-- between its two arguments, where the second argument is a foldable collection
-- whose elements will be sayable emitted with comma separators.
--
-- >>> sez @"info" $ t'"three:" &<* [1, 2, 3::Int]
-- "three:\n1, 2, 3"
--
-- @since: 1.1.0.0
(&<*) :: forall saytag m n t . (Sayable saytag m, Sayable saytag n, Foldable t)
      => m -> t n -> Saying saytag
m &<* n = let addElem e (s, Saying p) =
                (", ", Saying $ saying (sayable @saytag e) <> s <> p)
          in Saying
             $ (saying $ sayable @saytag m)
             <> (PP.line :: PP.Doc SayableAnn)
             <> (saying $ sayable @saytag
                  (snd $ foldr addElem ("", Saying PP.emptyDoc) n))
infixl 1 &<*


-- | A helper operator that emits the first argument and optionally emits a
-- newline and the 'Just' value of the second argument if the second argument is
-- not 'Nothing' (a combination of the '&<' and '&?' operators).
--
-- >>> sez @"info" $ t'"First" &<? Just (t'"something")
-- "First\nsomething"
-- >>> sez @"info" $ t'"Then" &<? (Nothing :: Maybe Text)
-- "Then"
--
-- @since: 1.1.0.0
(&<?) :: forall saytag m n . (Sayable saytag m, Sayable saytag n)
      => m -> Maybe n -> Saying saytag
m &<? Nothing = sayable m
m &<? (Just n) = Saying
                 $ (saying $ sayable @saytag m)
                 <> (PP.line :: PP.Doc SayableAnn)
                 <> (saying $ sayable @saytag n)
infixl 1 &<?


-- | A helper operator that emits the first argument and optionally emits a the
-- 'Just' value of the second argument immediately thereafter if the second
-- argument is not 'Nothing'
--
-- >>> sez @"info" $ t'"It's" &+? Nothing &- t'"ok" &+? Just "time"
-- "It's oktime"
--
-- @since: 1.2.0.0
(&+?) :: forall saytag m n . (Sayable saytag m, Sayable saytag n)
      => m -> Maybe n -> Saying saytag
m &+? Nothing = sayable m
m &+? (Just n) = Saying
                 $ (saying $ sayable @saytag m)
                 <> (saying $ sayable @saytag n)
infixl 1 &+?


-- | A helper function to use when @OverloadedStrings@ is active to identify the
-- following quoted literal as a "Data.Text" object.  It is common to enable
-- OverloadedStrings because 'Prettyprinter.Pretty' declares an 'Data.String.IsString'
-- instance and thus facilitates the pretty-printing of string values, but this
-- causes GHC to emit warnings about assuming the types of strings, so this
-- function can be used to clarify the intended type.
--
-- >>> putStrLn $ t'"This is type: Data.Text"
-- "This is type: Data.Text"
--
t' :: Text -> Text
t' = id
{-# INLINE t' #-}


-- | A helper function to use when creating a PP.Doc SayableAnn data
-- object (i.e. fixing the @ann@ of 'Doc ann' to 'SayableAnn')
d' :: PP.Pretty n => n -> PP.Doc SayableAnn
d' = PP.pretty


----------------------------------------------------------------------

-- | This is the default annotation type for the Saying module.  The
-- Prettyprinter reannotate operation can be used to change this annotation into
-- any other annotation type the client desires.
--
-- The SayableAnn is an instance of IsLabel, so if OverloadedLabels is enabled,
-- this can easily be specified:
--
-- @
-- import qualified Prettyprinter as PP
-- import Text.Sayable
--
-- putStrLn $ sez @"info" $ PP.annotate #myann $ "Hello" &- "world!"
-- @
--
-- Note however that labels cannot start with a capital letter.

data SayableAnn = SayableAnn Text

instance KnownSymbol ann => IsLabel (ann :: Symbol) SayableAnn where
  fromLabel = SayableAnn $ pack $ symbolVal' (proxy# :: Proxy# ann)


----------------------------------------------------------------------

-- | This is a convenience function that can be used for simple conversions of a
-- Sayable to a String.  The use of this function is not generally recommended: a
-- more controlled rendering of the resulting 'Prettyprinter.Doc' (obtained from
-- the via 'saying') is recommended, but there are times (especially when
-- debugging) when a quick conversion/extraction to a @String@ is convenient.
--
--  This function is often used with a type application:
--
--  > putStrLn $ sez @"info" $ "There are" &- length lst &- "list elements."
--
-- Note that this will use the 'show' representation provided by 'Prettyprinter';
-- notably this will usually assume a width of 80 characters and perform wrapping
-- accordingly.

sez :: forall saytag a . Sayable saytag a => a -> String
sez = show . saying . sayable @saytag

-- | This is a convenience function similar to the 'sez' helper, but specifies an
-- unlimited width so there is no wrapping.

sez_ :: forall saytag a . Sayable saytag a => a -> String
sez_ = PPS.renderString
       . PP.layoutPretty (PP.LayoutOptions PP.Unbounded)
       . saying . sayable @saytag


----------------------------------------------------------------------

-- | When creating 'Sayable' instances, it is necessary to create a Sayable
-- constraint for all sub-element data structures so that GHC will search for the
-- specific tagged instance, otherwise GHC will use a default instance that is
-- not necessarily associated with the current tag (see the "Sub-Element
-- Constraints" section above for more information).
--
-- The 'sayableSubConstraints' function is a Template Haskell helper that can
-- automatically generate the constraints for the sub-elements of this
-- datastructure.  This has several advantages, including brevity and ensuring
-- that no sub-elements are missed.  The 'sayableSubConstraints' is a function
-- taking a 'ConstrM' monad where that monad specifies the various information
-- (via the 'ConstrM' operations defined below) that are needed to generate the
-- sub-element constraints.
--
-- To use this, you will need to enable the @ConstraintKinds@ and
-- @TemplateHaskell@ pragmas.
--
-- With these enabled, the instance declaration would
-- be specified as:
--
-- > instance $(sayableSubConstraints $ ofType ''Baz) => Sayable saytag Foo where ...
--
-- If there are other constraints that should also be included, those can be
-- specified in a standard constraint tuple:
--
-- @
-- instance ( $(sayableSubConstraints $ ofType ''Baz)
--          , Show Bar
--          ) => Sayable tag Foo where ...
-- @
--
-- The 'sayableSubConstraints' function will examine the definition of the type
-- referenced by the second argument, and for every sub-type referenced (that is
-- accepted by any 'subElemFilter' specified), it will generate a Sayable
-- constraint for the sub type(s).
--
-- @
-- data Bar = ...
-- data Baz = ...
-- data Foo = Foo { fld1 :: Bar
--                , fld2 :: [Baz]
--                , fld3 :: Maybe Bar
--                }
--
-- instance $(sayableSubConstraints $ do ofType ''Foo
--                                       tagVar "stag"
--           ) => Sayable stag Foo where
--   sayable foo = ...
-- @
--
-- becomes (via the magic of Template Haskell):
--
-- @
-- instance ( Sayable stag Bar
--          , Sayable stag Baz
--          ) => Sayable stag Foo where
--   sayable foo = ...
-- @
--
-- The 'subElemFilter' 'ConstrM' operation can be used to select only a subset of
-- the sub-elements for this constraints generation.  For example, with the
-- following definition:
--
-- @
-- data Foo2 = FC1 Bar [Maybe Baz] | FC2 Bar Int HiddenValue
-- @
--
-- The Sayable instance for Foo2 does not need a tag-specific constraint for the
-- Int type, and the instance will not output the @fld5@ hidden value, so no
-- 'Sayable' instance constraint is needed (and in fact, for safety, no actual
-- 'Sayable' instance will ever be created for @HiddenValue@).  To support this,
-- a filtering function can be used for the "Language.Haskell.TH.Name" type
-- references:
--
-- @
-- module MyModule where
--
-- import qualified Language.Haskell.TH as TH
--
-- data Bar = ...
-- data Baz = ...
-- data HiddenValue = ...
-- data Foo2 = FC1 Bar [Baz] | FC2 (Maybe Bar) Int HiddenValue
--
-- foo2Filter :: TH.Name -> Bool
-- foo2Filter nm = and [ \"HiddenValue\" /= TH.nameBase nm
--                     , maybe False (\"MyModule\" ==) $ TH.nameModule nm
--                     ]
--
-- instance $(sayableSubConstraints $ do ofType ''Foo2
--                                       tagVar "t"
--                                       subElemFilter foo2Filter
--           ) => Sayable t Foo2 where
--   sayable = \case
--       FC1 x y -> "First Foo2 form with" &- x &- "and" &- y
--       FC2 x y h -> "Second Foo2 form with" &? x &- y
--
-- instance Sayable t Bar where ...
-- instance Sayable t Baz where ...
-- @
--
-- which generates:
--
-- @
-- instance ( Sayable t Bar, Sayable t Baz ) => Sayable t Foo2 where ...
-- @
--
-- When the sub-elements are themselves parameterized, it is necessary to specify
-- what those parameters should map to: either the same parameter variables that
-- occur on the main type, or other types or values based on the usage of the
-- main type.  This can be done using the 'paramVar', 'paramSym', 'paramNat', or
-- 'paramTH' 'ConstrM' operations.  Each of these param declarations specifies
-- one of the parameters for sub-elements. All sub-elements must specify the
-- parameters in the same order, although each does not need to use all of the
-- parameters; sub-element types that do not follow this arrangement will need to
-- be manually specified instead (and filtered out of the template-haskell
-- generated constraints via the 'subElemFilter').
--
-- @
-- data Bar a = Bar (Maybe a)
-- data Baz a b = BazL a | BazR b
--
-- data Foo3 a = Foo3 { inputs :: Bar a, outputs :: Baz a String }
--
-- instance $(sayableSubConstraints $ do ofType ''Foo3
--                                       tagVar "t"
--                                       paramVar "a"
--                                       paramTH $ TH.ConT ''String
--           ) => Sayable t (Foo3 a) where ...
-- @
--
-- generates the following:
--
-- @
-- instance ( Sayable t (Bar a), Sayable t (Baz a String), Sayable t a ) => Sayable (Foo3 a) where ...
-- @
--
-- Clearly, there are some limitations to the 'sayableSubConstraints'
-- capabilities, so it is not always useable in all situations (e.g. with
-- tuples).  The sub-element constraints can and should be manually generated in
-- these situations.
--
-- In order to debug the output of the 'sayableSubConstraints' or otherwise
-- examine its suitability, enable @-ddump-splices@ when compiling.
--
sayableSubConstraints :: ConstrM () -> PredQ
sayableSubConstraints cspec =
  let initCtx = SCCtx { cTgt = ''()
                      , cFilt = const True
                      , cSaytag = Right "saytag"
                      , cVars = mempty
                      , cWrapper = Nothing
                      }
      (ctx, _) = runConstrM cspec initCtx
  in sayableSubConstraints' (cFilt ctx) (cTgt ctx) (cSaytag ctx) (cWrapper ctx) (cVars ctx)


-- | ConstrM is a monadic context for describing the constraint parameters needed
-- by 'sayableSubConstraints' when generating sub-element Sayable constraints.
newtype ConstrM a = ConstrM { runConstrM :: SCCtx -> (SCCtx, a) }

instance Applicative ConstrM where
  pure x = ConstrM $ \c -> (c, x)
  (<*>) = ap

instance Functor ConstrM where fmap = liftA

instance Monad ConstrM where
  return = pure
  m >>= k = ConstrM $ \c -> let (c', a) = runConstrM m c in runConstrM (k a) c'


-- | This ConstrM operation is used to declare the target data type for which the
-- Sayable constraints are to be generated via template-haskell.
--
-- > instance $(sayableSubConstraints $ do { ofType ''Foo; ... }) => Sayable t Foo where
--
-- If not used, the default type is @()@, which is not likely to be the desired
-- type.
ofType :: Name -> ConstrM ()
ofType nm = ConstrM $ \c -> (c { cTgt = nm }, ())

-- | This ConstrM operation is used to declare the name of the saytag variable
-- used to specify the "Saying VAR sub-element" constraints.  This should match
-- the variable for the primary instance declaration.
--
-- > instance $(sayableSubConstraints $ do { ...; tagVar "t" }) => Sayable t X where ...
--
-- If not used, the default saytag is the variable @saytag@.  This operation is
-- coincident with the 'tagSym', 'tagNat', and 'tagTH' operations and the last
-- such operation will be the one used.
tagVar :: String -> ConstrM ()
tagVar var = ConstrM $ \c -> (c { cSaytag = Right var }, ())

-- | This ConstrM operation is used to declare a Symbol singleton value for the
-- saytag of the sub-element Sayable constraints.  This should match the Symbol
-- used for the primary instance declaration.
--
-- @
-- instance $(sayableSubConstraints $ do tagSym "loud"
--                                       ofType ''X
--           ) => Sayable "loud" X where ...
-- @
--
-- If not used, the default saytag is the variable @saytag@.  This operation is
-- coincident with the 'tagVar', 'tagNat', and 'tagTH' operations and the last
-- such operation will be the one used.
tagSym :: String -> ConstrM ()
tagSym str = ConstrM $ \c -> (c { cSaytag = Left $ TH.LitT $ TH.StrTyLit str }, ())

-- | Sometimes the sub-constraints should be wrapped in another type (via a
-- constructor for that type).  For example:
--
-- @
-- data WithIndentation a = WInd Int a
--
-- data Foo { subVal :: Bar }
--
-- instance $(sayableSubConstraints $ do ofType ''Foo
--                                       subWrapper (TH.ConT ''WithIndentation)
--           ) => Sayable saytag Foo where
--    sayable foo = "FOO" &< WInd 2 (subVal foo)
--
subWrapper :: Type -> ConstrM ()
subWrapper wrp = ConstrM $ \c -> (c { cWrapper = Just wrp }, ())


-- | This ConstrM operation is used to declare a filter to be applied to the
-- sub-elements: only sub-element 'Name' candidates for which this filter
-- function returns 'True' will have a Sayable constraint generated.
--
-- @
-- myModuleNames :: TH.Name -> Bool
-- myModuleNames = maybe False ("MyModule" ==) . TH.nameModule
--
-- instance $(sayableSubConstraints $ do ofType ''Foo
--                                       subElemFilter myModuleNames
--          ) => Sayable saytag Foo where ...
-- @
--
-- If not used, the default is equivalent to @subElemFilter (const True)@ which
-- accepts all sub-element names.
subElemFilter :: (Name -> Bool) -> ConstrM ()
subElemFilter fltrf = ConstrM $ \c -> (c { cFilt = fltrf }, ())


-- | This ConstrM operation is used to specify the variable name that should be
-- used for the next sub-element parameter.  When sub-elements are parameterized
-- (e.g. @Maybe a@) then this operation allows the identification of a primary
-- instance variable to be used for the sub-element. All sub-elements must take
-- parameters in the same order, although they don't need to accept all
-- parameters.
--
-- @
-- data Foo b a = Foo (Either a b)
--
-- instance $(sayableSubConstraints $ do ofType ''Foo
--                                       paramVar "x"
--                                       paramVar "y"
--           ) => Sayable saytag (Foo y x) where ...
-- @
--
-- generates: @(Sayable saytag (Either x y), Sayable saytag x, Sayable saytag y)@
--
-- As shown above, this operation can be used multiple times to specify multiple
-- parameters.
paramVar :: String -> ConstrM ()
paramVar pname = ConstrM $ \c -> (c { cVars = cVars c <> [ Right pname ] }, ())


-- | This ConstrM operation is used to specify a Symbol singleton value that
-- should be used for the next sub-element parameter.  When sub-elements are
-- parameterized (e.g. @Maybe a@) then this operation allows the identification
-- of a Symbol to be used for the sub-element. All sub-elements must take
-- parameters in the same order, although they don't need to accept all
-- parameters.
--
-- @
-- data Bar (s :: Symbol) = ...
-- data Foo (s :: Symbol) = Foo { thing :: Bar s }
--
-- instance $(sayableSubConstraints $ do ofType ''Foo
--                                       paramSym "arg"
--           ) => Sayable saytag (Foo "arg") where ...
-- @
--
-- generates: @Sayable saytag (Bar "arg")@
--
-- This operation can be used multiple times to specify multiple parameters.
paramSym :: String -> ConstrM ()
paramSym pname = let psym = Left $ TH.LitT $ TH.StrTyLit pname
                 in ConstrM $ \c -> (c { cVars = cVars c <> [ psym ] }, ())


-- | This ConstrM operation is used to specify a Nat singleton value that
-- should be used for the next sub-element parameter.  When sub-elements are
-- parameterized (e.g. @Maybe a@) then this operation allows the identification
-- of a Nat to be used for the sub-element. All sub-elements must take
-- parameters in the same order, although they don't need to accept all
-- parameters.
--
-- @
-- data Bar (s :: Symbol) (n :: Nat) = ...
-- data Foo (n :: Nat) (s :: Symbol) = Foo { thing :: Bar s n }
--
-- instance $(sayableSubConstraints $ do ofType ''Foo
--                                       paramSym "arg"
--                                       paramNat 2
--           ) => Sayable saytag (Foo 2 "arg") where ...
-- @
--
-- generates: @Sayable saytag (Bar "arg" 2)@
--
-- This operation can be used multiple times to specify multiple parameters.
paramNat :: Integer -> ConstrM ()
paramNat pnum = let pnat = Left $ TH.LitT $ TH.NumTyLit pnum
                 in ConstrM $ \c -> (c { cVars = cVars c <> [ pnat ] }, ())


-- | This ConstrM operation is used to specify a template-haskell Type that
-- should be used for the next sub-element parameter.  When sub-elements are
-- parameterized (e.g. @Maybe a@) then this operation allows specification of a
-- specific Type for that parameter.  The use of this operation is unusual and is
-- expected only in cases where 'paramVar', 'paramSym', or 'paramNat' are
-- insufficient.
--
-- @
-- data Bar '(s :: Symbol, n :: Nat) = ...
-- data Foo (n :: Nat) (s :: Symbol) = Foo { thing :: Bar '(s, n) }
--
-- instance $(sayableSubConstraints $ do ofType ''Foo
--                                       paramTH (TH.App
--                                                (TH.App (TH.TupleT 2)
--                                                 (TH.LitT $ TH.StrTyLit "loud"))
--                                                (TH.LitT $ TH.NumTyLit 3))
--           ) => Sayable saytag (Foo 3 "loud") where ...
-- @
--
-- generates: @Sayable saytag (Bar '("loud", 3))@
--
-- This operation can be used multiple times to specify multiple parameters.
paramTH :: TH.Type -> ConstrM ()
paramTH pty = ConstrM $ \c -> (c { cVars = cVars c <> [ Left pty ] }, ())


data SCCtx = SCCtx { cTgt :: Name
                   , cFilt :: Name -> Bool
                   , cSaytag :: Either TH.Type String
                   , cVars :: [Either TH.Type String]
                   , cWrapper :: Maybe Type
                   }


sayableSubConstraints' :: (Name -> Bool)
                       -> Name
                       -> Either TH.Type String
                       -> Maybe TH.Type
                       -> [Either TH.Type String]
                       -> PredQ
sayableSubConstraints' fltr t tagName mbWrapper varBindings = do
  v <- case tagName of
         Left x -> return x
         Right tn -> varT $ mkName tn

  let vbs = let toTgt = \case
                  Left ty -> ty
                  Right nm -> VarT (mkName nm)
        in fmap toTgt varBindings

  rt <- reifyDatatype t
  let cf' = concat (constructorFields <$> datatypeCons rt)
  cf <- mapM resolveTypeSynonyms cf'

  let tsubmap = Map.fromList $ zip (tvName <$> datatypeVars rt) vbs

  let collectTC :: TH.Type -> Q [TH.Type]
      collectTC = \case

        -- String as a [Char] is a special case because String is the fundamental
        -- Pretty printable type, so restore it after resolveTypeSynonyms.
        AppT ListT (ConT a) | a == ''Char -> return [ConT ''String]

        -- Assumes that lists or list-like sequences are handled via &* and so
        -- only a dependency on the element type and not the sequence type itself
        -- is needed.
        AppT ListT b -> collectTC b
        AppT (ConT a) b | TH.nameBase a `elem` [ "NonEmpty"
                                               , "Seq"
                                               , "Set"
                                               ] -> collectTC b

        -- Assumes Maybe is handled via &? so only a dependency on the element
        -- type and not on the Maybe itself.
        x@(AppT (ConT a) b) -> if a == ''Maybe
                               then collectTC b
                               else return $ if fltr a then [x] else []

        (AppT a b) -> do y <- collectTC a
                         z <- collectTC b
                         return $ concat ((\x -> AppT x <$> z) <$> y)
        x@(ConT a) -> return $ if fltr a then [x] else []
        x@(VarT a) -> return $ if fltr a then [x] else []
        x -> return [x]
  tc <- fmap (applySubstitution tsubmap) . concat <$> (mapM collectTC cf)


  -- Make a tuple of constraints for all the sub-element types collected in tc.
  -- Note that tc may contain duplicates, but it is fine to express duplicate
  -- constraints, so don't bother trying to filter down to only unique target
  -- types here.  However, the maximum tuple arity is 62, so create nested tuples
  -- if necessary to stay under that limit.

  let mkConstrTpl elem0 lst =
        if null lst then elem0
        else let (lst1, lst2) = splitAt 60 lst
                 l1len = length lst1
                 base = AppT (TupleT (l1len + 1)) elem0
                 next tc' p' = AppT p' (classPred ''Sayable [v, tc'])
             in mkConstrTpl (foldr next base lst1) lst2
  let p = mkConstrTpl (TupleT 0) (maybe id AppT mbWrapper <$> tc)
  let pv = mkConstrTpl p (VarT . mkName <$> rights varBindings)
  return pv
