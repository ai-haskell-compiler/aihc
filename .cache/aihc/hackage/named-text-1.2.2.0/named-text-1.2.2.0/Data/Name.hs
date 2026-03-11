{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|

The 'Data.Name' type is designed to be used in place of plain 'String' or
'Data.Text' types.  Fundamentally 'Data.Name' is an extension of 'Data.Text', but
it includes two type-level parameters that help to manage the underlying data: a
style parameter and a nameOf parameter.

 * The style parameter is used to control various functionality and validation
   around the contained Text type.  For example, one style is CaseInsensitive,
   which allows comparisons to be done independently of ASCII case.

 * The nameOf parameter is a phantom type string which ensures that two different
   strings aren't inadvertently swapped or combined.  Any transformation from one
   nameOf to another nameOf must be intentional.

== Example

For a more complete example, consider a @login@ form which takes an email as the
username and a password.  Without the Data.Name module, the type signature might
be:

>    login :: String -> String -> IO Bool

There are a number of deficiencies that can be identified for this
implementation, including:

* Which argument is the email username, which is the password?

* Is there any protection against simply printing the password to stdout?

* Can these protections be extended to the code calling @login@ and not just
  observed within the @login@ function itself?

* Email addresses are typically not case sensitive: does the @login@ function
  provide the appropriate case handling?

Using 'Data.Name', the declaration would look more like:

> login :: Named CaseInsensitive "email" -> Named Secure "password" -> IO Bool

There are a number of advantages that can be observed here:

* The arguments are self-identifying.  No need to try to remember which was used
  for what purpose.

* The email is treated as a case-insensitive value, both within @login@ but also
  automatically in any other uses elsewhere.  Setting this value automatically
  applies case insensitivity conversions, and comparisons are always case
  independent.

* The password is secured against simply printing it or retrieving the value to
  use unsafely elsewhere.  There is a special operation to return the actual
  underlying Text from a secure name, which will presumably be very carefully
  used only by the @login@ implementation itself.

* Zero runtime cost (other than where needed, such as case translation).

== Alternatives:

One typical alternative approach is to use a @newtype@ wrapper around 'Data.Text'
or 'String' to provide the type level safety.  This is not a bad approach, but
this module seeks to provide the following additional benefits over a simple
@newtype@:

* New names do not need a separate declaration, with associated instance
  declarations:  simply use a new type string.

* Names are parameterized over both style and identity, with different conversion
  abilities for both.  Similar functionality could be established for a @newtype@
  but this would result in either a duplication of effort for each new @newtype@
  declared this way, or else a parameterization of a generic @newtype@ in the
  same general manner as provided by this module (and 'Data.Name' *is* simply a
  @newtype@ at the core).

Another approach is to use the 'Data.Tagged'.  This module is highly similar to
'Data.Tagged', but this module's 'Named' type has two parameters and the
underlying type is always 'Data.Text'.  This module can therefore be considered a
specialization of the generic capabilities of 'Data.Tagged' but more customized
for representing textual data.
-}


{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Name
  (
    -- * Core type
    Named  -- type only, no constructor or member references
  , nameOf
  , nameProxy
  , styleProxy
  , SomeName(SomeName), viewSomeName
  , HasName, myName
    -- ** Style management
    --
    -- | Defines the style type parameter and some well-known styles directly
    -- supported by this module.  Users may define additional styles as needed.
  , NameStyle
  , SomeNameStyle(SomeNameStyle), viewSomeNameStyle
    -- ** Creating a Name
    --
    -- | The 'Named' type is an instance of 'IsString', so a name can be created
    -- from a string via 'fromString'.  In addition, this module defines an
    -- 'IsText' class with a 'fromText' method that operates in a parallel
    -- fashion.
  , IsText(fromText)
    -- ** Conversions
  , ConvertName(convertName)
  , ConvertNameStyle(convertStyle)
    -- ** Extraction and rendering
    --
    -- | For rendering, the 'sayable' package is preferred (as provided by the
    -- 'Sayable' instances, which is an extension of the "prettyprinter" package
    -- (and users desiring a "prettyprinter" output can extract that from the
    -- 'sayable' representation).
  , NameText, nameText

    -- * Regular (UTF-8) Names
  , UTF8
  , type Name

    -- * Case Insensitive Names
  , CaseInsensitive

    -- * Secure Names
  , Secure
  , SecureName, secureNameBypass

    -- * HTML-renderable Names
  , HTMLStyle
  , rawNamedHTML

    -- * Constraining allowed names
  , ValidNames, validName

    -- * Utility operations
  , nameLength
  , nullName
)
where

import           Data.Hashable ( Hashable )
import           Data.Proxy ( Proxy(Proxy) )
import           Data.String ( IsString(fromString) )
import           Data.Text ( Text )
import qualified Data.Text as T
import           GHC.Exts ( Proxy#, proxy#, IsList(fromList, toList), Item )
import           GHC.TypeLits
import           Prettyprinter ( (<+>) )
import qualified Prettyprinter as PP
import           Text.Sayable

#if !MIN_VERSION_base(4,16,0)
import           Numeric.Natural
#endif

import Data.Name.Internal


-- | Retrieve the @nameOf@ type parameter (the "what am I") of a Named as a text
-- value

nameOf :: KnownSymbol nameOf => Named style nameOf -> Proxy# nameOf -> String
nameOf _ = symbolVal'


-- | Retrieve a proxy for the @nameOf@ parameter of 'Named'.

nameProxy :: KnownSymbol nameOf => Named style nameOf -> Proxy nameOf
nameProxy _ = Proxy

-- | Retrieve a proxy for the @style@ parameter of 'Named'.

styleProxy :: KnownSymbol style => Named style nameOf -> Proxy style
styleProxy _ = Proxy


instance {-# OVERLAPPABLE #-} IsString (Named style nameOf) where
  fromString = Named . fromString


-- | The 'IsText' class provides similar functionality to the 'IsString' class,
-- but with 'Data.Text' sources instead of 'String' sources.  Defining an
-- instance of this class allows the use of 'fromText' to convert from
-- 'Data.Text' to the target type (which does not necessarily need to be a
-- 'Named' type, and this generic class should be deprecated in favor of a
-- generic implementation the the "text" library).

class IsText a where fromText :: Text -> a
instance {-# OVERLAPPABLE #-} IsText (Named style nameOf) where
  fromText = Named


-- | Conversion from a 'Named' with one @nameOf@ to a separate @nameOf@ must be
-- done explicitly; the recommended method is via an instance of the
-- 'ConvertName' class, which provides the 'convertName' method to perform the
-- requested conversion.  If there should not be a conversion between the two
-- 'Named' types, no 'ConvertName' class should be defined, and users should
-- refrain from providing an alternative explicit function to perform this
-- conversion.

class NameText style => ConvertName style origTy newTy where
  convertName :: Named style origTy -> Named style newTy
  convertName = fromText . nameText


-- | A 'Named' can be converted from one @style@ to another with an instance of
-- the 'ConvertNameStyle' class.  If no conversion should be supported, no
-- instance should be defined.  Users are highly recommended to use the
-- convertStyle method (instead of a separate manual conversion function) to
-- ensure proper conversions are performed.

class ( NameText inpStyle
      , IsText (Named outStyle nameTy)
      )
      => ConvertNameStyle inpStyle outStyle nameTy where
  convertStyle :: Named inpStyle nameTy -> Named outStyle nameTy
  convertStyle = fromText . nameText


-- | A general class that can be used to extract the Text back out of a name.
-- This should be the preferred method of obtaining the raw Text, and should be
-- used carefully as all of the protections provided by this module are no longer
-- available for that raw Text.  In addition, no instance of this class is
-- provided where the name should not be extractable, and this method may extract
-- a modified form of the text (e.g. the Secure namestyle will return a masked
-- version of the original Text).

class NameText style where
  -- | nameText is used to retrieve the original 'Data.Text' text from a 'Named'
  -- object of the specified style.  This should be the main method used to
  -- extract the Text from a Named, but it should be used carefully because the
  -- protections offered by the 'Named' type will no longer be available for the
  -- raw Text.
  nameText :: Named style nm -> Text
  nameText = named

-- | Some objects have (contain) an associated name that identifies or labels
-- that object.  If they do, they can declare the 'HasName' constraint, and use
-- its 'myName' method to reconstitute the 'Named' from the object.

class HasName x style nm | x -> style, x -> nm where
  -- | myName can be used to extract the associated 'Named' from an object.
  myName :: x -> Named style nm


----------------------------------------------------------------------
-- Rendering

-- | For an @"info"@ @saytag@ (and possibly others), a 'Name' doesn't include its
-- label and simply shows the 'Data.Text' as would be rendered by
-- "prettyprinter".

instance NameText style => Sayable "info" (Named style nm) where
  sayable = Saying . PP.pretty . nameText


-- | Generically the rendered version includes the textual representation of the
-- 'nameOf' parameter followed by the 'Data.Text' itself.

instance {-# OVERLAPPABLE #-} (PP.Pretty (Named style nm)
                              ) => Sayable tag (Named style nm)
 where sayable = Saying . PP.pretty


-- | There is also a 'Show' method; this is *not* the inverse of a 'Read', and in
-- fact there is no 'Read' instance for 'Named'.  The 'Sayable' instance is
-- preferred over 'Show', but 'Show' is provided for default considerations such
-- as test failure reporting.

instance (Sayable "show" (Named style nm)) => Show (Named style nm) where
  show = sez @"show"


-- | This is the general pretty rendering for a Named object.  This can be
-- overriden for specific types or styles for a different rendering.

instance  {-# OVERLAPPABLE #-} ( KnownSymbol ty
                               , NameText style
                               )
                               => PP.Pretty (Named style ty) where
  pretty nm = (PP.pretty $ nameOf nm proxy#)
              <+> PP.squotes (PP.pretty (nameText nm))


----------------------------------------------------------------------
-- Utility operations

-- | Returns the length of the underlying 'Data.Text'

nameLength :: Named style nm -> Natural
nameLength = toEnum . T.length . named

-- | Returns true if the name value is empty.

nullName :: Named style nm -> Bool
nullName = T.null . named


----------------------------------------------------------------------

-- | The 'SomeName' data type is used to existentially hide the identification
-- type parameter for 'Named' objects.  This is usually used when names of
-- different types are mixed together in some container or other name-agnostic
-- interface.

data SomeName =
  forall (s :: Symbol) . KnownSymbol s => SomeName (Name s)


-- | The 'viewSomeName' function is used to project the 'Named' object with its
-- identification type parameter existentially recovered to a function that will
-- consume that 'Named' object and return some sort of result.

viewSomeName :: (forall (s :: Symbol) . KnownSymbol s => Name s -> r) -> SomeName -> r
viewSomeName f (SomeName n) = f n


-- | The 'SomeNameStyle' data type is used to existentially hide the style type
-- of 'Named' objects.  This is usually used when names of different styles are
-- mixed together in some container or other style-agnostic interface.

data SomeNameStyle nameTy =
  forall (s :: Symbol)
  . (KnownSymbol s, NameText s)
  => SomeNameStyle (Named s nameTy)


-- | The 'viewSomeNameStyle' function is used to project the 'Named' object with
-- its style type existentially recovered to a function that will consume that
-- 'Named' object and return some sort of result.

viewSomeNameStyle :: (forall (s :: Symbol) . (KnownSymbol s, NameText s) => Named s nameTy -> r)
                  -> SomeNameStyle nameTy -> r
viewSomeNameStyle f (SomeNameStyle n) = f n


----------------------------------------------------------------------
-- * UTF8 Names

-- | The UTF8 type alias is useable as the @style@ parameter of a 'Named' type.
-- The type-string form may also be used but the type alias is designed to allow
-- abstraction from the raw type-string value.

type UTF8 = "UTF8" :: NameStyle

-- | The Name type is for the standard/most commonly used style which is
-- orthogonal to a normal String or Text.  Because this is the most frequently
-- used form of 'Named', it has a type alias to shorten the usage references.

type Name = Named UTF8

instance IsList (Name s) where
  type Item (Name s) = Item Text
  fromList = fromText . fromList
  toList = toList . nameText

instance ConvertName UTF8 a a where convertName = id

instance NameText UTF8

deriving instance Eq (Named UTF8 nameOf)
deriving instance Ord (Named UTF8 nameOf)
deriving instance Hashable (Named UTF8 nameOf)


----------------------------------------------------------------------
-- * CaseInsensitive Named objects

-- | The CaseInsensitive style of Named objects will allow case-insensitive ASCII
-- comparisons between objects.  On creation, all text is converted to lowercase,
-- so the original input case is not preserved on extraction or rendering.

type CaseInsensitive = "CaseInsensitive" :: NameStyle

instance {-# OVERLAPPING #-} IsString (Named CaseInsensitive nameOf) where
  fromString = Named . T.toLower . fromString

instance {-# OVERLAPPING #-} IsText (Named CaseInsensitive nameOf) where
  fromText = Named . T.toLower

instance KnownSymbol ty => PP.Pretty (Named CaseInsensitive ty) where
  pretty nm = (PP.pretty $ nameOf nm proxy#)
              <+> PP.surround (PP.pretty (nameText nm)) "«" "»"

instance NameText CaseInsensitive


instance ConvertNameStyle UTF8 CaseInsensitive nameTy

-- No ConvertNameStyle is defined for CaseInsensitive -> UTF8 because this cannot
-- be round-tripped.

-- CaseInsensitive names are normalized during construction, so standard
-- instances are sufficient:

deriving instance Eq (Named CaseInsensitive nameOf)
deriving instance Ord (Named CaseInsensitive nameOf)
deriving instance Hashable (Named CaseInsensitive nameOf)


----------------------------------------------------------------------
-- * Secure Named objects

-- | The Secure style of Named objects masks the internal text on extraction or
-- rendering to avoid leaking information.  The actual internal text can be
-- retrieved only with the explicit 'secureNameBypass' function.

type Secure = "SECURE!" :: NameStyle

-- | The SecureName is like Name, but its display form does not reveal the full
-- name.  The use of the 'nameText' extractor or any of the renderers will
-- occlude a portion of the secure name to avoid revealing it in its entirety.

type SecureName = Named Secure


-- | The secureName accessor is used to obtain the name field from a Secure
-- Named.  This is the normal accessor for a Secure Named and will occlude a
-- portion of the extracted name for protection.  For those specific cases where
-- the full Secure Named text is needed, the 'secureNameBypass' accessor should
-- be used instead.

secureName :: Named Secure nameOf -> Text
secureName nm = if T.length (named nm) < 5
                then T.replicate 8 "#"
                else ((T.take 2 $ named nm)
                      <> T.replicate (T.length (named nm) - 4) "#"
                      <> T.reverse (T.take 2 $ T.reverse $ named nm))


-- | The secureNameBypass accessor is used to obtain the raw Text from a Secure
-- Named; this essentially BYPASSES THE SECURITY PROTECTION and should only be
-- used in the limited cases when the raw form is absolutely needed.

secureNameBypass :: Named Secure nameOf -> Text
secureNameBypass = named

instance NameText Secure where
  nameText = secureName

-- No ConvertNameStyle forms are defined for Secure because this is a lossy
-- conversion due to masking.

-- Secure names are constructed in a standard manner and differ only in
-- projection out of the Name, so standard instances are sufficient:

deriving instance Eq (Named Secure nameOf)
deriving instance Ord (Named Secure nameOf)
deriving instance Hashable (Named Secure nameOf)


----------------------------------------------------------------------
-- * HTML Names

-- | The HTMLStyle type alias is useable as the @style@ parameter of a 'Named'
-- type.  The type-string form may also be used but the type alias is designed to
-- allow abstraction from the raw type-string value.
--
-- Text contained in these styles is safe to represent in HTML: angle brackets
-- are converted to their html representation, and ampersands and quotes are
-- escaped.

-- n.b. JSON is a separate module for conditional compilation without introducing
-- additional dependencies, but there are no additional dependencies for
-- HTMLStyle.

type HTMLStyle = "HTML" :: NameStyle

instance {-# OVERLAPPING #-} IsString (Named HTMLStyle nameOf) where
  fromString = Named . toHTMLSafe . fromString

instance {-# OVERLAPPING #-} IsText (Named HTMLStyle nameOf) where
  fromText = Named . toHTMLSafe

instance KnownSymbol ty => PP.Pretty (Named HTMLStyle ty) where
  pretty = PP.pretty . nameText

instance NameText HTMLStyle

-- | To create a Named HTMLStyle from Text that is raw HTML and shouldn't have
-- escaping performed.

rawNamedHTML :: Text -> Named HTMLStyle nameOf
rawNamedHTML = Named

toHTMLSafe :: Text -> Text
toHTMLSafe = T.replace "<" "&lt;"
             . T.replace ">" "&gt;"
             . T.replace "\"" "&quot;"
             . T.replace "'" "&#39;"
             . T.replace "&" "&amp;"  -- do this first because & is added above

fromSafeHTML :: Text -> Text
fromSafeHTML = T.replace "&amp;" "&"
               . T.replace "&gt;" ">"
               . T.replace "&lt;" "<"
               . T.replace "&quot;" "\""
               . T.replace "&#39;" "'"

instance IsList (Named HTMLStyle s) where
  type Item (Named HTMLStyle s) = Item Text
  fromList = fromText . fromList
  toList = toList . nameText

instance ConvertName HTMLStyle a a where convertName = id

instance ConvertNameStyle UTF8 HTMLStyle nameTy
instance ConvertNameStyle HTMLStyle UTF8 nameTy where
  convertStyle = fromText . fromSafeHTML . nameText

-- HTMLStyle names are normalized during construction, so standard instances are
-- sufficient:

deriving instance Eq (Named HTMLStyle nameOf)
deriving instance Ord (Named HTMLStyle nameOf)
deriving instance Hashable (Named HTMLStyle nameOf)

-- Trigger faults in the code and trace them for impact

-- GA has no MB experience, so don't know how much to expect or have any expectations.  Fault triggering and detection would be the principle focus.

----------------------------------------------------------------------
-- Constraining allowed names

-- | The ValidNames constraint can be used to specify the list of allowed names
-- for a parameterized name argument.  For example:
--
-- > foo :: ValidNames n '[ "right", "correct" ] => Name n -> a
--
-- The above allows @foo@ to be called with a @Name "right"@ or a @Name
-- "correct"@, but if it is called with any other 'Named' @nameOf@ parameter then
-- a compilation error will be generated indicating "the supplied @nameOf@ type
-- parameter is not in the allowed Names".
--
-- All instances of this class are pre-defined by this module and the user should
-- not need to create any instances.


class ( KnownNat (AllowedNameType nameOf ntl)
      , DisallowedNameType nameOf ntl ntl
      )
  => ValidNames (nameOf :: Symbol) (ntl :: [Symbol]) where
  -- | The validName method is used to extract the text form of a 'Name' for
  -- which @nameOf@ is a member of the valid names specified by the @ntl@ type
  -- list.  It corresponds to 'nameText' while also providing the validation
  -- of the extraction.
  validName :: Proxy ntl -> Name nameOf -> Text


----------------------------------------------------------------------
-- Internal definitions to support the ValidNames constraint class
-- implementation.

instance ( KnownNat (AllowedNameType nty ntl)  -- n.b. if this fails, see Note-1
         , DisallowedNameType nty ntl ntl
         )
   => ValidNames nty ntl where
  validName _ = nameText

type family AllowedNameType (nty :: Symbol) (ntl :: [Symbol]) :: Nat where
  AllowedNameType nty (nty ': ntl) = 0
  AllowedNameType nty (any ': ntl) = 1 + (AllowedNameType nty ntl)

-- * Note-1
--
-- Normally the DisallowedNameType will generate a useful TypeError if
-- a parameter uses a name not in the allowed names list.  For example:
--
-- > foo :: ValidNames n '[ "name1", "name 2" ] -> Name n -> Bool
-- > foo (Name @"name1" "indiana")  -- OK
-- > foo (Name @"last" "jones")  -- generates TypeError indicating
--                               -- "last" is not a member of the
--                               -- allowed list ["name1", "name 2"]
--
-- However, when parametric constraints are cascaded and the
-- parametric constraints don't align, the compilation complaint will
-- be that there's no KnownNat for the AllowedNameType instance
-- constraint on ValidNames above.  for example:
--
-- > bar :: ValidNames n '[ "name1", "stage" ]
-- > bar n = foo n
--
-- generates the KnownNat error.  Not sure of a better way to handle
-- this at the moment.

class DisallowedNameType (nty :: Symbol) (okntl :: [Symbol]) (ntl :: [Symbol])

instance TypeError ('Text "Name '" ':<>: 'ShowType nty
                    ':<>: 'Text "' not in allowed Names: " ':<>: 'ShowType ntl)
         => DisallowedNameType nty '[] ntl
instance DisallowedNameType nty (nty ': ntys) ntl
instance {-# OVERLAPPABLE #-} DisallowedNameType nty ntys ntl
         => DisallowedNameType nty (oty ': ntys) ntl
