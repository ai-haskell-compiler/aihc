{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.JsonSpec.OpenApi.Rename (
  Rename
) where

import Data.JsonSpec
  ( FieldSpec(Optional, Required)
  , Specification
    ( JsonArray, JsonBool, JsonDateTime, JsonEither, JsonInt, JsonLet
    , JsonNullable, JsonNum, JsonObject, JsonRaw, JsonRef, JsonString, JsonTag
    )
  )
import GHC.TypeError (ErrorMessage((:$$:)))
import GHC.TypeLits (type (+), AppendSymbol, Nat, Symbol)
import qualified GHC.TypeError as TE


{-|
  Resolve OpenApi name conflicts.

  The json-spec mechanism for giving names to things is more powerful than
  OpenApi mechanism for naming things. 'JsonLet' allows for nested scopes,
  where deeper names can shadow other names. OpenApi on the other hand only has
  a flat space to name schemas, so every name is "globally scoped", resulting
  in possible name conflicts when mapping 'JsonLet' names to OpenApi names.

  This type family resolves the conflict by renaming the 'JsonLet' names
  in your 'Specification' so that they are all unique, so they won't
  conflict when mapping them into the global OpenApi schema namespace.

  We do not apply this type family by default because it has the potential to
  add significant compilation time if your 'Specification's are large. If you
  happen to know that your 'Specification' contains no name conflicts then you
  can avoid paying that cost.

  It isn't perfect. I've tried to strike a balance between implementation
  complexity and avoiding unnecessary renames.

  Essentially, if a duplicate name is detected, I append a @".<n>"@ to the
  name, where @<n>@ is an integer. So if you are using names that /already/
  follow this format you might get into trouble.

  For instance, this 'Specification' will fail to rename properly:
  > JsonLet
  >   '[ '("foo", JsonString)
  >    , '("foo.1", JsonString)
  >    ]
  >    ( JsonObject
  >        '[ "field1" ::: JsonRef "foo"
  >         , "field2" ::: JsonLet '[ '("foo", JsonInt)] (JsonRef "foo")
  >         ]
  >    )

  because the "foo" in "field2" will be renamed to "foo.1", causing a
  new conflict with the existing "foo.1".
-}
type family Rename (spec :: Specification) :: Specification where
  Rename spec =
    Fst (FoldRename (G '[]) (A '[]) spec)


type family
    Fst
      (a :: (Specification, Global))
      :: Specification
  where
    Fst '(spec, global) = spec


type family
    FoldRename
      (global :: Global)
      (active :: Active)
      (spec   :: Specification)
      :: (Specification, Global)
  where
    FoldRename global active JsonString = '(JsonString, global)

    FoldRename global active JsonNum = '(JsonNum, global)

    FoldRename global active JsonInt = '(JsonInt, global)

    FoldRename global active JsonBool = '(JsonBool, global)

    FoldRename global active (JsonTag tag) = '(JsonTag tag, global)

    FoldRename global active JsonDateTime = '(JsonDateTime, global)

    FoldRename global active JsonRaw = '(JsonRaw, global)

    FoldRename global active (JsonNullable spec) =
      MapSpec
        JsonNullable
        (FoldRename global active spec)

    FoldRename global active (JsonArray spec) =
      MapSpec JsonArray (FoldRename global active spec)

    FoldRename global active (JsonObject fields) =
      RenameObject global active fields

    FoldRename global active (JsonEither left right) =
      RenameEither global active left right

    FoldRename global active (JsonLet defs spec) =
      RenameLet
        (UpdateGlobals global defs)
        active
        defs
        spec

    FoldRename global (A active) (JsonRef name) =
      '(JsonRef (LookupNewName name active), global)


type family
    RenameObject
      (global :: Global)
      (active :: Active)
      (fields :: [FieldSpec])
      :: (Specification, Global)
  where
    RenameObject global active fields =
      RenameObject2
        (FoldReflectFields '( '[], global) active fields)


type family
    RenameObject2
      (acc :: ([FieldSpec], Global))
      :: (Specification, Global)
  where
    RenameObject2 '(fields, global) =
      '(JsonObject (Reverse '[] fields), global)


type family
    FoldReflectFields
      (   acc :: ([FieldSpec], Global))
      (active :: Active)
      (fields :: [FieldSpec])
      :: ([FieldSpec], Global)
  where
    FoldReflectFields '(acc, global) active '[] = '(acc, global)
    FoldReflectFields '(acc, global) active (field : more) =
      FoldReflectFields2
        acc
        active
        (ReflectField global active field)
        more


type family
    FoldReflectFields2
      (   acc :: [FieldSpec])
      (active :: Active)
      ( field :: (FieldSpec, Global))
      (  more :: [FieldSpec])
      :: ([FieldSpec], Global)
  where
    FoldReflectFields2 acc active '(field, global) more =
      FoldReflectFields
        '(field : acc, global)
        active
        more


type family
    ReflectField
      (global :: Global)
      (active :: Active)
      (field :: FieldSpec)
      :: (FieldSpec, Global)
  where
    ReflectField global active (Required name spec) =
      ReflectRequiredField
        name
        (FoldRename global active spec)
    ReflectField global active (Optional name spec) =
      ReflectOptionalField
        name
        (FoldRename global active spec)


type family
    ReflectRequiredField
      (name :: Symbol)
      (spec :: (Specification, Global))
      :: (FieldSpec, Global)
  where
    ReflectRequiredField name '(spec, global) =
      '(Required name spec, global)


type family
    ReflectOptionalField
      (name :: Symbol)
      (spec :: (Specification, Global))
      :: (FieldSpec, Global)
  where
    ReflectOptionalField name '(spec, global) =
      '(Optional name spec, global)


type family
    RenameEither
      (global :: Global)
      (active :: Active)
      (  left :: Specification)
      ( right :: Specification)
      :: (Specification, Global)
  where
    RenameEither global active left right =
      RenameEither2
        active
        (FoldRename global active left)
        right


type family
    RenameEither2
      (active :: Active)
      (  left :: (Specification, Global))
      ( right :: Specification)
      :: (Specification, Global)
  where
    RenameEither2 active '(left, global) right =
      RenameEither3 left (FoldRename global active right)


type family
    RenameEither3
      (left :: Specification)
      (right :: (Specification, Global))
      :: (Specification, Global)
  where
    RenameEither3 left '(right, global) =
      '(JsonEither left right, global)


type family
    UpdateGlobals
      (global :: Global)
      (  defs :: [(Symbol, Specification)])
      :: Global
  where
    UpdateGlobals global '[] = global
    UpdateGlobals (G global) ( '(name, spec) : more) =
      UpdateGlobals
        (G (IncrementName global name))
        more


type family
    IncrementName
      (global :: [NameState])
      (  name :: Symbol)
      :: [NameState]
  where
    IncrementName '[] name = '[N name 0]
    IncrementName (N name ord : more) name =
      N name (ord + 1) : more
    IncrementName (N miss ord : more) name =
      N miss ord : IncrementName more name


type family
    RenameLet
      (global :: Global) -- updated
      (active :: Active)
      (defs :: [(Symbol, Specification)])
      (spec :: Specification)
      :: (Specification, Global)
  where
    RenameLet global active defs spec =
      RenameLet2
        global
        (UpdateActives global active defs)
        defs
        spec


type family
    RenameLet2
      (global :: Global) -- updated
      (active :: Active) -- updated
      (defs :: [(Symbol, Specification)])
      (spec :: Specification)
  where
    RenameLet2 global active defs spec =
      RenameLet3
        global
        active
        (RenameDefs global defs)
        spec


type family
    RenameLet3
      (global :: Global)
      (active :: Active)
      (  defs :: [(Symbol, Specification)])
      (  spec :: Specification)
      :: (Specification, Global)
  where
    RenameLet3 global active defs spec =
      RenameLet4
        (ReflectDefs global active defs)
        active
        spec


type family
    RenameLet4
      (     r :: (Global, [(Symbol, Specification)]))
      (active :: Active)
      (  spec :: Specification)
      :: (Specification, Global)
  where
    RenameLet4 '(global, defs) active spec =
      RenameLet5
        (FoldRename global active spec)
        defs


type family
    RenameLet5
      (r :: (Specification, Global))
      (defs :: [(Symbol, Specification)])
      :: (Specification, Global)
  where
    RenameLet5 '(spec, global) defs =
      '(JsonLet defs spec, global)


type family
    UpdateActives
      (global :: Global) -- already updated
      (active :: Active)
      (  defs :: [(Symbol, Specification)])
      :: Active
  where
    UpdateActives global active '[] = active
    UpdateActives global (A active) ( '(name, spec) : more) =
      UpdateActives
        global
        (A (SetActive global active name))
        more


type family
    SetActive
      (global :: Global)
      (active :: [NameState])
      (  name :: Symbol)
      :: [NameState]
  where
    SetActive
        (G (N name ord : moreG))
        (N name x : moreA)
        name
      =
        (N name ord : moreA)
    SetActive
        (G (N name ord : moreG))
        (N miss x : moreA)
        name
      =
        N miss x : SetActive (G (N name ord : moreG)) moreA name
    SetActive
        (G (N name ord : moreG))
        '[]
        name
      =
        '[N name ord]
    SetActive
        (G (N miss x : moreG))
        active
        name
      =
        SetActive (G moreG) active name


type family
    ReflectDefs
      (global :: Global)
      (active :: Active)
      (  defs :: [(Symbol, Specification)])
      :: (Global, [(Symbol, Specification)])
  where
    ReflectDefs global active defs =
      FoldReflectDefs '(global, '[]) active defs


type family
    FoldReflectDefs
      (acc :: (Global, [(Symbol, Specification)]))
      (active :: Active)
      (defs :: [(Symbol, Specification)])
      :: (Global, [(Symbol, Specification)])
  where
    FoldReflectDefs acc active '[] = acc
    FoldReflectDefs
        '(global, acc)
        active
        ( '(name, spec) : more )
      =
        FoldReflectDefs2
          (FoldRename global active spec)
          active
          name
          acc
          more


type family
    FoldReflectDefs2
      (  spec :: (Specification, Global))
      (active :: Active)
      (  name :: Symbol)
      (   acc :: [(Symbol, Specification)])
      (  more :: [(Symbol, Specification)])
      :: (Global, [(Symbol, Specification)])
  where
    FoldReflectDefs2 '(spec, global) active name acc more =
      FoldReflectDefs '(global, '(name, spec) : acc) active more


{-| Update the LHS of the definitions, to match the already updated globals -}
type family
    RenameDefs
      (global :: Global)
      (  defs :: [(Symbol, Specification)])
      :: [(Symbol, Specification)]
  where
    RenameDefs globals '[] = '[]
    RenameDefs (G globals) ( '(name, spec) : more) =
      '(LookupNewName name globals, spec) : RenameDefs (G globals) more


type family
    LookupNewName
      (target :: Symbol)
      ( names :: [NameState])
      :: Symbol
  where
    LookupNewName target (N target 0 : more) = target
    LookupNewName target (N target ord : more) =
      AppendSymbol target ( AppendSymbol "." ( ToSymbol ord))
    LookupNewName target (N miss ord : more) =
      LookupNewName target more


type family
    ToSymbol
      (o :: Nat)
      :: Symbol
  where
    {-
      We _could_ do some fancy stuff with type level mod and division
      over Nat to handle an almost arbitrary number of name conflicts,
      but for now handling 50 levels of name conflict seems sufficient.
    -}
    ToSymbol 1 = "1"
    ToSymbol 2 = "2"
    ToSymbol 3 = "3"
    ToSymbol 4 = "4"
    ToSymbol 5 = "5"
    ToSymbol 6 = "6"
    ToSymbol 7 = "7"
    ToSymbol 8 = "8"
    ToSymbol 9 = "9"
    ToSymbol 10 = "10"
    ToSymbol 11 = "11"
    ToSymbol 12 = "12"
    ToSymbol 13 = "13"
    ToSymbol 14 = "14"
    ToSymbol 15 = "15"
    ToSymbol 16 = "16"
    ToSymbol 17 = "17"
    ToSymbol 18 = "18"
    ToSymbol 19 = "19"
    ToSymbol 20 = "20"
    ToSymbol 21 = "21"
    ToSymbol 22 = "22"
    ToSymbol 23 = "23"
    ToSymbol 24 = "24"
    ToSymbol 25 = "25"
    ToSymbol 26 = "26"
    ToSymbol 27 = "27"
    ToSymbol 28 = "28"
    ToSymbol 29 = "29"
    ToSymbol 30 = "30"
    ToSymbol 31 = "31"
    ToSymbol 32 = "32"
    ToSymbol 33 = "33"
    ToSymbol 34 = "34"
    ToSymbol 35 = "35"
    ToSymbol 36 = "36"
    ToSymbol 37 = "37"
    ToSymbol 38 = "38"
    ToSymbol 39 = "39"
    ToSymbol 40 = "40"
    ToSymbol 41 = "41"
    ToSymbol 42 = "42"
    ToSymbol 43 = "43"
    ToSymbol 44 = "44"
    ToSymbol 45 = "45"
    ToSymbol 46 = "46"
    ToSymbol 47 = "47"
    ToSymbol 48 = "48"
    ToSymbol 49 = "49"
    ToSymbol 50 = "50"
    ToSymbol 51 = "51"
    ToSymbol 52 = "52"
    ToSymbol 53 = "53"
    ToSymbol 54 = "54"
    ToSymbol 55 = "55"
    ToSymbol 56 = "56"
    ToSymbol 57 = "57"
    ToSymbol 58 = "58"
    ToSymbol 59 = "59"
    ToSymbol 60 = "60"
    ToSymbol 61 = "61"
    ToSymbol 62 = "62"
    ToSymbol 63 = "63"
    ToSymbol 64 = "64"
    ToSymbol 65 = "65"
    ToSymbol 66 = "66"
    ToSymbol 67 = "67"
    ToSymbol 68 = "68"
    ToSymbol 69 = "69"
    ToSymbol 70 = "70"
    ToSymbol 71 = "71"
    ToSymbol 72 = "72"
    ToSymbol 73 = "73"
    ToSymbol 74 = "74"
    ToSymbol 75 = "75"
    ToSymbol 76 = "76"
    ToSymbol 77 = "77"
    ToSymbol 78 = "78"
    ToSymbol 79 = "79"
    ToSymbol 80 = "80"
    ToSymbol 81 = "81"
    ToSymbol 82 = "82"
    ToSymbol 83 = "83"
    ToSymbol 84 = "84"
    ToSymbol 85 = "85"
    ToSymbol 86 = "86"
    ToSymbol 87 = "87"
    ToSymbol 88 = "88"
    ToSymbol 89 = "89"
    ToSymbol 90 = "90"
    ToSymbol 91 = "91"
    ToSymbol 92 = "92"
    ToSymbol 93 = "93"
    ToSymbol 94 = "94"
    ToSymbol 95 = "95"
    ToSymbol 96 = "96"
    ToSymbol 97 = "97"
    ToSymbol 98 = "98"
    ToSymbol 99 = "99"
    ToSymbol 100 = "100"

    ToSymbol a =
      TE.TypeError (
        TE.Text      "The author of json-spec-openapi was lazy and did"
        :$$: TE.Text "not foresee that anyone would write a specification"
        :$$: TE.Text "that would have to handle more than 100 collisions"
        :$$: TE.Text "for a single name when converting to an OpenApi"
        :$$: TE.Text "spec. Please submit a bug report on GitHub and"
        :$$: TE.Text "I'll add a fix."
      )


type family
    MapSpec
      (f :: Specification -> Specification)
      (a :: (Specification, Global))
      :: (Specification, Global)
  where
    MapSpec f '(a, b) = '(f a, b)


type family
    Reverse
      (acc  :: [FieldSpec])
      (list :: [FieldSpec])
      :: [FieldSpec]
  where
    Reverse acc '[] = acc
    Reverse acc (a : more) = Reverse (a : acc) more


data NameState = N
  { _name :: Symbol
  ,  _ord :: Nat
  }


newtype Global = G [NameState]


newtype Active = A [NameState]


