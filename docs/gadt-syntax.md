# GADT Syntax Grammar

This document describes the formal grammar for GADT (Generalized Algebraic Data
Types) syntax in Haskell, as implemented by the `GADTSyntax` extension.

## Grammar

```
gadt_con ::= conids '::' foralls opt_ctxt gadt_body

conids ::= conid
        |  conid ',' conids

foralls ::= <empty>
         | forall_telescope foralls

forall_telescope ::= 'forall' tv_bndrs_spec '.'
                  |  'forall' tv_bndrs      '->'   -- with RequiredTypeArguments

tv_bndrs      ::= <empty> | tv_bndr      tv_bndrs
tv_bndrs_spec ::= <empty> | tv_bndr_spec tv_bndrs_spec

tv_bndr       ::= tyvar
               |  '(' tyvar '::' ctype ')'

tv_bndr_spec  ::= tv_bndr
               |  '{' tyvar '}'
               |  '{' tyvar '::' ctype '}'

opt_ctxt ::= <empty>
          |  btype '=>'
          |  '(' ctxt ')' '=>'

ctxt ::= ctype
      |  ctype ',' ctxt

gadt_body ::= prefix_gadt_body
           |  record_gadt_body

prefix_gadt_body ::= '(' prefix_gadt_body ')'
                  |  return_type
                  |  opt_unpack btype '->' prefix_gadt_body

record_gadt_body ::= '{' fieldtypes '}' '->' return_type

fieldtypes ::= <empty>
            |  fieldnames '::' opt_unpack ctype
            |  fieldnames '::' opt_unpack ctype ',' fieldtypes

fieldnames ::= fieldname
            |  fieldname ',' fieldnames

opt_unpack ::= opt_bang
            |  {-# UNPACK #-} opt_bang
            |  {-# NOUNPACK #-} opt_bang

opt_bang ::= <empty>
          |  '!'
          |  '~'
```

## Implementation Notes

### Supported Features

The current implementation supports:

- Basic GADT syntax with `where` keyword
- Multiple constructor names per declaration (`T1, T2 :: ...`)
- Explicit `forall` with `.` (invisibly-bound type variables)
- Type class constraints (`Eq a => ...`)
- Prefix constructor bodies (`a -> b -> T a`)
- Record constructor bodies (`{ field :: Type } -> T a`)
- Strictness annotations (`!`, `~`)
- Deriving clauses after GADT declarations

### Deferred Features

The following features are deferred to separate extensions:

- `RequiredTypeArguments`: The `forall a ->` syntax for visibly-bound type
  arguments
- `UNPACK`/`NOUNPACK` pragmas: Unpacking hints for strict fields

## Examples

### Basic GADT

```haskell
data Maybe a where
    Nothing :: Maybe a
    Just    :: a -> Maybe a
```

### Newtype with GADT Syntax

```haskell
newtype Down a where
  Down :: a -> Down a
```

### Existential Quantification

```haskell
data Foo where
   MkFoo :: a -> (a->Bool) -> Foo
   Nil   :: Foo
```

### With Constraints

```haskell
data Set a where
  MkSet :: Eq a => [a] -> Set a
```

### Multiple Constructors

```haskell
data T a where
  T1,T2 :: a -> T a
  T3 :: T a
```

### Strict Fields

```haskell
data Term a where
    Lit    :: !Int -> Term Int
    If     :: Term Bool -> !(Term a) -> !(Term a) -> Term a
    Pair   :: Term a -> Term b -> Term (a,b)
```

### With Deriving

```haskell
data Maybe1 a where {
    Nothing1 :: Maybe1 a ;
    Just1    :: a -> Maybe1 a
  } deriving( Eq, Ord )
```

### Record Syntax

```haskell
data Person where
    Adult :: { name :: String, children :: [Person] } -> Person
    Child :: Show a => { name :: !String, funny :: a } -> Person
```

### Infix Constructors

```haskell
infix 6 :--:
data T a where
  (:--:) :: Int -> Bool -> T Int
```
