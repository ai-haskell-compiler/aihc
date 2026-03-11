# Changelog
All notable changes to this project will be documented in this file.

The format is based on
[Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## [1.5.0.2] - 2026-01-08
### Fixed
- Support GHC 9.14 and containers 0.8.

## [1.5.0.1] - 2025-06-19
### Fixed
- Allow any 2.x QuickCheck version.
- Make the lexer and parser safe.

## [1.5.0.0] - 2024-12-03
### Removed
- Remove deprecated 'gql' quasi quoter.

### Changed
- Validate the subscription root not to be an introspection field
  (`singleFieldSubscriptionsRule`).

## [1.4.0.0] - 2024-10-26
### Changed
- `Schema.Directive` is extended to contain a boolean argument, representing
  repeatable directives. The parser can parse repeatable directive definitions.
  Validation allows repeatable directives.
- `AST.Document.Directive` is a record.
- `gql` quasi quoter is deprecated (moved to graphql-spice package).

### Fixed
- `gql` quasi quoter recognizeds all GraphQL line endings (CR, LF and CRLF).

### Added
- @specifiedBy directive.

## [1.3.0.0] - 2024-05-01
### Changed
- Remove deprecated `runCollectErrs`, `Resolution`, `CollectErrsT` from the
  `Error` module.

## [1.2.0.3] - 2024-01-09
### Fixed
- Fix corrupted source distribution.

## [1.2.0.2] - 2024-01-09
### Fixed
- `gql` removes not only leading `\n` but also `\r`.
- Fix non nullable type string representation in executor error messages.
- Fix input objects not being coerced to lists.
- Fix used variables are not found in the properties of input objects.

## [1.2.0.1] - 2023-04-25
### Fixed
- Support hspec 2.11.

## [1.2.0.0] - 2023-02-28
### Added
- Schema printing.
- `Semigroup` and `Monoid` instances for `AST.Document.Description`.
- Support for vector 0.13.0.0 and transformers 0.6.1.0.

### Fixed
- Fix resolvers returning a list in the reverse order.

### Removed
- GHC 8 support.
- Cabal -json flag.
- `Test.Hspec.GraphQL`: moved to `graphql-spice` package.
- CPP `ifdef WITH_JSON` blocks.

## [1.1.0.0] - 2022-12-24
### Changed
- Removed deprecated `Language.GraphQL.Error` functions: `addErr`, `addErrMsg`,
  `singleError`.
- Deprecate `Resolution`, `CollectErrsT` and `runCollectErrs` in the `Error`
  module. It was already noted in the documentation that these symbols are
  deprecated, now a pragma is added.
- `Language.GraphQL`: Added information about the *json* flag and switching to
  *graphql-spice* for JSON support.

### Added
- Partial schema printing: operation type encoder.

## [1.0.3.0] - 2022-03-27
### Fixed
- Index position in error path. (Index and Segment paths of a field have been
  swapped).
- Parsing empty list as an argument.

### Added
- quickCheck Parser test for arguments. Arbitrary instances for Language.GraphQL.AST.Document.
- Enhanced query error messages. Add tests for these cases.
- Allow version 2.0 of the text package.

## [1.0.2.0] - 2021-12-26
### Added
- `Serialize` instance for `Type.Definition.Value`.
- `VariableValue` instance for `Type.Definition.Value`.
- `Json` build flag, enabled by default. JSON and Aeson support can be disabled
  by disabling this flag.

## [1.0.1.0] - 2021-09-27
### Added
- Custom `Show` instance for `Type.Definition.Value` (for error
  messages).
- Path information in errors (path to the field throwing the error).
- Deprecation notes in the `Error` module for `Resolution`, `CollectErrsT` and
  `runCollectErrs`. These symbols are part of the old executor and aren't used
  anymore, it will be deprecated in the future and removed.
- `TH` module with the `gql` quasi quoter.

### Fixed
- Error messages are more concrete, they also contain type information and
  wrong values, where appropriate and possible.
- If the field with an error is Non-Nullable, the error is propagated to the
  first nullable field, as required by the specification.

## [1.0.0.0] - 2021-07-04
### Added
- `Language.GraphQL.Execute.OrderedMap` is a map data structure, that preserves
  insertion order.
- `Language.GraphQL.Schema.schemaWithTypes` constructs a complete schema,
  including an optional schema description and user-defined types not referenced
  in the schema directly (for example interface implementations).
- `Language.GraphQL.Schema.description` returns the optional schema description.
- All errors that can be associated with a location in the query contain
  location information.

### Fixed
- Parser now accepts empty lists and objects.
- Parser now accepts all directive locations.
- `valuesOfCorrectTypeRule` doesn't check lists recursively since the
  validation traverser calls it on all list items.
- `valuesOfCorrectTypeRule` doesn't check objects recursively since the
  validation traverser calls it on all object properties.
- Validation of non-nullable values inside lists.
- `executeField` shouldn't assume that a selection has only one field with a
  given name, but it should take the first field. The underlying cause is a
  wrong pattern, which (because of the laziness) is executed only if the field
  has arguments.

### Changed
- `AST.Document.Value.List` and `AST.Document.ConstValue.ConstList` contain
  location information for each list item.
- `Error`: `singleError`, `addErr` and `addErrMsg` are deprecated. They are
  internal functions used by the executor for error handling.

## [0.11.1.0] - 2021-02-07
### Added
- `Validate.Rules`:
  - `overlappingFieldsCanBeMergedRule`
  - `possibleFragmentSpreadsRule`
  - `variablesInAllowedPositionRule`
  - `valuesOfCorrectTypeRule`
- `Type.Schema.implementations` contains a map from interfaces and objects to
  interfaces they implement.
- Show instances for GraphQL type definitions in the `Type` modules.
- Custom Show instances for type and value representations in the AST.
- `AST.Document.escape` escapes a single character in a `StringValue`.

## [0.11.0.0] - 2020-11-07
### Changed
- `AST.Document.Selection` wraps additional new types: `Field`, `FragmentSpread`
  and  `InlineFragment`. Thus validation rules can be defined more concise.
- `AST.Document`: `Argument` and `Directive` contain token location.
- `AST.Document.Argument` contains the `Value` wrapped in the `Node`.
- `AST.Lexer.colon` and `AST.Lexer.at` ignore the result (it is always the
- same).
- `Validate.Validation`: `Validation.rules` was removed. `Validation.rules`
  contained the list of rules, but the executed rules shouldn't know about other
  rules. `rules` was a part of the `Validation` context to pass it easier
  around, but since the rules are traversed once now and applied to all nodes in
  the tree at the beginning, it isn't required anymore.
- `Validate.Validation.Error`: `path` is removed since it isn't possible to get
  the path without executing the query.
- `Error.Error`: `path` added. It is currently always empty.
- `Validate.Validation.Path` was moved to `Error`.
- `Type.Schema.Schema`: data constructor is hidden, fields are accessible with
  freestanding functions: `query`, `mutation`, `subscription`, `directives` and
  `types`.

### Added
- `Validate.Validation.Rule` constructors:
  - `SelectionRule`
  - `FragmentRule`
  - `FragmentSpreadRule`
  - `ArgumentsRule`
  - `DirectivesRule`
  - `VariablesRule`
  - `FieldRule`
- `Validate.Rules`:
  - `fragmentsOnCompositeTypesRule`
  - `fragmentSpreadTargetDefinedRule`
  - `fragmentSpreadTypeExistenceRule`
  - `noUnusedFragmentsRule`
  - `noFragmentCyclesRule`
  - `uniqueArgumentNamesRule`
  - `uniqueDirectiveNamesRule`
  - `uniqueVariableNamesRule`
  - `variablesAreInputTypesRule`
  - `noUndefinedVariablesRule`
  - `noUndefinedVariablesRule`
  - `noUnusedVariablesRule`
  - `uniqueInputFieldNamesRule`
  - `fieldsOnCorrectTypeRule`
  - `scalarLeafsRule`
  - `knownArgumentNamesRule`
  - `knownDirectiveNamesRule`
  - `directivesInValidLocationsRule`
  - `providedRequiredArgumentsRule`
  - `providedRequiredInputFieldsRule`
- `AST.Document.Field`.
- `AST.Document.FragmentSpread`.
- `AST.Document.InlineFragment`.
- `AST.Document.Node`.
- `Type.In.Arguments`: Type alias for an argument map.
- `Type.Schema.Directive` and `Type.Schema.Directives` are directive definition
  representation.
- `Type.Schema.schema`: Schema constructor.

### Fixed
- Collecting existing types from the schema considers subscriptions.

### Removed
- `AST.Document.Alias`. Use `AST.Document.Name` instead.

## [0.10.0.0] - 2020-08-29
### Changed
- `Test.Hspec.GraphQL.*`: replace `IO` in the resolver with any `MonadCatch`.
- The `Location` argument of `AST.Document.Definition.ExecutableDefinition` was
  moved to `OperationDefinition` and `FragmentDefinition` since these are the
  actual elements that have a location in the document.
- `Validate.Rules` get the whole validation context (AST and schema).

### Added
- `Validate.Validation` contains data structures and functions used by the
  validator and concretet rules.
- `Validate.Rules`: operation validation rules.

## [0.9.0.0] - 2020-07-24
### Fixed
- Location of a parse error is returned in a singleton array with key
  `locations`.
- Parsing comments in the front of definitions.
- Some missing labels were added to the parsers, some labels were fixed to
  refer to the AST nodes being parsed.

### Added
- `AST` reexports `AST.Parser`.
- `AST.Document.Location` is a token location as a line and column pair.
- `Execute` reexports `Execute.Coerce`.
- `Error.Error` is an error representation with a message and source location.
- `Error.Response` represents a result of running a GraphQL query.
- `Type.Schema` exports `Type` which lists all types possible in the schema.
- Parsing subscriptions.
- `Error.ResponseEventStream`, `Type.Out.Resolve`, `Type.Out.Subscribe` and
  `Type.Out.SourceEventStream` define subscription resolvers.
- `Error.ResolverException` is an exception that can be thrown by (field value
  and event stream) resolvers to signalize an error. Other exceptions will
  escape.
- `Test.Hspec.GraphQL` contains some test helpers.
- `Validate` contains the validator and standard rules.

### Changed
- `Type.Out.Resolver`: Interface fields don't have resolvers, object fields
  have value resolvers, root subscription type resolvers need an additional
  resolver that creates an event stream. `Resolver` represents these differences
  now and pairs a field with the function(s). Resolvers don't have `ExceptT`,
  errors are handled with `MonadThrow`/`MonadCatch`.
- All code from `Trans` is moved to `Type.Out` and exported by `Type` and
  `Type.Out`.
- `AST.Core` contained only `Arguments` which was moved to `Type.Definition`.
  `AST` provides now only functionality related to parsing and encoding, as it
  should be.
- `Execute.execute` takes an additional argument, a possible operation name
  and returns either a stream or the response.
- `Error` module was changed to work with dedicated types for errors and the
  response instead of JSON.
- `graphqlSubs` takes an additional argument, the operation name. The type of
  variable names is changed back to JSON since it is a common format and it
  saves additional conversions. Custom format still can be used with the
  underlying functions (in the `Execute` module). The function returns either a
  a stream or the resolved value.
- `graphql` returns either a stream or the resolved value.
- The constraint of the base monad was changed to `MonadCatch` (and it implies
  `MonadThrow`).

### Removed
- `Trans.ActionT` is an unneeded layer of complexity. `Type.Out.Resolver`
  represents possible resolver configurations.
- `Execute.executeWithName`. `Execute.execute` takes the operation name and
   completely replaces `executeWithName`.

## [0.8.0.0] - 2020-06-20
### Fixed
- The parser rejects variables when parsing defaultValue (DefaultValue). The
  specification defines default values as `Value` with `const` parameter and
  constants cannot be variables. `AST.Document.ConstValue` was added,
  `AST.Document.ObjectField` was modified.
- AST transformation should never fail.
    * Arguments and fields with a missing variable as value should be left out.
    * Invalid (recusrive or non-existing) fragments should be skipped.
- Argument value coercion.
- Variable value coercion.
- Result coercion.
- The executor should skip the fields missing in the object type and not fail.
- Merging subselections.

### Changed
- `Schema.Resolver` was moved to `Type.Out`, it is a field and resolver function
  pair.
- `AST.Core.Value` was moved into `Type.Definition`. These values are used only
  in the execution and type system, it is not a part of the parsing tree.
- `Type` module is superseded by `Type.Out`. This module contains now only
  exports from other module that complete `Type.In` and `Type.Out` exports.
- `Error.CollectErrsT` contains the new `Resolution` data structure.
  `Resolution` represents the state used by the executor. It contains all types
  defined in the schema and collects the thrown errors.

### Added
- `Type.Definition` contains base type system definition, e.g. Enums and
  Scalars.
- `Type.Schema` describes a schema. Both public functions that execute queries
  accept a `Schema` now instead of a `HashMap`. The execution fails if the root
  operation doesn't match the root Query type in the schema.
- `Type.In` and `Type.Out` contain definitions for input and output types.
- `Execute.Coerce` defines a typeclass responsible for input, variable value
  coercion. It decouples us a bit from JSON since any format can be used to pass
  query variables. Execution functions accept (`HashMap Name a`) instead of
  `Subs`, where a is an instance of `VariableValue`.

### Removed
- `Schema.scalar`, `Schema.wrappedScalar`. They accepted everything can be
  converted to JSON and JSON is not suitable as an internal representation for
  GraphQL. E.g. GraphQL distinguishes between Floats and Integers.
- `Schema.wrappedObject`, `Schema.object`, `Schema.resolversToMap`. There is no
  need in special functions to construct field resolvers anymore, resolvers are
  normal functions attached to the fields in the schema representation.
- `Schema.resolve` is superseded by `Execute.Execution`.
- `Error.runAppendErrs` isn't used anywhere.
- `AST.Core`: `Document`, `Directive`, `Field`, `Fragment`, `Selection`, `Alias`
  `TypeCondition` were modified, moved into `Execute.Transform.Document` and
  made private. These types describe intermediate representation used by the
  executor internally. Moving was required to avoid cyclic dependencies between
  the executor and type system.
- `AST.Core` doesn't reexports anything.

## [0.7.0.0] - 2020-05-11
### Fixed
- Result of null encoding
- Block strings encoding
- Result of tab and newline encoding

### Added
- AST for the GraphQL schema.
- Type system definition parser.
- `Trans.argument`.
- Schema extension parser.
- Contributing guidelines.
- `Schema.resolversToMap` (intended to be used internally).

### Changed
- Rename `AST.Definition` into `AST.Document.ExecutableDefinition`.
  `AST.Document.TypeSystemDefinition` and `AST.Document.TypeSystemExtension`
  can also be definitions.
- Move all AST data to `AST.Document` and reexport them.
- Rename `AST.OperationSelectionSet` to `AST.Document.SelectionSet`.
- Make `Schema.Subs` a `Data.HashMap.Strict` (was a function
  `key -> Maybe value` before).
- Make `AST.Lexer.at` a text (symbol) parser. It was a char before and is
  `symbol "@"` now.
- Replace `MonadIO` with a plain `Monad`. Since the tests don't use IO,
  set the inner monad to `Identity`.
- `NonEmpty (Resolver m)` is now `HashMap Text (NonEmpty (Resolver m))`. Root
  operation type can be any type, therefore a hashmap is needed. Since types
  cannot be empty, we save the list of resolvers in the type as a non-empty
  list. Currently only "Query" and "Mutation" are supported as types. For more
  schema support is required. The executor checks now if the type in the query
  matches the type of the provided root resolvers.

### Removed
- `AST.Field`, `AST.InlineFragment` and `AST.FragmentSpread`.
  These types are only used in `AST.Selection` and `AST.Selection` contains now
  3 corresponding data constructors, `Field`, `InlineFragment` and
  `FragmentSpread`, instead of separate types. It simplifies pattern matching
  and doesn't make the code less typesafe.
- `Schema.scalarA`.
- `Schema.wrappedScalarA`.
- `Schema.wrappedObjectA`.
- `Schema.objectA`.
- `AST.Argument`. Replaced with `AST.Arguments` which holds all arguments as a
  key/value map.

## [0.6.1.0] - 2019-12-23
### Fixed
- Parsing multiple string arguments, such as
  `login(username: "username", password: "password")` would fail on the comma
  due to strings not having a space consumer.
- Fragment spread is evaluated based on the `__typename` resolver. If the
  resolver is missing, it is assumed that the type condition is satisfied (all
  fragments are included).
- Escaping characters during encoding.

### Added
- Directive support (@skip and @include).
- Pretifying multi-line string arguments as block strings.

## [0.6.0.0] - 2019-11-27
### Changed
- `Language.GraphQL.Encoder` moved to `Language.GraphQL.AST.Encoder`.
- `Language.GraphQL.Parser` moved to `Language.GraphQL.AST.Parser`.
- `Language.GraphQL.Lexer` moved to `Language.GraphQL.AST.Lexer`.
- All `Language.GraphQL.AST.Value` data constructor prefixes were removed. The
  module should be imported qualified.
- All `Language.GraphQL.AST.Core.Value` data constructor prefixes were removed.
  The module should be imported qualified.
- `Language.GraphQL.AST.Core.Object` is now just a HashMap.
- `Language.GraphQL.AST.Transform` is isn't exposed publically anymore.
- `Language.GraphQL.Schema.resolve` accepts a selection `Seq` (`Data.Sequence`)
  instead of a list. Selections are stored as sequences internally as well.
- Add a reader instance to the resolver's monad stack. The Reader contains
  a Name/Value hashmap, which will contain resolver arguments.

### Added
- Nested fragment support.

### Fixed
- Consume ignored tokens after `$` and `!`. I mistakenly assumed that
  `$variable` is a single token, same as `Type!` is a single token. This is not
  the case, for example `Variable` is defined as `$ Name`, so these are two
  tokens, therefore whitespaces and commas after `$` and `!` should be
  consumed.

### Improved
- `Language.GraphQL.AST.Parser.type_`: Try type parsers in a variable
  definition in a different order to avoid using `but`.

### Removed
- `Language.GraphQL.AST.Arguments`. Use `[Language.GraphQL.AST.Argument]`
  instead.
- `Language.GraphQL.AST.Directives`. Use `[Language.GraphQL.AST.Directives]`
  instead.
- `Language.GraphQL.AST.VariableDefinitions`. Use
  `[Language.GraphQL.AST.VariableDefinition]` instead.
- `Language.GraphQL.AST.FragmentName`. Use `Language.GraphQL.AST.Name` instead.
- `Language.GraphQL.Execute.Schema` - It was a resolver list, not a schema.
- `Language.GraphQL.Schema`: `enum`, `enumA`, `wrappedEnum` and `wrappedEnumA`.
  Use `scalar`, `scalarA`, `wrappedScalar` and `wrappedScalarA` instead.

## [0.5.1.0] - 2019-10-22
### Deprecated
- `Language.GraphQL.AST.Arguments`. Use `[Language.GraphQL.AST.Argument]`
  instead.
- `Language.GraphQL.AST.Directives`. Use `[Language.GraphQL.AST.Directives]`
  instead.
- `Language.GraphQL.AST.VariableDefinitions`. Use
  `[Language.GraphQL.AST.VariableDefinition]` instead.

### Added
- Module documentation.
- Inline fragment support.

### Fixed
- Top-level fragments.
- Fragment for execution is chosen based on the type.

## [0.5.0.1] - 2019-09-10
### Added
- Minimal documentation for all public symbols.

### Deprecated
- `Language.GraphQL.AST.FragmentName`. Replaced with Language.GraphQL.AST.Name.
- `Language.GraphQL.Execute.Schema` - It is not a schema (at least not a
  complete one), but a resolver list, and the resolvers should be provided by
  the user separately, because the schema can originate from a GraphQL
  document. `Schema` name should be free to provide a data type for the real
  schema later.
- `Language.GraphQL.Schema`: `enum`, `enumA`, `wrappedEnum` and `wrappedEnumA`.
  There are actually only two generic types in GraphQL: Scalars and objects.
  Enum is a scalar value.

### Fixed
- Parsing block string values.

## [0.5.0.0] - 2019-08-14
### Added
- `executeWithName` executes an operation with the given name.
- Export `Language.GraphQL.Encoder.definition`,
  `Language.GraphQL.Encoder.type'` and `Language.GraphQL.Encoder.directive`.
- Export `Language.GraphQL.Encoder.value`. Escapes \ and " in strings now.

### Changed
- `Operation` includes now possible operation name which allows to support
  documents with multiple operations.
- `Language.GraphQL.Encoder.document` and other encoding functions take a
  `Formatter` as argument to distinguish between minified and pretty printing.
- All encoder functions return `Data.Text.Lazy`.

### Removed
- Unused `Language.GraphQL.Encoder.spaced`.

## [0.4.0.0] - 2019-07-23
### Added
- Support for mutations.
- Error handling (with monad transformers).
- Nullable types.
- Arbitrary nested lists support.
- Potential BOM header parsing.

### Changed
- attoparsec is replaced with megaparsec.
- The library is now under `Language.GraphQL` (instead of `Data.GraphQL`).
- HUnit and tasty are replaced with Hspec.
- `Alternative`/`MonadPlus` resolver constraints are replaced with `MonadIO`.

### Removed
- Duplicates from `Language.GraphQL.AST` already available in
  `Language.GraphQL.AST.Core`.
- All module exports are now explicit, so private and help functions aren't
  exported anymore.

## [0.3] - 2015-09-22
### Changed
- Exact match numeric types to spec.
- Names follow now the spec.
- AST slightly different for better readability or easier parsing.
- Replace golden test for test to validate parsing/encoding.

### Added
- Parsing errors in all cases where `Alternative` is used.
- GraphQL encoder.

### Fixed
- Expect braces `inputValueDefinitions` instead of parens when parsing.

## [0.2.1] - 2015-09-16
### Fixed
- Include data files for golden tests in Cabal package.
- Support for ghc-7.8.

## [0.2] - 2015-09-14
### Added
- Rudimentary parser for `GraphQL` which successfully parses the sample file
  `kitchen-sink.graphql` from `graphql-js` tests.
- Golden test for `kitchen-sink.grahql` parsing.
### Changed
- Many optional data types in `GraphQl` didn't need to be wrapped in a `Maybe`.
- Some `newtype`s became type synonyms for easier parsing.

## 0.1 - 2015-09-12
### Added
- Data types for the GraphQL language.

[1.5.0.2]: https://git.caraus.tech/OSS/graphql/compare/v1.5.0.1...v1.5.0.2
[1.5.0.1]: https://git.caraus.tech/OSS/graphql/compare/v1.5.0.0...v1.5.0.1
[1.5.0.0]: https://git.caraus.tech/OSS/graphql/compare/v1.4.0.0...v1.5.0.0
[1.4.0.0]: https://git.caraus.tech/OSS/graphql/compare/v1.3.0.0...v1.4.0.0
[1.3.0.0]: https://git.caraus.tech/OSS/graphql/compare/v1.2.0.3...v1.3.0.0
[1.2.0.3]: https://git.caraus.tech/OSS/graphql/compare/v1.2.0.2...v1.2.0.3
[1.2.0.2]: https://git.caraus.tech/OSS/graphql/compare/v1.2.0.1...v1.2.0.2
[1.2.0.1]: https://git.caraus.tech/OSS/graphql/compare/v1.2.0.0...v1.2.0.1
[1.2.0.0]: https://git.caraus.tech/OSS/graphql/compare/v1.1.0.0...v1.2.0.0
[1.1.0.0]: https://git.caraus.tech/OSS/graphql/compare/v1.0.3.0...v1.1.0.0
[1.0.3.0]: https://git.caraus.tech/OSS/graphql/compare/v1.0.2.0...v1.0.3.0
[1.0.2.0]: https://git.caraus.tech/OSS/graphql/compare/v1.0.1.0...v1.0.2.0
[1.0.1.0]: https://git.caraus.tech/OSS/graphql/compare/v1.0.0.0...v1.0.1.0
[1.0.0.0]: https://git.caraus.tech/OSS/graphql/compare/v0.11.1.0...v1.0.0.0
[0.11.1.0]: https://git.caraus.tech/OSS/graphql/compare/v0.11.0.0...v0.11.1.0
[0.11.0.0]: https://git.caraus.tech/OSS/graphql/compare/v0.10.0.0...v0.11.0.0
[0.10.0.0]: https://git.caraus.tech/OSS/graphql/compare/v0.9.0.0...v0.10.0.0
[0.9.0.0]: https://git.caraus.tech/OSS/graphql/compare/v0.8.0.0...v0.9.0.0
[0.8.0.0]: https://git.caraus.tech/OSS/graphql/compare/v0.7.0.0...v0.8.0.0
[0.7.0.0]: https://git.caraus.tech/OSS/graphql/compare/v0.6.1.0...v0.7.0.0
[0.6.1.0]: https://git.caraus.tech/OSS/graphql/compare/v0.6.0.0...v0.6.1.0
[0.6.0.0]: https://git.caraus.tech/OSS/graphql/compare/v0.5.1.0...v0.6.0.0
[0.5.1.0]: https://git.caraus.tech/OSS/graphql/compare/v0.5.0.1...v0.5.1.0
[0.5.0.1]: https://git.caraus.tech/OSS/graphql/compare/v0.5.0.0...v0.5.0.1
[0.5.0.0]: https://git.caraus.tech/OSS/graphql/compare/v0.4.0.0...v0.5.0.0
[0.4.0.0]: https://git.caraus.tech/OSS/graphql/compare/v0.3...v0.4.0.0
[0.3]: https://git.caraus.tech/OSS/graphql/compare/v0.2.1...v0.3
[0.2.1]: https://git.caraus.tech/OSS/graphql/compare/v0.2...v0.2.1
[0.2]: https://git.caraus.tech/OSS/graphql/compare/v0.1...v0.2
