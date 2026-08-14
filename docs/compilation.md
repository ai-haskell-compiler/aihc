# Compilation and stored artifacts

## Terms and definitions

| Term | Definition |
| --- | --- |
| Package | A named and versioned collection of modules. |
| Module | One Haskell source module with one stable package and module identity. |
| Definition identity | The stable identity of a top-level language entity or generated instance dictionary. |
| Name-resolution interface | The exported scope of one module. It maps each visible name to its original definition identity. |
| Type interface | The type facts that another compilation unit needs to check source code. |
| System FC artifact | The checked and desugared System FC program for one module. This document also calls it the Core artifact. |
| Object artifact | The target-specific native object file for one module. |
| Archive artifact | A native archive that contains the object artifacts for one library. |
| Compilation unit | One strongly connected component, or SCC, of the module dependency graph. |
| SCC digest | A digest of all source, interface, and configuration inputs for one compilation unit. |
| Dependency hash | The digest in a package artifact directory name. It identifies all inputs that can change stored artifacts. |
| Incremental compilation | Compilation that lowers each module separately from System FC to an object artifact. |
| Whole-program compilation | Compilation that merges stored System FC artifacts before the GRIN pipeline. |
| Artifact store | An immutable collection of package artifact directories. |
| Static object | A native runtime object that the compiler puts directly in an object-file data section. |
| CAF | A top-level updateable thunk stored as a writable static object. |

Name resolution and type checking process all modules in one compilation unit together.

A definition identity contains a module identity, namespace, source name, and deterministic disambiguator.

A change to one SCC member changes the SCC digest for all members.

## Design rules

The compiler must not keep a separate whole-program cache.

Each module must have one name-resolution interface, type interface, System FC artifact, and object artifact.

The compiler must encode the first three artifacts with CBOR. The native toolchain defines the object-file format.

The compiler must generate name-resolution and type interfaces while it processes a compilation unit.

After type checking, the compiler must process each module separately. Each module must have a separate System FC artifact and object artifact.

All module artifacts must record the SCC digest. A changed SCC member invalidates every artifact for that SCC.

The compiler must not store GRIN. It must generate GRIN from System FC and release it after native code generation.

The compiler must not create an object link manifest. The native object file already contains symbols, relocations, sections, and foreign references.

## Artifact store

### Package directory identity

Store each built package in this directory form:

```text
{pkg_name}-{pkg_version}-{dephash}/
```

Use a safe file-name encoding for the package name and version. Use a fixed lowercase hexadecimal encoding for `dephash`.

`dephash` must cover these inputs:

- The content of every package source file.
- The package description and enabled language extensions.
- All compiler options that can change an artifact.
- The identities of all direct dependency artifact directories.
- The AIHC compiler identity and artifact schema versions.
- The target triple, runtime ABI, and native code options.
- The native toolchain identity when it can change object code.

The target is part of `dephash` because object artifacts are target-specific. Two targets can share content through filesystem deduplication.

Do not change a published package directory. Build into a temporary directory, validate it, and publish it with one atomic rename.

Multiple projects can refer to the same package directory. Multiple executables can also use the same interfaces, Core artifacts, objects, and archives.

A project lock file can store package directory identities. The project must not copy package artifacts into its build directory.

### Directory layout

Use this logical layout:

```text
{pkg_name}-{pkg_version}-{dephash}/
├── package.cbor
├── modules/
│   ├── Data/
│   │   └── List/
│   │       ├── resolve.cbor
│   │       ├── type.cbor
│   │       ├── core.cbor
│   │       └── object.o
│   └── Example/
│       ├── resolve.cbor
│       ├── type.cbor
│       ├── core.cbor
│       └── object.o
└── lib/
    └── lib{pkg_name}.a
```

Map module-name components to directory components. Do not use the host path separator inside a module identity.

`package.cbor` is a store index. It is not an object link manifest.

The package index must contain:

```text
PackageIndex
  schemaVersion
  packageIdentity
  dependencyStorePaths
  compilerIdentity
  targetTriple
  runtimeAbi
  nativeLinkInputs
  modules
  compilationUnits
  archivePath

ModuleIndex
  moduleIdentity
  sourceDigest
  sccDigest
  resolveDigest
  typeDigest
  coreDigest
  objectDigest
  resolvePath
  typePath
  corePath
  objectPath

CompilationUnitIndex
  sccDigest
  members
  predecessorUnits
```

Paths in the index must be relative to the package directory. The index must not contain temporary build paths.

`predecessorUnits` lists direct SCC dependencies inside the package. Package dependencies come from `dependencyStorePaths`.

`nativeLinkInputs` lists required system libraries, frameworks, and package-level linker options. It does not describe Haskell object symbols.

`archivePath` is absent when the package directory does not contain a library.

## CBOR encoding

Use deterministic CBOR for all compiler-owned stored data.

Each top-level value must start with an artifact kind and a schema version. A decoder must reject an unsupported major schema version.

Use integer field keys or fixed-position arrays for stable records. Use explicit constructor tags for sum types.

Use definite lengths. Sort maps by their encoded keys. Use the shortest valid integer encoding.

Do not encode Haskell `Show` output. Do not depend on constructor order that the schema does not specify.

Encode text as UTF-8. Encode digests and object data as byte strings.

The schema must define every identity representation. In-memory unique numbers must not become cross-module identities.

Use these common logical identity records:

```text
PackageIdentity
  packageName
  packageVersion
  unitHash

ModuleIdentity
  packageIdentity
  moduleName

DefinitionIdentity
  moduleIdentity
  namespace
  sourceName
  disambiguator
```

`unitHash` equals the directory `dephash`. `disambiguator` distinguishes equal source names in one namespace.

Assign disambiguators deterministically from the checked declaration order. Use zero when the source name is unique in its namespace.

## Name-resolution interface

### Purpose

The name-resolution interface describes the exported scope of one module. Import processing uses this data without loading source or Core.

The interface includes re-exported entities. A re-export keeps the definition identity from the module that defines the entity.

The interface does not include local scopes, local unique numbers, use-site annotations, or diagnostics.

### Data format

Use this logical structure:

```text
ResolveInterface
  schemaVersion
  moduleIdentity
  sccDigest
  exports
  fixities

ResolveExport
  visibleName
  namespace
  entityKind
  definitionIdentity
  parentIdentity

ResolveFixity
  definitionIdentity
  associativity
  precedence
```

`namespace` must distinguish at least terms and types. Separate namespaces permit one text name in more than one namespace.

`entityKind` must distinguish these exported entity forms:

- Value.
- Type constructor.
- Type synonym.
- Data constructor.
- Class.
- Class method.
- Record field.
- Type family.
- Data family.
- Pattern synonym, when supported.

`parentIdentity` is absent for an independent entity. It identifies the parent type, class, or pattern synonym for a bundled entity.

Constructor, method, and record-field associations support import forms such as `T(..)` and `C(method)`.

Fixity data uses the original definition identity. A re-exported operator therefore keeps its defining fixity.

An exported module item expands to its exported entities before storage. The stored interface does not need a recursive qualified-module scope.

Store entries in stable order by namespace, visible name, entity kind, and definition identity.

### Production and use

The resolver must process all modules in one SCC together. It must use predecessor name-resolution interfaces for imports outside the SCC.

The resolver must then write one interface for each module. Each file contains only that module's exported scope.

## Type interface

### Purpose

The type interface contains the semantic facts needed to type-check dependent modules.

The interface must contain declarations that this module defines and exports. It must not copy declarations that this module only re-exports.

The interface must contain every class instance that this module defines. Haskell does not require an explicit import or export for an instance.

The interface must also contain required support declarations. These are private local declarations referenced by an exported type or local instance.

Support declarations do not add names to the exported scope. Mark each applicable declaration as `supportOnly`.

### Data format

Use this logical structure:

```text
TypeInterface
  schemaVersion
  moduleIdentity
  sccDigest
  terms
  typeConstructors
  dataTypes
  classes
  typeFamilies
  dataFamilies
  instances
  familyInstances
```

The following sections define the required records.

#### Terms

```text
TermInterface
  definitionIdentity
  typeScheme
```

Store one term entry for each locally defined and exported value. The type scheme includes quantified variables, kinds, and constraints.

Do not store a term entry for a re-exported value. Its defining module already stores its type scheme.

Constructor and method types can occur in their declaration records. Do not duplicate them in `terms`.

#### Type constructors and synonyms

```text
TypeConstructorInterface
  definitionIdentity
  supportOnly
  arity
  kind
  roles
  flavor
  synonymParameters
  synonymBody
```

`flavor` distinguishes data types, newtypes, classes, type synonyms, type families, and data families.

Store `synonymParameters` and `synonymBody` only for a type synonym. Dependent type checking needs the synonym body for expansion.

#### Data types and constructors

```text
DataTypeInterface
  typeIdentity
  supportOnly
  typeVariables
  flavor
  constructors

ConstructorInterface
  definitionIdentity
  constructorIndex
  universalVariables
  existentialVariables
  constraints
  fields
  resultType
  sourceForm

ConstructorFieldInterface
  label
  fieldType
  runtimeRepresentation
  strict
  lazy
  unpack
```

`flavor` distinguishes data types and newtypes. `sourceForm` distinguishes prefix, infix, and record constructors.

`constructorIndex` gives a stable constructor order within the data type. The backend can derive the constructor identity and runtime layout from this record.

The field record contains checked types and runtime representations. The backend must not reconstruct these facts from source syntax.

Store a constructor record when the module exports that constructor. Also store it when another stored declaration needs its semantic layout.

The name-resolution interface controls source visibility. A support constructor does not become importable through its type record.

#### Classes

```text
ClassInterface
  definitionIdentity
  supportOnly
  typeVariables
  superclasses
  methods
  defaultMethods
  defaultSignatures
  associatedFamilies

MethodInterface
  definitionIdentity
  typeScheme
```

Store method types after class type checking. Record whether each method has a default implementation.

#### Type and data families

```text
FamilyInterface
  definitionIdentity
  supportOnly
  familyKind
  parameters
  resultKind
  injectivity
  equations

FamilyEquationInterface
  axiomIdentity
  typeVariables
  leftHandSide
  rightHandSide
  role
```

Store equations that dependent type checking can use. Closed-family equation order is significant and must remain unchanged.

#### Class instances

```text
InstanceInterface
  classIdentity
  dictionaryIdentity
  typeVariables
  context
  headTypes
  overlapMode
  associatedEquations
```

Store all locally defined instances. This rule includes orphan instances and instances for non-exported local bindings.

The dictionary identity names the System FC dictionary binding. It also gives later modules a stable reference to selected evidence.

#### Data-family instances

Open type-family equations also need a separate instance record:

```text
TypeFamilyInstanceInterface
  familyIdentity
  axiomIdentity
  typeVariables
  leftHandSide
  rightHandSide
  role
```

Store every local open type-family equation. These equations are globally available without an explicit import.

Data-family instances use this record:

```text
DataFamilyInstanceInterface
  familyIdentity
  familyType
  typeVariables
  representationTypeIdentity
  axiomIdentity
  constructorIdentities
  isNewtype
```

Store each local data-family instance because dependent type checking needs its representation type and equality axiom.

### Production and use

The type checker must process all modules in one SCC together. It must use predecessor type interfaces for declarations outside the SCC.

The type checker must produce one type interface for each module. A file must not contain the combined interface for the complete SCC.

## System FC artifacts

### Per-module form

After SCC type checking, desugar each checked module separately. Write one `core.cbor` file for each module.

Use this logical top-level structure:

```text
CoreArtifact
  schemaVersion
  moduleIdentity
  sccDigest
  coreSchemaVersion
  topLevelDeclarations
```

`topLevelDeclarations` contains the complete System FC program for the module. It can contain these declaration forms:

- External term declarations with their types and definition identities.
- Data declarations.
- Equality axioms.
- Newtype declarations.
- Primitive declarations.
- Foreign imports.
- Non-recursive and recursive value bindings.

The CBOR schema must encode all System FC types, kinds, coercions, expressions, alternatives, literals, and binders.

Top-level references must use stable definition identities. Local binders can use module-local unique numbers.

Local unique numbers must be deterministic for equal inputs. They only need uniqueness inside one Core artifact.

The artifact must contain enough external declarations to check and merge the module without loading another module first.

### SCC rule

One Core artifact contains one module, including a module from a cyclic SCC. System FC supports external references between these module artifacts.

The SCC affects validation and invalidation. It does not require one combined Core file.

### Incremental use

For incremental compilation, load one module's Core artifact. Generate GRIN, run the GRIN pipeline, and emit that module's object artifact.

Use dependency type interfaces for referenced constructor layouts, newtype facts, and equality axioms. Do not reconstruct these facts in GRIN.

Release the GRIN program after object generation. Do not store a GRIN interface or GRIN cache.

### Whole-program use

For whole-program compilation, load the Core artifacts for all required modules. Merge them into one System FC program.

Start with the executable Core artifacts. Follow each external definition identity to its provider module until no new provider remains.

The package indexes map provider module identities to Core paths. This process does not require a reachability manifest.

The merge must resolve external declarations by definition identity. It must also verify that external and defining types agree.

Run whole-program System FC passes after the merge. Then generate one GRIN program and one whole-program object file.

Link this object file with the RTS and required foreign libraries. Do not link package archives into the whole-program executable.

## Object artifacts

### Contents

`object.o` uses the native object format for the selected target. Examples include ELF, Mach-O, COFF, and WebAssembly objects.

Each module object must contain:

- Native code for the module's generated entries.
- Defined native symbols for the module's top-level values.
- Undefined native symbols for referenced definitions and foreign calls.
- Native relocations for all cross-symbol references.
- Read-only info tables and other constant data.
- Writable static objects for CAFs and other mutable static values.
- Static-root section entries for static objects that can contain heap pointers.
- Unwind or debug sections when the selected compiler options request them.

The object must use one uniform value ABI. References to external Haskell values are object addresses used through `eval` and `apply`.

The object does not need constructor, primitive, reachability, defined-symbol, or required-symbol metadata in a separate file.

### Static CAFs

Emit each CAF directly into a writable data section. Put its initial info-table address and captured fields in that object.

Use native relocations for info tables and captured static values. The native linker resolves cyclic references between static objects.

Reserve at least one payload word for an updateable zero-field thunk. Evaluation uses this word for the indirection target.

Put one pointer to each applicable static object in the target's AIHC static-root section.

The moving collector must scan these static objects during each collection. It must update each pointer field from the current info-table map.

This design does not require a CAF initializer. It also does not require a per-module initializer function.

### Native linking

Use the native linker and archive index to resolve object symbols. An undefined reference can cause extraction of the applicable archive member.

Keep static-root entries with their static objects during section removal. Use target-specific section groups or equivalent linker features.

Foreign libraries and linker options come from package configuration. They do not come from an object link manifest.

## Library archives

After all module objects exist, put them in `lib/lib{pkg_name}.a`. Create the native archive symbol index.

The archive is a derived package artifact. A build can recreate it from the module objects, but storage avoids repeated archive construction.

The archive must contain separate module object members. This layout lets the linker extract only required members.

An incremental executable link uses its object files, dependency archives, the RTS, and configured foreign libraries.

## Compilation workflows

### Library compilation

1. Parse every source module.
2. Build the module dependency graph.
3. Order its SCCs by their dependencies.
4. Resolve all modules in one ready SCC together.
5. Write one name-resolution interface for each SCC member.
6. Type-check all modules in that SCC together.
7. Write one type interface for each SCC member.
8. Desugar each checked module to its own System FC artifact.
9. Lower each System FC artifact through GRIN to one object artifact.
10. Release each temporary GRIN program.
11. Build the library archive from all module objects.
12. Write the package index and publish the immutable package directory.

Independent ready SCCs can run at the same time. Modules can also run at the same time after their SCC type check completes.

### Incremental executable compilation

Compile executable modules with the same SCC rules. Generate one object artifact for each executable module.

Link these objects with dependency archives, the RTS, and required foreign libraries.

### Whole-program executable compilation

Perform the same parse, resolution, type-check, and per-module System FC steps.

Load required dependency Core artifacts from their shared package directories. Merge all required Core artifacts into one System FC program.

Run the GRIN pipeline once for the merged program. Emit one object and link it with the RTS and required foreign libraries.

Do not build or load a whole-program cache. The per-module Core artifacts are the reusable whole-program input.

## Rebuild and reuse rules

If one source file changes, recompute its complete SCC. Write new per-module artifacts for every member of that SCC.

Recompute a dependent SCC only when an interface digest that it uses changes. An implementation can avoid work when new interfaces equal old interfaces.

A changed target invalidates object and archive artifacts. It does not change the logical name-resolution, type, or System FC data.

The package directory remains target-specific because it is immutable and contains target-specific files.

Two projects reuse an artifact when all hashed inputs agree. Project location and build time must not affect artifact identity.

## Data that is intentionally absent

The design does not store these items:

- A whole-program cache.
- GRIN programs.
- GRIN interfaces.
- Backend interfaces for Haskell symbols.
- Object link manifests.
- Per-module initializer lists.
- Duplicate types for re-exported declarations.
- Combined SCC Core files.

The package index records storage and dependency facts only. Compiler interfaces contain language facts, and native objects contain linker facts.
