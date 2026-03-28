# Haskell Parser Extension Support Status

## Summary

- Total Extensions: 138
- Supported: 36
- In Progress: 23
- Planned: 79

## Extension Status

| Extension | Status | Tests Passing | Notes |
|-----------|--------|---------------|-------|
| AllowAmbiguousTypes | Planned | - | AllowAmbiguousTypes |
| ApplicativeDo | Planned | - | ApplicativeDo |
| Arrows | In Progress | 0/2 | Arrows |
| BangPatterns | Supported | 5/5 | BangPatterns |
| BinaryLiterals | Supported | 3/3 | BinaryLiterals |
| BlockArguments | In Progress | 0/4 | BlockArguments |
| CApiFFI | Planned | - | CApiFFI |
| ConstrainedClassMethods | Planned | - | ConstrainedClassMethods |
| ConstraintKinds | Planned | - | ConstraintKinds |
| CPP | Planned | - | CPP |
| CUSKs | Planned | - | CUSKs |
| DataKinds | Supported | 5/5 | DataKinds |
| DatatypeContexts | Planned | - | DatatypeContexts |
| DeepSubsumption | Planned | - | DeepSubsumption |
| DefaultSignatures | In Progress | 0/2 | DefaultSignatures |
| DeriveAnyClass | Planned | - | DeriveAnyClass |
| DeriveDataTypeable | Planned | - | DeriveDataTypeable |
| DeriveFoldable | Planned | - | DeriveFoldable |
| DeriveFunctor | Planned | - | DeriveFunctor |
| DeriveGeneric | Planned | - | DeriveGeneric |
| DeriveLift | Planned | - | DeriveLift |
| DeriveTraversable | Planned | - | DeriveTraversable |
| DerivingStrategies | Supported | 5/5 | DerivingStrategies |
| DerivingVia | In Progress | 0/4 | DerivingVia |
| DisambiguateRecordFields | Planned | - | DisambiguateRecordFields |
| DoAndIfThenElse | Supported | 9/9 | DoAndIfThenElse |
| DuplicateRecordFields | Planned | - | DuplicateRecordFields |
| EmptyCase | Supported | 4/4 | EmptyCase |
| EmptyDataDecls | Supported | 5/5 | EmptyDataDecls |
| EmptyDataDeriving | In Progress | 2/3 | EmptyDataDeriving |
| ExistentialQuantification | Supported | 6/6 | ExistentialQuantification |
| ExplicitForAll | Supported | 5/5 | ExplicitForAll |
| ExplicitLevelImports | Supported | 4/4 | ExplicitLevelImports |
| ExplicitNamespaces | Supported | 3/3 | ExplicitNamespaces |
| ExtendedDefaultRules | Planned | - | ExtendedDefaultRules |
| ExtendedLiterals | Supported | 2/2 | ExtendedLiterals |
| FieldSelectors | Planned | - | FieldSelectors |
| FlexibleContexts | Planned | - | FlexibleContexts |
| FlexibleInstances | Supported | 4/4 | FlexibleInstances |
| ForeignFunctionInterface | Supported | 4/4 | ForeignFunctionInterface |
| FunctionalDependencies | Supported | 7/7 | FunctionalDependencies |
| GADTs | Supported | 3/3 | GADTs |
| GADTSyntax | Supported | 9/9 | GADTSyntax |
| GeneralisedNewtypeDeriving | Planned | - | GeneralisedNewtypeDeriving |
| GHC2021 | Planned | - | GHC2021 |
| GHC2024 | Planned | - | GHC2024 |
| GHCForeignImportPrim | Planned | - | GHCForeignImportPrim |
| Haskell2010 | Planned | - | Haskell2010 |
| Haskell98 | Planned | - | Haskell98 |
| HexFloatLiterals | Supported | 3/3 | HexFloatLiterals |
| ImplicitParams | In Progress | 0/2 | ImplicitParams |
| ImplicitPrelude | Planned | - | ImplicitPrelude |
| ImplicitStagePersistence | Planned | - | ImplicitStagePersistence |
| ImportQualifiedPost | Supported | 3/3 | ImportQualifiedPost |
| ImpredicativeTypes | Planned | - | ImpredicativeTypes |
| IncoherentInstances | Planned | - | IncoherentInstances |
| InstanceSigs | Supported | 5/5 | InstanceSigs |
| InterruptibleFFI | Planned | - | InterruptibleFFI |
| KindSignatures | In Progress | 5/6 | KindSignatures |
| LambdaCase | Supported | 5/5 | LambdaCase |
| LexicalNegation | Planned | - | LexicalNegation |
| LiberalTypeSynonyms | Planned | - | LiberalTypeSynonyms |
| LinearTypes | Planned | - | LinearTypes |
| ListTuplePuns | Planned | - | ListTuplePuns |
| MagicHash | Supported | 2/2 | MagicHash |
| MonadComprehensions | Planned | - | MonadComprehensions |
| MonoLocalBinds | Planned | - | MonoLocalBinds |
| MonomorphismRestriction | Planned | - | MonomorphismRestriction |
| MultilineStrings | In Progress | 0/6 | MultilineStrings |
| MultiParamTypeClasses | Supported | 5/5 | MultiParamTypeClasses |
| MultiWayIf | In Progress | 0/4 | MultiWayIf |
| NamedDefaults | Planned | - | NamedDefaults |
| NamedFieldPuns | Supported | 5/5 | NamedFieldPuns |
| NamedWildCards | Supported | 4/4 | NamedWildCards |
| NegativeLiterals | Planned | - | NegativeLiterals |
| NondecreasingIndentation | Planned | - | NondecreasingIndentation |
| NPlusKPatterns | Planned | - | NPlusKPatterns |
| NullaryTypeClasses | Planned | - | NullaryTypeClasses |
| NumDecimals | Planned | - | NumDecimals |
| NumericUnderscores | Supported | 3/3 | NumericUnderscores |
| OrPatterns | Planned | - | OrPatterns |
| OverlappingInstances | Planned | - | OverlappingInstances |
| OverloadedLabels | Planned | - | OverloadedLabels |
| OverloadedLists | Planned | - | OverloadedLists |
| OverloadedRecordDot | Planned | - | OverloadedRecordDot |
| OverloadedRecordUpdate | Planned | - | OverloadedRecordUpdate |
| OverloadedStrings | Planned | - | OverloadedStrings |
| PackageImports | Supported | 3/3 | PackageImports |
| ParallelListComp | Supported | 1/1 | ParallelListComp |
| PartialTypeSignatures | In Progress | 1/2 | PartialTypeSignatures |
| PatternGuards | Supported | 5/5 | PatternGuards |
| PatternSynonyms | In Progress | 2/17 | PatternSynonyms |
| PolyKinds | Planned | - | PolyKinds |
| PostfixOperators | Planned | - | PostfixOperators |
| QualifiedDo | Supported | 1/1 | QualifiedDo |
| QualifiedStrings | Planned | - | QualifiedStrings |
| QuantifiedConstraints | Planned | - | QuantifiedConstraints |
| QuasiQuotes | In Progress | 2/3 | QuasiQuotes |
| Rank2Types | Planned | - | Rank2Types |
| RankNTypes | Planned | - | RankNTypes |
| RebindableSyntax | Planned | - | RebindableSyntax |
| RecordWildCards | In Progress | 0/3 | RecordWildCards |
| RecursiveDo | In Progress | 0/2 | RecursiveDo |
| RelaxedPolyRec | Planned | - | RelaxedPolyRec |
| RequiredTypeArguments | In Progress | 0/2 | RequiredTypeArguments |
| RoleAnnotations | In Progress | 0/4 | RoleAnnotations |
| Safe | Planned | - | Safe |
| ScopedTypeVariables | Planned | - | ScopedTypeVariables |
| StandaloneDeriving | Supported | 12/12 | StandaloneDeriving |
| StandaloneKindSignatures | Supported | 5/5 | StandaloneKindSignatures |
| StarIsType | Planned | - | StarIsType |
| StaticPointers | Planned | - | StaticPointers |
| Strict | Planned | - | Strict |
| StrictData | Planned | - | StrictData |
| TemplateHaskell | In Progress | 0/6 | TemplateHaskell |
| TemplateHaskellQuotes | In Progress | 0/7 | TemplateHaskellQuotes |
| TraditionalRecordSyntax | Planned | - | TraditionalRecordSyntax |
| TransformListComp | Planned | - | TransformListComp |
| Trustworthy | Planned | - | Trustworthy |
| TupleSections | Supported | 4/4 | TupleSections |
| TypeAbstractions | Planned | - | TypeAbstractions |
| TypeApplications | In Progress | 0/2 | TypeApplications |
| TypeData | In Progress | 0/1 | TypeData |
| TypeFamilies | In Progress | 0/17 | TypeFamilies |
| TypeFamilyDependencies | Planned | - | TypeFamilyDependencies |
| TypeInType | Planned | - | TypeInType |
| TypeOperators | In Progress | 0/4 | TypeOperators |
| TypeSynonymInstances | Planned | - | TypeSynonymInstances |
| UnboxedSums | In Progress | 0/3 | UnboxedSums |
| UnboxedTuples | Supported | 5/5 | UnboxedTuples |
| UndecidableInstances | Planned | - | UndecidableInstances |
| UndecidableSuperClasses | Planned | - | UndecidableSuperClasses |
| UnicodeSyntax | Supported | 6/6 | UnicodeSyntax |
| UnliftedDatatypes | Planned | - | UnliftedDatatypes |
| UnliftedFFITypes | Planned | - | UnliftedFFITypes |
| UnliftedNewtypes | Planned | - | UnliftedNewtypes |
| Unsafe | Planned | - | Unsafe |
| ViewPatterns | Supported | 3/3 | ViewPatterns |

