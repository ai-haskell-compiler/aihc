# Registry [![Hackage](https://img.shields.io/hackage/v/registry.svg)](https://hackage.haskell.org/package/registry) [![Build Status](https://github.com/etorreborre/registry/workflows/ci/badge.svg)](https://github.com/etorreborre/registry/actions)

##### *It's functions all the way down* <img src="https://raw.githubusercontent.com/etorreborre/registry/main/doc/images/unboxed-bottomup.jpg" border="0"/>

#### Presentation

This library provides a data structure, a "Registry", to control the creation of functions from other functions. You can use this technique to:

 - create applications out of software components ("dependency injection")
 - fine tune encoders/decoders (see the [`registry-aeson`][registry-aeson] and the [`registry-messagepack`][registry-messagepack] projects)
 - create composable data generators for nested datatypes (see the [`registry-hedgehog`][registry-hedgehog] and the [`registry-hedgehog-aeson`][registry-hedgehog-aeson] projects)

You can watch a video presenting the main ideas behind the library [here](https://www.youtube.com/watch?v=xKgII4ggo-4).

The following sections introduce in more details the problem that this library is addressing, the concepts behind the solution and various use-cases which can arise on real projects:

 1. [what is the problem?][motivation]
 2. the concept of a [Registry][registry] and the resolution algorithm
 3. how does this [compare to monad transformers and effects](https://github.com/etorreborre/effects)?

#### Tutorials

 1. tutorial: use a `Registry` to create [applications][tutorial] and define components
 1. use a `Registry` to compose [Hedgehog generators][generators]
 1. [workshop][workshop]: implement your own simplified registry and understand the basic concepts behind it

#### How-tos

 1. how to [install this library][install]?
 1. how to do [mocking][mocking]?
 1. how to [specialize some values in some contexts][specialize]?
 1. how to [control effects][caching] occurring when creating a component (like a connection pool)?
 1. how to [allocate resources][resources] which must be finalized?
 1. how to [extract a dot graph from the registry][dot] in an application?
 1. how to [interact with a library using monad transformers](https://github.com/etorreborre/registry/blob/master/test/Test/Data/Registry/MonadRandomSpec.hs)?
 1. how to [remove boilerplate][boilerplate] due to parameter passing?
 1. how to [create a typeclass from a record of functions][typeclass]?
 1. how to [read type errors][errors]?

#### Reference guides

 1. [main operators and functions][reference]
 1. [implementation notes][implementation]


[motivation]: http://github.com/etorreborre/registry/blob/main/doc/motivation.md
[registry]: http://github.com/etorreborre/registry/blob/main/doc/registry.md
[tutorial]: http://github.com/etorreborre/registry/blob/main/doc/tutorial.md
[applications]: http://github.com/etorreborre/registry/blob/main/doc/applications.md
[mocking]: http://github.com/etorreborre/registry/blob/main/doc/applications.md#integration
[install]: http://github.com/etorreborre/registry/blob/main/doc/install.md
[specialize]: http://github.com/etorreborre/registry/blob/main/doc/applications.md#context-dependent-configurations
[caching]: http://github.com/etorreborre/registry/blob/main/doc/applications.md#caching
[resources]: http://github.com/etorreborre/registry/blob/main/doc/applications.md#resources
[dot]: http://github.com/etorreborre/registry/blob/main/doc/dot.md
[boilerplate]: http://github.com/etorreborre/registry/blob/main/doc/boilerplate.md
[typeclass]: http://github.com/etorreborre/registry/blob/main/doc/typeclass.md
[generators]: http://github.com/etorreborre/registry-hedgehog/blob/main/doc/tutorial.md
[registry-hedgehog]: http://github.com/etorreborre/registry-hedgehog
[registry-messagepack]: http://github.com/etorreborre/registry-messagepack
[registry-aeson]: http://github.com/etorreborre/registry-aeson
[registry-hedgehog-aeson]: http://github.com/etorreborre/registry-hedgehog-aeson
[reference]: http://github.com/etorreborre/registry/blob/main/doc/reference.md
[implementation]: http://github.com/etorreborre/registry/blob/main/doc/implementation.md
[errors]: http://github.com/etorreborre/registry/blob/main/doc/errors.md
[workshop]: https://github.com/etorreborre/registry-workshop
