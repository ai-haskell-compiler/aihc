# `call-alloy` [![Haskell CI](https://github.com/marcellussiegburg/call-alloy/workflows/Haskell%20CI/badge.svg)](https://github.com/marcellussiegburg/call-alloy/actions?query=workflow%3A%22Haskell+CI%22+branch%3Amaster)

This is a simple library to call [Alloy](http://alloytools.org) given a specification.
This package installs a simple Java Library to make an API call to the Alloy Library.
Alloy is installed (as JAR file) alongside this library as well.

The currently used Alloy version is 6.2.0.

## Requirements

- Java Runtime Environment:
  There is currently no warning if you have not set up any Java Runtime Environment.
  However, you will get runtime errors if it is not available
  when a call to Alloy happens.
  If you want to force a check, perform the test cases.

## Please note

The Java interface to get Alloy instances as well as the
[Alloy Jar](https://github.com/AlloyTools/org.alloytools.alloy/releases/download/v5.1.0/org.alloytools.alloy.dist.jar)
file are installed together with this library using usual cabal means (data directory).

## The library in action

This is a basic description on how to use the library.

### A specification example

Consider this Alloy specification of a simple Graph:

```Alloy
# test/unit/readmeExampleSpecification.als

abstract sig Node {
  flow : Node -> lone Int,
  stored : one Int
} {
  stored >= 0
  all n : Node | some flow[n] implies flow[n] >= 0
  no flow[this]
}

fun currentFlow(x, y : one Node) : Int {
  let s = x.stored, f = x.flow[y] | s < f implies s else f
}

pred withFlow[x, y : one Node] {
  currentFlow[x, y] > 0
}

pred show {}

run withFlow for 3 Int, 2 Node

```

The graph is consisting of `Node`s, which might have some goods `stored`
and may deliver them to other `Node`s (via `flow`).
`Node`s do not have `flow` to themselves.
The `currentFlow` is the minimum between the flow
from the starting `Node` to the end `Node` and the currently `stored` goods
at the starting `Node` (note: intermediate `Node`s are not allowed).
We call two `Nodes` `x` and `y` `withFlow` if `currentFlow` from `x` to `y`
is greater than `0`.
We restrict our search to `3`-Bit signed `Int` values and `2` `Nodes`.

### An instance example

Calling Alloy using `getInstances` and the above program
could return the following instance:

```hs
-- test/unit/readmeExampleInstance.hs

fromList [
  ( Signature {scope = Nothing, sigName = "$withFlow_x"},
    Entry {
      annotation = Just Skolem,
      relation = fromList [
        ( "",
          Single (fromList [
            Object {objSig = "Node", identifier = 1}
            ]))
        ]
      }),
  ( Signature {scope = Nothing, sigName = "$withFlow_y"},
    Entry {
      annotation = Just Skolem,
      relation = fromList [
        ( "",
          Single (fromList [
            Object {objSig = "Node", identifier = 0}
            ]))
        ]
      }),
  ( Signature {scope = Nothing, sigName = "Int"},
    Entry {
      annotation = Nothing,
      relation = fromList [
        ( "",
          Single (fromList [
            NumberObject {number = -4},
            NumberObject {number = -3},
            NumberObject {number = -2},
            NumberObject {number = -1},
            NumberObject {number = 0},
            NumberObject {number = 1},
            NumberObject {number = 2},
            NumberObject {number = 3}
            ]))
        ]
      }),
  ( Signature {scope = Nothing, sigName = "String"},
    Entry {
      annotation = Nothing,
      relation = fromList [("", EmptyRelation)]
      }),
  ( Signature {scope = Nothing, sigName = "none"},
    Entry {
      annotation = Nothing,
      relation = fromList [("", EmptyRelation)]
  }),
  ( Signature {scope = Nothing, sigName = "univ"},
    Entry {
      annotation = Nothing,
      relation = fromList [
        ( "",
          Single (fromList [
            Object {objSig = "Node", identifier = 0},
            Object {objSig = "Node", identifier = 1},
            NumberObject {number = -4},
            NumberObject {number = -3},
            NumberObject {number = -2},
            NumberObject {number = -1},
            NumberObject {number = 0},
            NumberObject {number = 1},
            NumberObject {number = 2},
            NumberObject {number = 3}
            ]))
        ]
      }),
  ( Signature {scope = Just "seq", sigName = "Int"},
    Entry {
      annotation = Nothing,
      relation = fromList [
        ( "",
          Single (fromList [
            NumberObject {number = 0},
            NumberObject {number = 1},
            NumberObject {number = 2}
            ]))
        ]
      }),
  ( Signature {scope = Just "this", sigName = "Node"},
    Entry {
      annotation = Nothing,
      relation = fromList [
        ( "",
          Single (fromList [
            Object {objSig = "Node", identifier = 0},
            Object {objSig = "Node", identifier = 1}
            ])),
        ( "flow",
          Triple (fromList [
            ( Object {objSig = "Node", identifier = 1},
              Object {objSig = "Node", identifier = 0},
              NumberObject {number = 3})
            ])),
        ( "stored",
          Double (fromList [
            ( Object {objSig = "Node", identifier = 0},
              NumberObject {number = 0}),
            ( Object {objSig = "Node", identifier = 1},
              NumberObject {number = 1})
            ]))
        ]
      })
  ]

```

### A retrieval example

Using this library we may retrieve returned signature values using `lookupSig`,
then query parameter variables of the queried predicate using `unscoped`,
and query signature sets and relations using `getSingleAs`, `getDoubleAs`, and `getTripleAs`.

The following Code might for instance be used for the graph example:

```hs
-- test/readmeExample.hs

module ReadmeExample (
  Node (Node),
  instanceToNames,
  ) where

import Control.Monad.Catch              (MonadThrow)
import Data.Set                         (Set)

import Language.Alloy.Call (
  AlloyInstance,
  getDoubleAs,
  getSingleAs,
  getTripleAs,
  int,
  lookupSig,
  object,
  scoped,
  unscoped,
  )
newtype Node = Node Int deriving (Eq, Show, Ord, Read)

instanceToNames
  :: MonadThrow m
  => AlloyInstance
  -> m (Set Node, Set (Node, Int), Set (Node, Node, Int), Set Node, Set Node)
instanceToNames model = do
  let node :: MonadThrow m => String -> Int -> m Node
      node = object "Node" Node
  n     <- lookupSig (scoped "this" "Node") model
  nodes <- getSingleAs "" node n
  store <- getDoubleAs "stored" node int n
  flow  <- getTripleAs "flow" node node int n
  x     <- lookupSig (unscoped "$withFlow_x") model >>= getSingleAs "" node
  y     <- lookupSig (unscoped "$withFlow_y") model >>= getSingleAs "" node
  return (nodes, store, flow, x, y)

```

Calling `instanceToNames` on the above instance would result in the following expression:

```hs
-- test/unit/readmeExampleResult.hs

(
  fromList [Node 0,Node 1],
  fromList [(Node 0,0),(Node 1,1)],
  fromList [(Node 1,Node 0,3)],
  fromList [Node 1],
  fromList [Node 0]
  )

```
