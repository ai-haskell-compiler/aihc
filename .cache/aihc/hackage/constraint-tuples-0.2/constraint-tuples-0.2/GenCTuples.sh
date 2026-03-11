#!/bin/bash

cabal v2-run generator-script -v0 -- -o src/Data/Tuple/Constraint.hs
cabal v2-run generator-script -v0 -- -o src/Data/Tuple/Constraint/ClassNewtype.hs --mode ClassNewtype
cabal v2-run generator-script -v0 -- -o src/Data/Tuple/Constraint/TypeFamily.hs   --mode TypeFamily
cabal v2-run generator-script -v0 -- -o src/Data/Tuple/Constraint/TypeSynonym.hs  --mode TypeSynonym
