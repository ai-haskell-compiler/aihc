<p align="center">
  <a href="http://www.adjoint.io"><img src="https://www.adjoint.io/assets/img/adjoint-logo@2x.png" width="250"/></a>
</p>

[![CircleCI](https://circleci.com/gh/adjoint-io/merkle-tree.svg?style=svg)](https://circleci.com/gh/adjoint-io/merkle-tree)
[![Hackage](https://img.shields.io/hackage/v/merkle-tree.svg)](https://hackage.haskell.org/package/merkle-tree)

merkle-tree
===========

This library implements a merkle-tree data structure, for representing a list of
hashed data as a binary tree of pairs of hashes converging to a single hash, the
`merkle root`. `SHA3_256` is used as the hashing algorithm.

# Description

A Merkle tree is a tamper-resistant data structure that allows a large amount of
data to be compressed into a single hash and can be queried for the presence
of specific elements in the data with a proof constructed in logarithmic space.

## Construction

A Merkle tree is a binary tree of hashes, in which all the leaf nodes are the
individual data elements in the block. To construct a merkle tree, the initial
data elements are first hashed using the merkle tree hash function to generate 
the leaf nodes of the tree. The resulting hashed data are subsequently hashed 
together in pairs to create the parent nodes of the leaf nodes. This process 
continues until it results in a single hash known as the merkle root.

```haskell
-- | Constructs a Merkle Tree from a list of ByteStrings
mkMerkleTree :: [ByteString] -> MerkleTree ByteString

-- | Generates the hash of a piece of data existing as a leaf node in the Tree
mkLeafRootHash :: ByteString -> MerkleRoot ByteString
```

## Merkle Inclusion Proof

Perhaps the most important reason to use a merkle tree to represent the block
data is the ability to construct a merkle proof (`O(n)`) that verifies
the inclusion of a transaction *t* in a specific block *b* (`O(log(n))`).

Merkle proofs are an important part in creating decentralized block chain
networks as nodes or client programs (aka "light-weight nodes") that do not 
store blocks in memory or on disk regularly need to *verify* that a 
transaction has been included on the chain or not. This can be accomplished 
by the lightweight node supplying the transaction hash and a block index,
querying a full node for a merkle inclusion proof of the transaction's inclusion
in the block with the index supplied. If the full node finds the transaction in
that block and responds with the inclusion proof and the block's merkle root.
With this information, it takes `O(log(n))` for the lightweight node to validate
the inclusion proof.

```haskell
-- | Constructs a Merkle Inclusion Proof 
merkleProof 
  :: MerkleTree a   -- ^ Tree to which data may belong 
  -> MerkleRoot a   -- ^ Leaf root hash, data to query inclusion of 
  -> MerkleProof a  -- ^ A list of hashes to verify data inclusion 

-- | Validates a Merkle Inclusion Proof
validateMerkleProof 
 :: MerkleProof a  -- ^ Inclusion proof constructed by the prover  
 -> MerkleRoot a   -- ^ Root of the merkle tree from which the proof was constructed
 -> MerkleRoot a   -- ^ Leaf root hash for which the proof was constructed 
 -> Bool           -- ^ Leaf root inclusion
```

# Example

```haskell
import Crypto.Hash.MerkleTree

example :: Bool
example = 
    -- Does the proof prove that `mleaf` exists in `mtree`? 
    validateMerkleProof proof (mtRoot mtree) mleaf 
  where
    -- Build a merkle tree from a list of data
    mtree = mkMerkleTree ["tx1", "tx2", "tx3"] 
    -- Construct merkle proof that a leaf exists in `merkleTree`
    mleaf = mkLeafRootHash "tx2"
    proof = merkleProof mtree mleaf
```

License
-------

Copyright 2017-2018 Adjoint Inc

Released under MIT License
