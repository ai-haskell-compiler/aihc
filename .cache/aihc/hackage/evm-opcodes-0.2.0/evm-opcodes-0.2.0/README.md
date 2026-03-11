# evm-opcodes

[![MIT License](https://img.shields.io/badge/license-MIT-brightgreen.svg)](http://opensource.org/licenses/MIT)
[![Hackage](https://img.shields.io/hackage/v/evm-opcodes.svg?style=flat)](https://hackage.haskell.org/package/evm-opcodes)
[![Stackage Nightly](http://stackage.org/package/evm-opcodes/badge/nightly)](http://stackage.org/nightly/package/evm-opcodes)
[![Stackage LTS](http://stackage.org/package/evm-opcodes/badge/lts)](http://stackage.org/lts/package/evm-opcodes)
[![Haskell CI](https://github.com/sshine/evm-opcodes/actions/workflows/haskell.yml/badge.svg)](https://github.com/sshine/evm-opcodes/actions/workflows/haskell.yml)

This Haskell library provides opcode types for the Ethereum Virtual Machine (EVM).

The library has two purposes:

 - Provide interface between EVM-related libraries: Lower cost of interoperability.
 - Provide easy access to labelled jumps. Labelled jumps are most useful when
   generating EVM code, but actual EVM `jump` instructions pop the destination
   address from the stack.

The library has one parameterised type, `Opcode' j` where `j` is the annotation
for the jump-related instructions `JUMP`, `JUMPI` and `JUMPDEST`, and it has
three concrete variants:

 - `type Opcode = Opcode' ()`
 - `type PositionalOpcode = Opcode' Word`
 - `type LabelledOpcode = Opcode' Text`

The library has a fixpoint algorithm that translates labelled jumps into
positional jumps, and it has another function that translates those positional
jumps into plain EVM opcodes where a constant is pushed before a jump is made.

## Library conventions

When the documentation refers to a lowercase opcode (e.g. `push1`), then that
means the EVM opcode. When the documentation instead refers to an uppercase
opcode (e.g. `PUSH`), then that refers to the Haskell data constructor.

While `dup1`-`dup16`, `swap1`-`swap16` and `log1`-`log4` were implemented using
the data constructors `DUP`, `SWAP` and `LOG` that are not ergonomic to use but
convenient for the library maintainer, pattern synonyms were made:

 - `DUP1`, `DUP2`, ..., `DUP16`
 - `SWAP1`, `SWAP2`, ..., `SWAP16`
 - `LOG1`, `LOG2`, `LOG3`, `LOG4`

When pushing a constant to the stack, EVM uses `push1`, `push2`, ..., `push32`
where the number 1-32 refers to how many bytes the constant occupies. Instead
of having 32 unique push commands, this library has a single `PUSH !Word256`
constructor that serializes to the right `push1`, `push2`, etc.

## Example

Imagine translating the following C program to EVM opcodes:

```
int x = 1;
while (x != 0) { x *= 2 };
```

Since EVM is stack-based, let's put `x` on the stack.

```haskell
λ> import EVM.Opcode
λ> import EVM.Opcode.Labelled as L
λ> import EVM.Opcode.Positional as P

λ> let opcodes = [PUSH 1,JUMPDEST "loop",DUP1,ISZERO,JUMPI "end",PUSH 2,MUL,JUMP "loop",JUMPDEST "end"]

λ> L.translate opcodes
Right [PUSH 1,JUMPDEST 2,DUP1,ISZERO,JUMPI 14,PUSH 2,MUL,JUMP 2,JUMPDEST 14]

λ> P.translate <$> L.translate opcodes
Right [PUSH 1,JUMPDEST,DUP1,ISZERO,PUSH 14,JUMPI,PUSH 2,MUL,PUSH 2,JUMP,JUMPDEST]

λ> fmap opcodeText . P.translate <$> L.translate opcodes
Right ["push1 1","jumpdest","dup1","iszero","push1 14","jumpi","push1 2","mul","push1 2","jump","jumpdest"]
```

## Accounts for size of `PUSH`es when doing absolute jumps

EVM's `jump` and `jumpi` instructions are parameterless. Instead they pop and
jump to the address on the top of the stack. In order to perform absolute jumps
in the code, it is necessary to `PUSH` an address on the stack first.  This is
inconvenient, and so `PositionalOpcode` and `LabelledOpcode` are easier to use.

But what's more inconvenient is what happens to the offset of an absolute jump
when the address being jumped to crosses a boundary where its byte index can no
longer be represented by the same amount of bytes.

Take for example this EVM code:

```
0x00: push1 255
0x02: jump
0x03: stop
0x04: stop
0x05: stop
...
0xfe: stop
0xff: jumpdest
```

which can be represented with the following `LabelledOpcode`:

```haskell
λ> import EVM.Opcode
λ> import EVM.Opcode.Labelled as L
λ> import EVM.Opcode.Positional as P

λ> let opcodes = [JUMP "skip"] <> replicate 252 STOP <> [JUMPDEST "skip"]
λ> fmap (fmap opcodeText . P.translate) (L.translate opcodes)
Right ["push1 255","jump","stop","stop","stop",...,"jumpdest"]
```

Note especially the byte size of a `PUSH 255` vs. a `PUSH 256`:

```haskell
λ> opcodeSize (PUSH 255)
2
λ> opcodeSize (PUSH 256)
3
```

Then add another one-byte opcode between the `jump` and the `jumpdest`:

```haskell
λ> let opcodes = [JUMP "skip"] <> replicate 253 STOP <> [JUMPDEST "skip"]
λ> fmap (fmap opcodeText . P.translate) (L.translate opcodes)
Right ["push2 257","jump","stop","stop","stop",...,"jumpdest"]
```

Even though one byte was added, because the address of `jumpdest` is now
greater than 255, all references to it now take more than 2 bytes. Concretely,
one reference went from 2 bytes to 3 bytes, or rather, one `JUMP "skip"` became
a `push2 257` instead of a `push1 255`. And if there were many such `jump`s,
this amounts to a bit of book-keeping.

This happens at subsequent boundaries as well. While this library handles each
boundary the same way, it is unlikely to have EVM bytecode of more than a few
kilobytes at present time.

```haskell
λ> let opcodes = [JUMP "skip"] <> replicate 65532 STOP <> [JUMPDEST "skip"]
λ> fmap (fmap opcodeText . P.translate) (L.translate opcodes)
Right ["push3 65537","jump","stop","stop","stop",...,"jumpdest"]
```
