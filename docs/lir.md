# Lir

Lir is the low-level intermediate language of AIHC. It sits between GC-GRIN and
the machine backends. The intended pipeline is:

```text
Haskell -> System FC -> GRIN -> CPS-GRIN -> GC-GRIN -> Lir -> AMD64 / ARM64 / WebAssembly / LLVM
```

Lir has one purpose. It gives every backend the same simple input. The runtime
system will also move to Lir. After that move, the optimizer sees one program
without a boundary between user code and runtime code.

This document is the specification. The implementation lives in
`bin/aihc/compiler/lir`. The specification and the implementation change
together.

## Design rules

- Lir is a control-flow graph in static single assignment form.
- Blocks have parameters. Lir has no phi instructions.
- Every operation has a defined result. Lir has no undefined behavior. An
  operation that has no valid result traps.
- Lir has no garbage collector and no exception handler. GRIN makes both
  explicit before it produces Lir.
- Lir has no hidden state. Runtime registers are declared globals.
- The text format and the in-memory representation carry the same information.
  The pretty-printer and the parser round-trip every module.

## Lexical structure

A `;` starts a comment. The comment ends at the end of the line. White space and
comments separate tokens. Layout has no meaning.

Names have three forms:

| Form | Example | Meaning |
| --- | --- | --- |
| `%name` | `%acc` | A value inside a function |
| `@name` | `@sum` | A symbol: a function, a global, or a data object |
| `name` | `loop` | A block label inside a function |

A bare name uses the characters `A-Z`, `a-z`, `0-9`, `_`, `.`, and `$`. A
quoted name is a string literal after the sigil, for example `@"Data.List\u{0}map"`.
A quoted name can contain every character.

A string literal is enclosed in double quotes. The escapes are `\\`, `\"`, `\n`,
`\r`, `\t`, `\0`, `\xHH` with two hexadecimal digits, and `\u{H...}` with one
to six hexadecimal digits. In a name or a trap message, `\xHH` is the character
with that code. In a `bytes` field, `\xHH` is one byte and every other
character contributes its UTF-8 encoding. The pretty-printer emits a bare name
when the name permits it. Otherwise it emits a quoted name. A bare label also
starts with a letter or an underscore, so a label is never confused with a
literal.

Integer literals are decimal with an optional sign. Float literals contain a
decimal point or an exponent, for example `1.5` and `1.0e-3`. The literals
`inf`, `-inf`, and `nan` are also float literals. The literal `null` is the
`ptr` or the `code` value with address zero.

## Types

| Type | Meaning |
| --- | --- |
| `i1` | A boolean. The value is `0` or `1`. |
| `i8`, `i16`, `i32`, `i64` | An integer with the given number of bits. |
| `f32`, `f64` | An IEEE 754 binary float. |
| `ptr` | The address of data. Its size is the target word size. |
| `code` | The address of a function. Its size is the target word size. |

Integer types have no sign. Each operation names the sign it uses. The types
`i8` to `i64` are the integer types. The type `i1` is not an integer type. It
supports only the operations that this document lists for it.

The type `ptr` is opaque. Integer operations do not accept it. `ptr.to_int`
gives the address as an `i64`. On a 32-bit target the high 32 bits are zero.
`ptr.from_int` makes a pointer from an `i64`. On a 32-bit target it discards
the high 32 bits.

The type `code` is the type of a function symbol. A `code` value supports only
`eq`, `ne`, `select`, `load`, `store`, `call.indirect`, and `tailcall.indirect`.
No operation converts between `code` and another type, and no operation
computes a `code` value from an integer. This keeps `code` valid on targets
where a function address is not a memory address. On WebAssembly a `code` value
is an index into the function table.

A literal `n` fits a type `iN` when `n` is in the signed range or the unsigned
range of `iN`. A literal for `i1` is `0` or `1`. A literal for `ptr` is `null`
or the symbol of a data object. A literal for `code` is `null` or the symbol of
a function. A literal for `f32` or `f64` is a float literal or an integer
literal. A global has no address, so its symbol is not a literal.

## Module

A module is a sequence of items. Item order is not significant, except that the
pretty-printer preserves it.

```text
item ::= function | extern-function | global | data | extern-data
```

Every symbol is defined or declared at most once in a module.

### Functions

```text
function ::= "export"? "func" symbol "(" parameters ")" results? cc? "{" block+ "}"
parameters ::= (value ":" type ("," value ":" type)*)?
results ::= "->" type | "->" "(" type ("," type)* ")"
cc ::= "cc" ("aihc" | "c")
```

The default calling convention is `aihc`. The `aihc` convention permits every
arity and every result count. Tail calls are guaranteed. The `c` convention is
the platform C convention. Use it for the host boundary. A function without
`export` is internal to the module.

The first block is the entry block. The entry block has no parameters and no
predecessors. The function parameters are visible in every block.

```text
extern-function ::= "extern" "func" symbol "(" (type ("," type)*)? ")" results? cc?
```

An extern function is defined in another module or in the host.

### Globals

```text
global ::= "global" symbol ":" type "pinned"?
```

A global is one mutable cell of the given type. `global.get` and `global.set`
are the only operations that access it. A global has no address. The initial
value of a global is zero. A `pinned` global tells the backend to keep the cell
in a register when the target permits it. The heap pointer and the heap limit
are pinned globals.

### Data

```text
data ::= "export"? "data" "mut"? symbol "align" integer "=" "{" field ("," field)* "}"
field ::= int-type integer
        | float-type float
        | "ptr" symbol (("+" | "-") integer)?
        | "ptr" "null"
        | "code" symbol
        | "code" "null"
        | "bytes" string
        | "zero" integer
extern-data ::= "extern" "data" symbol
```

A data object is a sequence of bytes in memory with a fixed address. Its
alignment is a power of two. The fields are stored in order without padding.
Integers and floats are little-endian. A `ptr` field stores the address of a
data object plus an addend. The addend gives tagged headers a direct encoding.
A `code` field stores the address of a function. The fields `ptr null` and
`code null` store a word of zero bytes; unlike `zero`, their size follows the
target word size. A `bytes` field stores the UTF-8 encoding of the string. A
`zero` field stores the given number of zero bytes.

A data object without `mut` is read-only. A store to a read-only data object
traps.

### Info tables

An info table describes one kind of heap object. The header of a heap object
is the address of its info table. GC-GRIN emits one info table per object kind
as a read-only data object, so every backend receives the same layout and emits
it as bytes. No backend computes an info table of its own.

Every field of an info table is one word wide. A pointer field is `ptr`, a
code field is `code`, and a count or a kind is an integer of the word width:
`i64` on a 64-bit target and `i32` on a 32-bit target. Field `k` starts at
offset `k` words, and the table is aligned to the word size. A field without a
value is `ptr null`, `code null`, or `0`. The fields are, in order:

| Field | Type | Meaning |
| --- | --- | --- |
| `identity` | `ptr` | The saturated constructor table, or null for a function. Case code compares this field. |
| `entry` | `code` | The portable entry. Null when the object cannot be entered. |
| `field_count` | integer | The number of payload words. |
| `remaining_arity` | integer | The number of arguments the object still requires. |
| `field_is_pointer` | `ptr` | A `bytes` data object with one byte per payload word: `1` for a managed pointer, `0` otherwise. Null when `field_count` is `0`. |
| `next` | `ptr` | The table of the next application stage. Null for the last stage. |
| `backend_entry` | `code` | The direct entry. Null when the object cannot be entered. |
| `frame_kind` | integer | The continuation frame kind for stack unwinding. |
| `object_kind` | integer | Node, closure, thunk, partial constructor, or a runtime object kind. |
| `srt` | `ptr` | The static reference table, or null. |

Every `code` field of every info table has one fixed signature. A backend can
therefore call through a field without a per-object type, which WebAssembly
requires and which costs nothing elsewhere.

| Field | Signature |
| --- | --- |
| `entry` | `(ptr) -> ()` with the address of the argument slots. |
| `backend_entry` | `(ptr, i64, ptr, i64) -> ()` with the machine, the object slot, the address of the supplied argument slots, and the continuation slot. |

GC-GRIN generates a function with the fixed signature for each enterable
object. That function loads the stored fields and the supplied arguments and
tail-calls the generated code. On 64-bit targets the runtime's `AihcInfo`
structure has this layout already. On WebAssembly its counts and kinds shrink
from 64 to 32 bits when GC-GRIN starts to emit Lir.

## Functions and blocks

```text
block ::= label ("(" parameters ")")? ":" instruction* terminator
instruction ::= (value ("," value)* "=")? operation
```

Every value is defined once. A use of a value is valid when its definition
dominates the use. A block parameter is defined at the start of the block.

The last instruction of a block is a terminator. No other instruction is a
terminator.

## Operands

An operand is a value or a literal. The type of an operand comes from the
operation. Block arguments take their types from the block parameters.

## Operations

The notation `T` is a type. `iN` is an integer type. `fN` is a float type. The
operation trap conditions are the complete list. An operation with no trap
condition never traps.

### Integer arithmetic

| Operation | Result | Semantics |
| --- | --- | --- |
| `add iN %a, %b` | `iN` | Wrapping addition. |
| `sub iN %a, %b` | `iN` | Wrapping subtraction. |
| `mul iN %a, %b` | `iN` | Wrapping multiplication. |
| `div.s iN %a, %b` | `iN` | Signed division. Rounds toward zero. Traps when `%b` is zero. Traps when the result does not fit. |
| `div.u iN %a, %b` | `iN` | Unsigned division. Traps when `%b` is zero. |
| `rem.s iN %a, %b` | `iN` | Signed remainder. The sign follows `%a`. Traps when `%b` is zero. |
| `rem.u iN %a, %b` | `iN` | Unsigned remainder. Traps when `%b` is zero. |
| `and iN %a, %b` | `iN` | Bitwise and. Also accepts `i1`. |
| `or iN %a, %b` | `iN` | Bitwise or. Also accepts `i1`. |
| `xor iN %a, %b` | `iN` | Bitwise exclusive or. Also accepts `i1`. |
| `shl iN %a, %b` | `iN` | Shift left. The count is `%b` modulo `N`. |
| `shr.s iN %a, %b` | `iN` | Arithmetic shift right. The count is `%b` modulo `N`. |
| `shr.u iN %a, %b` | `iN` | Logical shift right. The count is `%b` modulo `N`. |
| `mul.wide.s iN %a, %b` | `iN, iN` | Signed full multiplication. The results are the low and the high half. |
| `mul.wide.u iN %a, %b` | `iN, iN` | Unsigned full multiplication. The results are the low and the high half. |
| `add.carry iN %a, %b` | `iN, i1` | Wrapping addition and the unsigned carry. |
| `sub.borrow iN %a, %b` | `iN, i1` | Wrapping subtraction and the unsigned borrow. |

### Comparison

| Operation | Result | Semantics |
| --- | --- | --- |
| `eq T %a, %b` | `i1` | Equal. `T` is any type, including `code`. Float comparison is IEEE 754. |
| `ne T %a, %b` | `i1` | Not equal. `T` is any type. |
| `lt.s iN %a, %b` | `i1` | Signed less than. |
| `lt.u T %a, %b` | `i1` | Unsigned less than. `T` is an integer type or `ptr`. |
| `le.s iN %a, %b` | `i1` | Signed less than or equal. |
| `le.u T %a, %b` | `i1` | Unsigned less than or equal. `T` is an integer type or `ptr`. |
| `gt.s iN %a, %b` | `i1` | Signed greater than. |
| `gt.u T %a, %b` | `i1` | Unsigned greater than. `T` is an integer type or `ptr`. |
| `ge.s iN %a, %b` | `i1` | Signed greater than or equal. |
| `ge.u T %a, %b` | `i1` | Unsigned greater than or equal. `T` is an integer type or `ptr`. |
| `flt fN %a, %b` | `i1` | Ordered less than. False when an operand is NaN. |
| `fle fN %a, %b` | `i1` | Ordered less than or equal. |
| `fgt fN %a, %b` | `i1` | Ordered greater than. |
| `fge fN %a, %b` | `i1` | Ordered greater than or equal. |

`eq` and `ne` on `fN` follow IEEE 754. NaN is not equal to any value.

### Float arithmetic

| Operation | Result | Semantics |
| --- | --- | --- |
| `fadd fN %a, %b` | `fN` | IEEE 754 addition. |
| `fsub fN %a, %b` | `fN` | IEEE 754 subtraction. |
| `fmul fN %a, %b` | `fN` | IEEE 754 multiplication. |
| `fdiv fN %a, %b` | `fN` | IEEE 754 division. Division by zero gives an infinity or NaN. |
| `fneg fN %a` | `fN` | Negation. |
| `fabs fN %a` | `fN` | Absolute value. |
| `fsqrt fN %a` | `fN` | Square root. |

### Conversion

| Operation | Result | Semantics |
| --- | --- | --- |
| `sext iN %a to iM` | `iM` | Sign extension. `M` is greater than `N`. Also accepts `i1` as `iN`. |
| `zext iN %a to iM` | `iM` | Zero extension. `M` is greater than `N`. Also accepts `i1` as `iN`. |
| `trunc iN %a to iM` | `iM` | Truncation. `M` is less than `N`. Also accepts `i1` as `iM`. |
| `itof.s iN %a to fM` | `fM` | Signed integer to float. Rounds to nearest even. |
| `itof.u iN %a to fM` | `fM` | Unsigned integer to float. Rounds to nearest even. |
| `ftoi.s fN %a to iM` | `iM` | Float to signed integer. Rounds toward zero. Traps on NaN and out of range. |
| `ftoi.u fN %a to iM` | `iM` | Float to unsigned integer. Rounds toward zero. Traps on NaN and out of range. |
| `fpext f32 %a to f64` | `f64` | Widen a float. |
| `fptrunc f64 %a to f32` | `f32` | Narrow a float. Rounds to nearest even. |
| `bitcast T %a to U` | `U` | Reinterpret the bits. `T` and `U` have the same width. One is a float type and one is an integer type. |
| `ptr.to_int %p` | `i64` | The address of a pointer. |
| `ptr.from_int %i` | `ptr` | The pointer with the given address. |

### Selection

| Operation | Result | Semantics |
| --- | --- | --- |
| `select T %c, %a, %b` | `T` | `%a` when `%c` is `1`. `%b` otherwise. `%c` is `i1`. `T` is any type. |

### Memory

```text
address ::= "[" value (("+" | "-") integer)? "]"
```

The base of an address is a `ptr` value. The offset is a constant.

| Operation | Result | Semantics |
| --- | --- | --- |
| `load T address align A` | `T` | Read a `T` from the address. Traps when the address is not mapped. Traps when the address is not a multiple of `A`. |
| `store T %v, address align A` | none | Write `%v` to the address. Traps when the address is not mapped, is read-only, or is not a multiple of `A`. |
| `ptr.add %p, %i` | `ptr` | Add an `i64` to a pointer. The addition wraps at the target word size. |
| `stack.alloc N align A` | `ptr` | Reserve `N` bytes of stack memory. The memory is zero. It lives until the function returns. Only the entry block may contain this operation. |

`A` is a power of two. `T` is `i1` only for `load` and `store` of one byte. `T`
may be `code`. Loading a `code` value from bytes that are not the address of a
function gives a value that traps in `call.indirect`.

### Globals

| Operation | Result | Semantics |
| --- | --- | --- |
| `global.get @g` | the type of `@g` | Read the global. |
| `global.set @g, %v` | none | Write the global. |

### Calls

| Operation | Result | Semantics |
| --- | --- | --- |
| `call @f(args)` | the results of `@f` | Call a function or an extern function. |
| `call.indirect %p(args) : signature` | the results of the signature | Call the code at `%p`. `%p` is `code`. Traps when `%p` is not a function with the same signature. |

```text
signature ::= "(" (type ("," type)*)? ")" results? cc?
```

The argument types are the parameter types of the target. A call with a `c`
convention target may have at most one result.

## Terminators

```text
target ::= label ("(" (operand ("," operand)*)? ")")?
```

The arguments of a target match the parameters of the block.

| Terminator | Semantics |
| --- | --- |
| `jump target` | Continue at the target. |
| `br %c, target1, target2` | Continue at `target1` when `%c` is `1`. Otherwise continue at `target2`. `%c` is `i1`. |
| `switch iN %v { case -> target ... default -> target }` | Continue at the target of the case that equals `%v`. Without a match, continue at the default target. Without a default, trap. |
| `return operands` | Return the operands. Their types are the result types of the function. |
| `tailcall @f(args)` | Replace the current activation by a call of `@f`. The result types and the calling convention of `@f` equal those of the current function. |
| `tailcall.indirect %p(args) : signature` | The indirect form of `tailcall`. |
| `trap "message"` | Stop the program with the message. |

Switch cases are distinct literals that fit `iN`.

## Traps

A trap stops the program. The message of a trap is one of these strings or the
message of a `trap` terminator:

| Message | Cause |
| --- | --- |
| `integer division by zero` | `div.s`, `div.u`, `rem.s`, or `rem.u` with a zero divisor. |
| `integer overflow` | `div.s` of the minimum value by minus one. |
| `invalid float to integer conversion` | `ftoi.s` or `ftoi.u` of NaN or of an out of range value. |
| `memory access out of bounds` | A load or a store outside mapped memory. |
| `misaligned memory access` | A load or a store with an address that is not a multiple of the alignment. |
| `store to read-only data` | A store to a data object without `mut`. |
| `indirect call to a non-function` | `call.indirect` of a `code` value that is not the address of a function, for example `null`. |
| `indirect call signature mismatch` | `call.indirect` of a function with a different signature. |
| `switch without a matching case` | A `switch` without a default and without a matching case. |
| `stack overflow` | The stack memory is exhausted. |

## Lint

The linter checks every rule of this document that the parser cannot check. A
module passes the linter before it reaches a backend. The linter reports each
error as `@symbol/block: message`. It omits the block, or the symbol and the
block, when they do not apply.

The test fixtures in `bin/aihc/compiler/lir/test/Test/Fixtures/lir/lint` give
the exact text of every error. Each fixture is a Lir module. Each header comment
`; error: <text>` is one expected error. The test asserts the complete list in
order.

## Interpreter

The interpreter is the reference implementation of this document. It executes a
module from a named function and reports the results or the trap. Memory is a
flat address space. Data objects, the stack, and code addresses have distinct
regions. Code addresses are not readable, and a `ptr.from_int` of a code
address is a pointer that traps on `load` and `store`. The interpreter uses a
64-bit word size. It cannot call an extern function.

The interpreter renders results with their declared types. An `iN` result is a
signed decimal. An `i1` result is `0` or `1`. A float result uses the Haskell
`show` format. A `ptr` or a `code` result is a hexadecimal address.

The test fixtures in `bin/aihc/compiler/lir/test/Test/Fixtures/lir/eval` are
Lir modules with a function `@main` without parameters. The header comment
`; expect: <results>` gives the rendered results separated by `, `. The header
comment `; expect-trap: <message>` gives the trap message instead. Every
fixture also passes the linter and the pretty-printer round-trip.

## Binary format

The binary format is not specified yet. It will use deterministic CBOR with a
symbol table and dense value indices.

## Example

```text
func @sum(%xs: ptr) -> i64 {
entry:
  jump loop(%xs, 0)

loop(%p: ptr, %acc: i64):
  %tag = load i8 [%p] align 1
  switch i8 %tag {
    0 -> done(%acc)
    1 -> cons(%p, %acc)
  }

cons(%cell: ptr, %sum: i64):
  %x = load i64 [%cell + 8] align 8
  %next = load ptr [%cell + 16] align 8
  %sum2 = add i64 %sum, %x
  jump loop(%next, %sum2)

done(%result: i64):
  return %result
}
```
