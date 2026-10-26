# Native runtime objects

The Apple ARM64 and Linux AMD64 backends share the runtime ABI, C runtime,
constructor/global link layout, and snapshot support from `aihc-native`. Both
consume Lir (see `docs/lir.md`); only instruction selection and object
emission belong to the architecture packages.

Both backends are built on every platform. `aihc compile` defaults to the host
target on supported hosts and accepts an explicit target for cross-compilation:

```text
aihc compile Main.hs --target apple-arm64
aihc compile Main.hs --target linux-amd64
```

The selected LLVM target triple is passed to Clang for dependency objects, the
shared runtime, and the final executable. Cross-linking therefore requires a
Clang installation with the corresponding target linker and sysroot.

## RTS options

Compiled programs accept RTS options between `+RTS` and `-RTS` arguments.
The runtime removes these arguments before `getArgs` reads the argument vector.
Use `--RTS` to stop RTS option processing for all subsequent arguments.

The `-M<size>` option sets the maximum managed heap size in bytes.
The size accepts an optional `K`, `M`, `G`, or `T` binary unit.
Lower-case units have the same meaning.
The default heap size is unlimited.

The limit bounds the objects of the old generations and the pinned list.
It does not count the nursery, the unused part of a block, collector
metadata, or static objects.
When a collection cannot bring the heap below the limit, the collector runs a
full collection, and the program stops if the heap is still above it.

The `-A<size>` option sets the size of the nursery, 4 MiB by default.
The runtime rounds this size up to a power of two and aligns the nursery arena to that size.
The machine owns the arena, and the capability keeps its allocation cursor.
A smaller heap limit can reduce the allocation budget within an aligned window.
The `-n<size>` option sets the nursery block size, 32 KiB by default.
Zero selects the default block size.
A block refill extends the allocation limit without a collection.
Fixed allocations and stack charges share the nursery budget with movable objects.
The `-R<size>` option sets the budget of barrier work between two collections of a capability, the nursery size by default.
The `-N<count>` option gives the machine that many capabilities, at most 256, one by default.
With more than one capability, each capability has a worker on its own OS thread.
A capability with surplus threads pushes threads to an idle capability.
The workers run Haskell threads in parallel, and every collection stops the other workers.
A store of a young value into a global object promotes the value.
A wasm32 host keeps one capability.
The write barrier counts the scan work that it adds: the size of each object that it puts in the remembered set, and one run of 128 elements for each card that it makes dirty.
When the work reaches the budget, the heap limit goes below the nursery, and the next reservation collects instead of a block refill.
A compiled store site loads the heap limit again after its barrier call.
The `-B<count>` option sets the number of collections that a survivor stays in gen1, 1 by default.
The collection at the end of this time copies all of gen1 into gen2.
A count of zero copies the survivors of the nursery into gen2 directly.
The `-F<factor>` option sets the growth of gen2 between two gen2 cycles,
2 by default.
The `-k<factor>` option sets the mark work of a gen2 cycle for each byte
promoted into gen2 since the last slice, 2 by default.
The `wasm32-wasip3` host starts its machine before it parses the arguments,
so a `-A` option takes effect only when the nursery is still empty.

Static reference tables determine static object liveness in a full collection.
A smaller collection reaches a static object only through the remembered set.

## Initialization and host scopes

One static machine controls one program heap.
The host initializes this heap before it imports arguments or environment data.
Startup uses explicit root scopes for temporary buffers.
After these scopes end, the runtime copies retained data into movable arrays.
These arrays contain only the retained bytes.
The collector then removes temporary imports.
The machine then applies the parsed heap limit and starts the main thread.
After this transition, the runtime rejects further startup imports.
Retained arguments and environment data count toward this limit.
A limit below the retained startup data causes failure before user code starts.

The machine holds the global table in a managed pinned byte array.
The collector traces each global table slot.
Argument replacement installs an immutable `ByteArray#` as a process root.
It does not allocate or copy the array.
The collector updates this root when the array moves and reclaims previous arrays when they become unreachable.

Normal runtime primitives consume reservations from GRIN safepoints.
They cannot collect inside their C calls.
The scheduler and startup can also establish explicit host safepoints with `AihcRootFrame`.
Each frame publishes all live managed C references before a host call can collect.
The collector updates these reference slots after relocation.
The runtime must reload relocated references from their slots.

`aihc_host_byte_array` requires an active root scope and uses the same heap and statistics as program allocation.
Its pinned buffers hold raw data for host interfaces that need fixed addresses.
Paths and POSIX poll arrays use call scopes.
WASI canonical ABI buffers use a scope that remains active until the asynchronous operation ends.
Generated buffer destructors defer memory reclamation until that scope ends.
Resource destructors still close their host resources.
The next collection can reclaim buffers after their scope ends.
No allocation occurs before GC initialization or outside a reservation or explicit host safepoint.

The collector alone obtains backing storage and metadata from the C allocator.
The runtime has no second heap or general auxiliary allocator.
The thread stacks below are the only other storage, and the collector file owns them too.

## Thread stacks

Continuation frames are not in the managed heap.
Each thread has a stack, and the frames of the thread are on this stack.
A stack is a doubly linked list of chunks.
Each chunk has 4096 bytes and the same alignment.
Thus the chunk of an address is the address with the low 12 bits cleared.
A chunk starts with a 64-byte header: the owner stack, the chunks below and above, the depth of the chunk, its age, and the scan record of the gen2 marking.
The frames of a chunk follow the header.

The stack pointer is the first free byte of the stack of the running thread.
The stack limit is the end of the chunk that holds the byte before the stack pointer.
Compiled code keeps both values in registers and passes them to each function that it transfers control to.
Refer to "Lowering from GC-GRIN" in [lir.md](lir.md).
A push writes the frame at the stack pointer and increases it.
The frame fits when the new stack pointer is not above the stack limit.
If the frame does not fit, `aihc_stack_grow` puts it at the start of the next chunk.
Then the stack limit is the end of that chunk.
A new chunk comes from the C allocator, so a push never collects.
The next minor collection scans each young chunk.
Thus `aihc_stack_grow` charges the next chunk to the nursery, as a large object does.
It adds the chunk bytes to `fixed_since_gc` and lowers `heap_limit` by them.
It charges a chunk one time for each collection, so a push and a pop across one boundary do not collect again and again.
After the call, the push loads the heap limit again.
When the charges use the nursery, `heap_limit` goes below the nursery start, and each reservation collects.
The first charge after a collection only empties the room, so each collection is followed by at least one chunk of progress.
The GC pass gives each frame push a reservation of zero words, so compiled code collects after about one nursery of stack growth, also when it does not allocate.
A frame push that only goes to an evaluation gets no reservation, because `aihc_lir_eval` and `aihc_lir_eval_single_entry` collect at their entry when the heap pointer is above the heap limit.
`aihc_lir_eval` collects in the same condition when its push of an update frame grows the stack.
Refer to "Pause bound" in [gc-design.md](gc-design.md).
A frame keeps the address of its parent in field zero, in any chunk.
Thus a chunk boundary needs no link frame.
The largest frame has 256 words, so a frame always fits in an empty chunk.

A continue helper sets the stack pointer to the address of the frame that it enters.
This pops the frame and every frame above it.
The stack limit becomes the end of the chunk of that frame.
An application resume sets the stack pointer to the first byte after its continuation.
These two rules are correct because code always pushes a frame directly above its current continuation.
The current continuation is thus always the topmost live frame.

Each stack chunk header has a capability pointer.
The header keeps its 64-byte size.
The depth and collection charge counters each use 32 bits.
Stack growth, chunk entry, scheduler resume, and callback entry set the capability pointer.
Compiled code reads it through the stack limit before a C runtime call.
The capability points to the shared process state in `AihcMachine`.

The capability field `stack_next` holds a copy of the stack pointer.
Compiled code stores the stack pointer there before a runtime call that can read it, such as a collection or a scheduler operation.
The scheduler writes `stack_next` when it selects a thread, and `aihc_lir_resume` loads the stack pointer from there.
The runtime pushes the frames of the list below through `stack_next` while no compiled code runs.
The capability fields `heap_next` and `heap_limit` are the same kind of copy of the heap pointer and the heap limit.

The frame layouts do not change, because each frame has its info table.
The collector finds a live frame through a pointer to it.
It marks the frame as it marks a static object and scans the frame in place.
Frames do not move, so a pointer to a frame needs no relocation.
A stack stays while the collector retains its thread record.
After each full collection and at the end of each gen2 cycle, the collector releases the stacks of the threads that it did not retain.
A collection of the younger generations keeps every thread record, because each record is in gen2.
It also releases the chunks above the chunk after the running chunk.
When a thread finishes, the runtime releases its stack immediately.
The machine keeps up to 64 released chunks for later growth.

Stack chunks are not in the heap statistics or in the `-M` limit.

The runtime puts these frames on the stack as well:

- The update frame that `aihc_lir_eval` pushes when it enters a thunk.
- The final and top continuations of the main thread, and the thread-done frame at the bottom of each forked thread.
- The stop frame of a foreign callback. The callback runs on the stack of the thread that made the foreign call.
- The frames that a `control0#` resume pushes again.

`control0#` copies the frames between the top and the prompt to the managed heap.
The copies link from the top down, and the lowest copy has a null parent.
A resume pushes new copies of these frames on the stack from the bottom up.
It never writes the heap copies, so a captured continuation can resume any number of times.

## Heap regions

Every managed allocation lives in a run of regions of 64 KiB.
An aligned run can require a larger host mapping.
The region allocator keeps the unused prefix and suffix in its free list.
A two-level region table gives the kind of the region that holds an address.
The top level is indexed by the address bits above the leaf, and a leaf
covers 4 GiB with one entry for each region. The runtime reserves no address
space in advance, so it runs where a large reservation is refused. The
kinds are:

| Kind | Content |
| --- | --- |
| `OUTSIDE` | Memory the runtime did not acquire: static data, C allocations, or the memory of another allocator |
| `FREE` | A region the runtime can acquire |
| `NURSERY` | The nursery |
| `GEN1` | A block of gen1 |
| `GEN2` | A segment of gen2 |
| `FROM1` | A block of gen1 that the running collection copies away |
| `LARGE` | A large object that never moves, with the card table of a boxed array behind it |
| `PINNED` | A large pinned byte array or host buffer |
| `STACK` | Sixteen stack chunks of 4 KiB |

A released run waits in a free list for the next run that fits. When every
region of a mapping is free and the free list holds more than 64 MiB, the
mapping goes back to the host. On a POSIX host a mapping is one private
anonymous mapping. On `wasm32-wasip3` a region is one WebAssembly page, a
mapping is one growth of the linear memory, and the memory never shrinks.
The pages of the C allocator keep the kind `OUTSIDE`.

## Generations

The heap has three generations. The nursery, generation zero, is one run of
regions that compiled code fills with a bump pointer. Gen1 is a list of
blocks of 256 KiB that only the collector fills. Gen2 is a set of segments
of 256 KiB that never move an object. A collection of the generations up to
g copies every live object of the nursery and gen1 that it covers one
generation up, and a full collection marks the live gen2 objects in place.
The policy is:

- A minor collection runs when the nursery is full. It copies the live
  nursery objects into gen1.
- When gen1 is above its maximum, the collection copies gen1 into gen2 as
  well, and the nursery survivors into fresh gen1 blocks.
- When gen2 has grown by the factor since the last full collection, or the
  `-M` limit is near, the collection is a full one: it copies the nursery
  and gen1 into gen2 and marks gen2.

A copied object keeps its new address in its old header with the low two
bits set to two. No live header has that pattern: the second bit is set only
on a blackhole, whose first bit is set as well. Heap indirections in a
copied generation are followed and not copied, and a full collection
follows a gen2 indirection and leaves it unmarked. An indirection in an
older generation stays until a collection of that generation.

### Gen2 segments

A segment is one block of 256 KiB that holds slots of one size class. The
size classes are the word counts two to eight and then four classes in each
doubling, up to 4096 words, so a slot holds at most a quarter more than its
object needs, and every object below the large object bound fits a class.
The segment header holds a bitmap with one bit for each slot. A set bit is
an occupied slot. An object copied into gen2 sets its bit when it is
allocated, so the bitmap is the free map of the allocator between full
collections. The allocator fills one segment for each size class and takes
the first free slot above its cursor.

A full collection marks in place. The header of a segment records the
epoch, the count of full collections, whose marks its bitmap holds. The
collection increments the count and clears the bitmap of a segment when it
first marks an object in it, so the start of a full collection costs
nothing for each segment. It sets the bit of each gen2 object it reaches
and queues the object for a scan: gen2 has no Cheney cursor, because its
slots are not in allocation order. While the collection traces, objects
copied into gen2 go to new segments only, because the marks of an older
segment are not final until the trace ends.

The sweep is lazy. At the start of a full collection every segment goes to
the unswept list of its class. The allocator sweeps an unswept segment of a
class when the class has no segment with room, and every collection sweeps
a bounded slice of 64 unswept segments when it ends. A sweep counts the set
bits: a segment with none, and a segment whose epoch is older than the last
full collection, goes back to the region table, a full segment waits for
the next full collection, and the others are available to the allocator.

The bytes of gen2 are the bytes of its occupied slots. A cycle counts the
bytes it marks and the slots allocated while it runs, and gen2 takes that
count when the cycle ends. A fixed block is a large object or a pinned
block. The `-F` growth rule reads this count plus the charges of the gen2
fixed blocks. The `-M` limit reads this count plus the used bytes of gen1 and
the charges of all fixed blocks.

### Gen2 cycles

Gen2 is collected by a snapshot-at-the-beginning marking that runs in
slices on the mutator thread. A cycle starts at the end of a collection
that copied gen1 into gen2, when gen2 is above its limit. The snapshot
marks the roots, scans the gen1 objects and the young pinned blocks, which
are older than the snapshot and outside the barrier, and scans the top
chunk of each stack. A full collection runs a whole cycle in one pause: it
starts a cycle, marks what it reaches while it copies, and ends the cycle.
A full collection while a cycle is active gives the cycle up and marks
again from the start.

The write barrier shades: before a store into an object the cycle marks,
the old values of its pointer fields are marked, and a store into an array
shades the element or the run it writes. A thunk update shades every
field, and it walks the static reference table of the thunk code. The
update deletes that code, and the objects the code made after the snapshot
can name the static objects of the table. The marker later finds an
indirection, which has no table. Marking follows the current fields of an object, so a value stored
after the snapshot is marked when it is old, and left to the young
collections when it is young. An object copied into gen2 while a cycle
runs is marked when it is allocated. A gen2 indirection is marked like any
object, because the marker does not rewrite the fields that name it.

Frames are write-once, and a pop deletes them without a store. The marker
scans a frame together with the frames below it in its chunk, down to the
frames an earlier scan covered, and records in the chunk the highest frame
it was scanned from. When the chain leaves the chunk, the frame below goes
on the list of pending frames of the stack, unless an earlier scan covered
it. A slice scans a pending frame, or the pop that enters its chunk does
through `aihc_stack_enter_chunk`. The list keeps every pending frame,
because a scan stops at the first chunk that the cycle scanned, and the
chunks below that chunk can still wait for a scan. A pop can pass a frame
without a read, because a forward frame gives its values to its parent.
Thus `aihc_stack_enter_chunk` removes the pending frames at or above the
frame that it enters, scans the entered frame at once, and a slice never
scans a frame that new frames replaced. The pending list holds live frames
only.
The exception walk and
the continuation capture read frames they pop, so they scan them first
through `aihc_gc_frame_read`. A frame named by a heap object is scanned
only when it is live: at or below the stack pointer of the running stack,
or at or below the frame its thread suspended with, which
`aihc_stack_note_top` records. The copy of a young collection uses the
same rule, because a dead object in the remembered set can name a frame
that is popped. A record that gives its continuation to a thread, such as
a woken MVar or blackhole waiter, a selected thread, or a completed IO
request, sets its continuation to null. A popped frame can be under new
live frames, so the liveness rule alone does not cover such a record.

Each collection ends with a slice. The slice does the mark work the
promotions since the last slice owe, `-k` times their bytes, within a
floor of 256 KiB and a cap of 4 MiB of scanned bytes, the size of the
default nursery. A collection that copied more than the cap may mark as
much as it copied, so a gen1 collection that moves gen1 into gen2 marks
the data it added within a pause of the same order. A boxed array is
scanned in pieces of 4096 elements. The cycle ends at a slice that finds
the mark stack, the static reference tables, and the pending frames of
every stack empty. The end drops the remembered set entries of unmarked
heap objects, the stable names and the stacks of unmarked objects, frees
the unmarked gen2 blocks of the pinned list, and hands the segments to the
lazy sweep. A static object keeps its remembered set entry. When gen2
has doubled since the snapshot while a cycle is active, the next collection
is a full one: this is the degradation mode.

When the collector scans an object of generation k, it copies each referent
that moves into generation k at least. Thus an old object points only at old
objects after one scan. When a referent still ends younger, because an
earlier reference copied it there, the object goes back to the remembered
set. A waiter that names a frame goes back as well while
the chunk of the frame can be scanned again: a chunk the collection scans
counts as young, and another chunk counts as its own age. The chunk is
reached only through the waiter, so the waiter must stay in the set until
the chunk is older than every collection that could scan it. The
resumption of a thread record is a root, so a record needs no such entry.

A record that deletes a pointer takes the write barrier first, so the
deletion barrier of an active cycle shades the old value. The run queue
dequeue, the pop of an MVar waiter, and the
completion of an IO request are such deletions. Without the barrier, a
thread that the snapshot reached only through the queue link of a dequeued
record is never marked, and the end of the cycle frees its record and
releases its stack.

### Write barrier and remembered set

A pointer store into an existing object outside the local nursery records the
object in the remembered set.
Compiled code reads the capability from the stack chunk header.
It loads `heap_start` and `nursery_bytes` from that capability.
The test uses an exclusive or with the aligned base and an unsigned compare against the size.
It calls `aihc_write_barrier` for every other object.
The stable base gives the correct result when the heap pointer equals the nursery end.
The sites are the array and MutVar stores and compare-and-swap
primitives in the Lir lowering, the inline thunk update in
`aihc_helpers.lir`, the array copy in `aihc_array.lir`, and every store of
a pointer into an existing object in the C runtime: thread resumption,
MVar operations, blackhole waiters, IO requests, transactions, and
`aihc_set_field`. A thunk update of an old blackhole takes the C path,
`aihc_update`, which calls `aihc_write_barrier_update`.

The remembered set is a list of objects. A store into the object that
entered last is not recorded again. A hot object still enters the list at
many stores, so the list is compacted: a sort removes the duplicates. Only
an entry from `aihc_remember` can repeat an object. The update barrier adds
a blackhole, which is updated once, and a collection adds each object that
it keeps once. Those entries do not count. The list is compacted when it is
full and at least half of it is entries that can repeat, and before a
collection scans it when it has such an entry. Without this rule, a program
that updates many old thunks sorted the whole list again each time it was
full. A collection scans each entry with the generation of the entry
as the floor of its referents and then drops the entry. An entry in a
copied generation is dropped unscanned: the object is copied and scanned if
it is live. A static entry is dropped unscanned in a full collection, which
traces static objects through the reference tables alone, so an evaluated
CAF that no live code reaches gives its value up.

A large boxed array has a card table: one byte for each run of 128
elements, behind the array in its region run. A store through `writeArray#`,
`writeSmallArray#`, or the compare-and-swap primitives calls
`aihc_write_barrier_at` with the index of the element, and the array copy
calls `aihc_write_barrier_range` with the run it writes. The barrier sets
the cards of the run and records the array. A collection that scans the
array from the remembered set scans only its dirty cards. A card becomes
clean when every referent of its run is in the generation of the array or
above, and the array leaves the remembered set when every card is clean. A
collection that reaches the array as a new object scans every card. The
barrier without an index sets every card of a large array.

### Objects that never move

A large object and a pinned block keep their generation and the mark of the
running collection in the second slot of their pinned block header: the low
56 bits are the charge, bits 56 to 59 the generation, and bit 63 the mark.
A collection of the generations up to g frees every unmarked block of
generation g or below. A marked block takes the generation of its first
referrer, as a copied object does. A small pinned block is a C allocation
outside every region. The collector tells it from a static object by its
kind: the runtime allocates pinned only byte arrays and IO requests, and no
static object has either kind.

Each capability keeps its young fixed blocks on its own list, and the
machine keeps the gen2 fixed blocks on one list. A collection sweeps the
young lists of the capabilities that it collects. Only a full collection
sweeps the gen2 list. Thus the sweep of a local collection does not walk
the gen2 blocks or the blocks of another capability. A block that a
collection or a promotion moves into gen2 goes to the gen2 list at the next
sweep of its young list. The sweep at the end of a gen2 cycle walks every
list, because a young list can hold such a block until then.

A fixed block is a large object or a pinned block. The machine keeps the
charges of the fixed blocks of each generation in `fixed_generation_bytes`.
A new fixed block counts in generation zero. A promotion moves the charge of
a fixed block to its new generation. The sweep removes the charge of each
fixed block that it frees. A gen1 fixed block goes into gen2 with the rest of
gen1 when the `-B` count ends. The gen2 limit test and the doubling test of a
cycle add the charges of the gen2 fixed blocks to the gen2 bytes. The next gen2 limit after a cycle also
includes these charges. Thus a dead old fixed block causes a collection
before the `-M` limit is near.

A static object is older than every generation. A full collection marks
the static objects it reaches in an address set and scans each one once.

### Stack chunk ages

A continuation frame is write-once, so a stack chunk carries one age for
its frames. A collection of the generations up to g scans the frames it
reaches in chunks of generation g or below. When the collection ends, each
chunk with a scanned frame takes generation g plus one, or the youngest
generation its frames refer to when that is lower, so a collection of that
generation scans the chunk again. The chunk of the running stack pointer
stays young, because compiled code pushes into it without a runtime call.
The walk of a collection stops at the first chunk older than the collected
generations, so no chunk may be younger than a chunk above it: when a chunk
takes a low age, the chunks above it take that age as well.

A chunk becomes young again when the stack pointer enters it from above.
The continue helpers and the inline returns of compiled code compare the
stack limit of the entered frame with the previous one. On a change, a
continue helper calls `aihc_stack_enter_chunk`. A resumed
thread makes the chunk of its continuation young in `aihc_lir_take_resume`,
and a new or reused chunk starts young.

Stack chunks come from `STACK` regions. A released chunk goes to the spare
list of the machine and is used again. Stack regions are not in the heap
statistics or in the `-M` limit.

## Runtime statistics

Set the environment variable `AIHC_RTS_STATS` to a file path to get the
runtime statistics of a program. When the program exits normally, the runtime
writes one JSON object to that file:

```json
{"schema": 4, "peak_heap_bytes": 0, "allocated_bytes": 0, "gc_count": 0, "gc_time_ns": 0, "gc_max_pause_ns": 0, "live_bytes": 0, "gc_minor_count": 0, "gc_gen1_count": 0, "gc_full_count": 0, "escape_count": 0, "push_count": 0, "gc_nursery_count": 0, "gc_barrier_count": 0, "gc_large_count": 0, "gc_escape_count": 0, "gc_push_count": 0, "gc_heap_limit_count": 0, "gc_request_count": 0, "gc_gen2_limit_count": 0}
```

A normal exit is a return from `main` or an `exitWith` call. A runtime failure
writes no file. An empty value counts as an unset variable.

- `peak_heap_bytes` records the maximum occupied space for movable objects and pinned blocks.
  The runtime samples this count before collection and at exit.
- `allocated_bytes` counts actual movable allocations and complete pinned block charges of all capabilities.
  Unused reservations and collector copies do not increase this count.
  Host buffers use the same count and heap budget.
- `gc_count` is the number of collections.
- `gc_time_ns` is the monotonic time the collections took, in nanoseconds.
- `gc_max_pause_ns` is the monotonic time of the longest collection, in nanoseconds.
- `live_bytes` is the occupied space directly after the last collection.
  It counts the objects of the old generations and the pinned blocks.
- `gc_minor_count`, `gc_gen1_count`, and `gc_full_count` count the
  collections that copied the nursery alone, the nursery and gen1, and
  every generation.
- `escape_count` counts the stores that gave an object outside the local
  heap a value of the local heap. The local heap of a capability is its
  nursery and its gen1. The count covers the compiled store sites into
  references and boxed arrays, the update of a thunk, `MVar` puts, `TVar`
  writes, and `aihc_set_field`. In the end state of
  [multi-threaded-rts-design.md](multi-threaded-rts-design.md) each such
  store promotes its value. The count adds the stores of all capabilities.
- `push_count` counts the threads that a capability pushed to an idle
  capability.
- The seven `gc_*_count` fields from `gc_nursery_count` to
  `gc_request_count` divide `gc_count` by the reason that started each
  collection. `gc_nursery_count` counts a reservation that found no room
  in the nursery. `gc_barrier_count` counts barrier work that reached the
  `-R` budget. `gc_large_count` counts fixed allocations that reached the
  nursery size. `gc_escape_count` counts a promotion that reached an
  object that a copy cannot promote. `gc_push_count` counts a push of
  threads to an idle capability. `gc_heap_limit_count` counts a full
  collection for the `-M` limit. `gc_request_count` counts a request of
  the program or of the runtime start.
- `gc_gen2_limit_count` counts the collections that copied gen1 only
  because gen2 was above its limit.

### Allocation profile

`aihc build --profile-allocations` makes a whole-program build (the flag
implies `--lto`) that counts the heap objects the program allocates. The
statistics object then has one more field, `allocations`: one entry for each
info table that allocated, the most bytes first.

```json
"allocations": [
  {"name": "C aihc-prim-0.13.0:GHC.Types::", "objects": 3614166, "bytes": 86739984},
  {"name": "F exe:Main:$main_argument_thunk", "objects": 2891008, "bytes": 69384192}
]
```

- The first letter of a name gives the kind of object: `C` a constructor, `F`
  a thunk, and `P` a closure or a partial application. The rest is the
  package, the module, and the name of the constructor or the function. A
  partial application also shows the number of argument groups it waits for.
- The counts are the objects that the generated code allocates. Objects that
  the runtime allocates, such as byte arrays, buffers, and the partial
  applications of `aihc_apply_slow`, are not in the list, so the sum of the
  list can be less than `allocated_bytes`. Continuation frames live on thread
  stacks, so they are not in the list either.
- The counters cost a load, an add, and a store for each count at each
  allocation, so a profiled program runs more slowly. Only measure the
  counts of such a program, not its time.

The lowering keeps two counters for each info table (the objects and the
words) in the exported data `aihc_allocation_profile_counts`, with the names
in `aihc_allocation_profile_names` and their number in
`aihc_allocation_profile_size`. Only the whole-program object defines them,
because the counters of two units would have the same symbols. The `main` of
the entry unit calls `aihc_allocation_profile_register` with the three before
the machine starts, and `aihc_runtime_statistics_report` writes the entries.

### Time profile

`aihc build --profile-time` makes a whole-program build (the flag implies
`--lto`) that samples the Haskell stack of the program. At exit, the program
writes the samples in the folded stack format to the file that
`AIHC_PROFILE` names. Without the variable, the file is the program name
with `.folded` added, in the current directory. Each line is one stack and
its sample count:

```text
exe:Main:$main_cont;exe:Main:$$wnfib_cont_2;exe:Main:$$wnfib 12
```

Give the file to `inferno-flamegraph`, `flamegraph.pl`, or speedscope to get
a flame graph:

```bash
nix shell nixpkgs#inferno -c inferno-flamegraph < main.folded > main.svg
```

- The names go from the bottom frame to the running function. A name is the
  package, the module, and the GRIN function. A continuation has the name of
  its function with `_cont`. Thus a frame shows the code that a return goes
  to, not the caller. An update frame shows the function of its thunk.
- `[collection]` is a sample in a collection. The frames below it show the
  code that started the collection.
- `[runtime]` is a sample before the first function starts.
- `[truncated]` is the bottom of a stack with more than 1024 frames. The
  sample keeps the 1024 frames nearest the running function.
- `AIHC_PROFILE_INTERVAL_US` sets the interval between two samples, in
  microseconds of CPU time. The default is 1000. A short interval gives
  fewer samples than it asks for. Use the counts as relative values.
- On Linux, an `ITIMER_PROF` timer sends `SIGPROF`. On macOS, a sampler
  thread sends `SIGPROF` to each thread of the program that used an interval
  of CPU time. macOS examines an `ITIMER_PROF` timer only when the thread goes
  into the kernel. Thus that timer put too many samples in collections,
  because a collection goes into the kernel at each page fault.

At the entry of each function, the lowering stores the continuation of the
function in the capability field `profile_frame`. Then it stores the code of
the function in `profile_code`. A computation gives its hidden continuation
parameter. A continuation frame gives its parent, its first parameter. Each
entry thus costs a load of the capability and two stores. A test program
with deep recursion and much allocation ran in 1.19 s with the flag and in
1.19 s without it on Apple ARM64.

The entry unit calls `aihc_time_profile_register` with the tables of the
whole-program object: the code and the name of each function, and the info
table of each function object. The registration starts an `ITIMER_PROF`
timer. The `SIGPROF` handler in `aihc_profile.c` finds the capability of the
interrupted OS thread in a thread-local variable. It walks the frames from
`profile_frame` through field zero of each frame. It reads a frame only when
the region table gives a stack chunk for its address and the frame header is
one of the registered info tables. Thus a stale continuation stops the walk
and does not cause a fault. The handler appends the codes to one buffer of
256 MiB address space, and the exit report names them.

While a capability collects, `profile_collecting` is not zero. The handler
then adds `[collection]` and does not read the thunk of an update frame,
because the collector can move it.

Only the POSIX hosts have the profiler. `aihc build` refuses the flag for
`wasm32-wasip3`.

The environment parser lives in `aihc_runtime_options.lir` next to the RTS
option parser. The POSIX host flattens `environ` into one buffer of
`NAME=VALUE` strings, and the C runtime reads the path through
`aihc_rts_stats_path`.

The `wasm32-wasip3` target does not implement the hook yet. The P3 driver
reads no environment, every file write on that host is one asynchronous
stream that the driver pumps. A
`wasm32-wasip3` program therefore never writes the file, even when the
runner passes `--env AIHC_RTS_STATS=<path>` and `--dir` to wasmtime.

STM delay variables use `wasi:clocks/monotonic-clock@0.3.0` on this target.
The runtime submits a timer request for the earliest deadline.
`awaitIO#` preserves the continuation while `wait-until` waits on the host.
The event of the WASI request completes it and resumes the continuation.
The runtime then updates expired delay variables before the transaction starts again.
Timer variables remain garbage collection roots throughout the wait.

Native heap objects have an eight-byte header followed by payload slots.
The header contains an info-table address and two low tag bits.
Info tables have at least four-byte alignment on every target.
In a thunk, bit zero alone marks a claimed or an evaluated thunk.
Both bits mark a claim between its two stores, which one capability never shows.
In an update frame, bit one marks a frame that a pause processed.
Header readers mask both bits before they read the info table.

```text
saturated constructor: [header] [fields...]
thunk:                 [header] [owner] [environment...]
partial application:   [header] [fields...]
indirection:           [header] [target]
blackhole:             [header] [owner / reserved target] [environment...]
```

Field zero of a thunk is its owner word.
The free variables start at field one.
A new thunk does not initialize the owner word, so the pointer bitmap marks it as a word that is not a pointer.
An update writes the result into the owner word, so the free variables stay intact.
A thunk with bit zero names its owner thread, a blackhole queue of its owner, or its value in field zero.
The design of the owner word and the lazy claims is in the blackholing section of
[multi-threaded-rts-design.md](multi-threaded-rts-design.md).

Each info table records the object's identity, populated field count, remaining
logical arity, pointer bitmap, next application-stage table, an optional native
apply entry, and the static reference table of the object's code.
Its `needs_eval` byte is `1` for a thunk and a blackhole, `2` for an indirection, and `0` for a value.
The inline evaluation check of compiled code reads only this byte.
The byte is correct for each header state.
A claimed and an evaluated thunk keep their thunk table, and the collector makes indirections.
Application changes the header to the statically known next table.
Ordinary objects share this static metadata.
A thunk under evaluation retains its original info table and payload.

The Lir lowering gives each closure stage with a remaining arity from one to
four an apply entry, the `backend_entry` of the info table. The entry takes
the values of all remaining argument groups of the stage. Apply sites pass
the context of the thread, the closure, the continuation, and the supplied
values in the `aihc` convention and tail-call that entry. The entry loads captured fields
directly from the closure, takes the supplied values as parameters, and
tail-calls the target function. A stage whose fields and supplied values are
all pointers shares one of the runtime's enter functions, which reaches the
target through the identity field of the table; any other stage gets a
generated stub.

One application supplies from one to four argument groups, as in the GHC
eval/apply model. The apply helper for the shape of the groups compares the
remaining arity of a closure with the group count:

- The arity is equal to the group count: the helper tail-calls the entry
  with all the values.
- The arity `k` is less than the group count: the helper pushes a normal
  continuation frame on the thread stack. The frame holds the parent
  continuation and the other groups. Then it tail-calls the
  entry with the first `k` groups and the frame as the continuation. The
  frame applies the result to the groups it holds.
- The arity is more than the group count, or the function is a partial
  constructor: the shared C slow path builds one
  partial application with all the supplied values.

Primitive operations have no heap-object tag. A partially applied primitive is
lowered to an ordinary closure whose generated entry makes the saturated
primitive call.

Every updateable object reserves at least two words. A thunk has its owner
word for this purpose. Evaluating a thunk executes the entry encoded by its
header. The update writes the returned heap pointer into the owner word and
sets bit zero of the header. There is no separate cell allocation.

Exceptions have no native heap tag or object representation. They are removed
before native runtime lowering. The final physical tag is the collector's
temporary forwarding marker.

The collector does not copy heap indirections or evaluated thunks of the
generations it copies. When it forwards a pointer to such an object, it
follows the chain and stores the final target, so the copied objects hold no
indirection after a collection. The thunk field of an update frame is the
exception: the collector keeps an evaluated thunk there, because a second
update of the thunk must not write into its value. An indirection of an older generation stays until a collection
copies that generation. Static indirections stay in place, because static
objects do not move. The collector forwards their targets instead.

Run `cabal run aihc-dev -- fuzz --filter aihc-gc` to test the collector continuously.
The command uses the same GC property as the test suite.
Each batch has its own C driver.
The command releases the driver when the batch stops.
Run this command from the repository or one of its directories.

The fuzz registry gives each property a positive weight.
A weight of 1 selects ten times as many batches as a weight of 0.1.
Each batch has 10,000 successful cases.
The GC property has weight 0.1. Other properties have weight 1.
Use `aihc-dev fuzz list` to see the weights.

The terminal display shows a spinner for each active property.
Counts include completed tests from all workers and all batches for each property.
The display uses decimal prefixes: K, M, G, T, P, and E.
It updates ten times per second.
Press `q` or Escape to stop. Use `--time-limit 30s` to stop after 30 seconds.

Use `--follow` to test each new commit of the selected branch.
The runner looks at the branch of the current checkout once each minute.
If the branch has an upstream branch with only new commits, the runner uses the upstream commit.
The runner builds a new commit in the background, in a worktree under `$XDG_CACHE_HOME/aihc/fuzz/worktrees`.
The first build in a worktree is a full build.
The build log is in `$XDG_CACHE_HOME/aihc/fuzz`.
When the build is ready, the runner stops and starts the new `aihc-dev` with the same arguments.
The known bugs stay in the bug store.
If a commit does not build, the runner continues with the old executable.

The collector has a fuzz test in `Test.Native.GcFuzz`. The test generates
random scripts that build heaps and stacks through the runtime interface,
change them, and force collections. A C driver runs each script against the
runtime. The driver plays the part of compiled code for several threads: it
pushes and enters frames as the continue helpers do, and it gives each
resumption of the scheduler to the thread it names. After every collection
the driver reports the objects, the frames of every live stack, the threads,
the run queue, the MVar waiters, the blackhole queues, the roots, and the
static objects. A model of the same script checks the report.

The model knows no collector policy. After every collection, each object
and frame the model reaches from the roots must survive with the same
content, the ages of the survivors must grow, and the scheduler state must
match. A full collection must keep the reachable set and nothing else. At
the end of a gen2 cycle, the gen2 objects that were dead at its snapshot must
be gone. The scripts cover constructors, closures, thunks, partial
applications, arrays, indirection chains, blackholes, static objects,
reference tables, MVars, threads, every frame kind, pops across chunks,
exceptions, and every root source the collector visits. The generator never
names an object the model says is dead, because no program can resurrect
garbage. The driver process stays alive across cases, so the test can
compile the driver with sanitizers when the C compiler supports them.

The runtime has a verifier for these tests. When the C preprocessor symbol
`AIHC_GC_VERIFY` is defined, every collection ends with a walk of the
objects the roots reach. The walk checks that each reachable object is in
an occupied slot of a live block, that no reachable header is a forwarding
header, that each reachable frame is live in a registered chunk, that the
remembered set holds valid objects, that the
pending frames of a cycle are live, and that the chunks of a stack grow
older from its top. A violation stops the program at the collection that
caused it. The fuzz driver and the GRIN `gc-stress` fixtures run with the
verifier. A release runtime does not pay for the walk.

The cooperative scheduler keeps pending IO requests in managed pinned objects. Suspended threads retain
ordinary action closures or pointers to continuation frames on their stacks. The scheduler hands a selected thread
back to generated code as a resume record, which the Lir resume helper
dispatches with a tail call. All retained closure values and pending-request
continuations are precise collector roots.
Generated code exposes live values to the collector through explicit root slots.

STM transactions, write logs, and timers use the managed heap.
Each record has a header and a distinct runtime object kind.
The collector obtains each size from its C structure.
It traces pointer fields through that structure on both 32-bit and 64-bit targets.
The current transaction of each live thread retains its parent transactions and write logs.
The machine timer list retains timer variables and final values until expiry.
Commit, abort, and expiry remove references without direct memory release.
These records count toward managed allocation statistics and the `-M` limit.

The GRIN primitive description gives fixed allocation bounds for ordinary calls and CPS calls.
The GC stage inserts `ensure-heap` before these calls.
The reservation protects call arguments and values needed after the call.
The existing GC transformation gives relocated roots fresh names.
Lir translates the explicit reservation and the call.

`stmBegin#` reserves three heap slots, and `writeTVar#` reserves four.
`newDelayTVar#` reserves eight slots for its TVar and optional timer.
`newPromptTag#` reserves one slot.
`makeStableName#` reserves four slots.
`newMVar#` and `fork#` each reserve nine slots.
`readMVar#`, `takeMVar#`, and `putMVar#` each reserve five slots for one optional waiter.
CPS call reservations protect the continuation and all pointer arguments.
Each slot has eight bytes on every target.
C size assertions check that runtime records fit these bounds.
Reservations can exceed actual allocation, which statistics measure separately.

These primitives consume reserved heap and must not collect.
Their callees must preserve this contract.

Managed heap allocation does not clear memory in release builds.
Each object initializer must write every field before collection can trace the object.
When the C preprocessor symbol `DEBUG` is defined, `aihc_gc_allocate` clears its allocation for diagnostics.
Runtime records explicitly initialize null links, empty queues, and inactive state in both builds.
`aihc_gc_allocate` checks the available space and cannot collect.
The delay-variable helper initializes its TVar and timer before any further collection.

GRIN snapshot fixtures request GC stress checks with `gc-stress: true`.
The test harness changes generated Lir to select the collector path at each reservation.
It identifies collector blocks by their calls to `aihc_heap_collect`.
It replaces those calls with `aihc_gc_collect` to force a collection instead of a block refill.
A reservation whose slow path enters the function again through a replay helper takes that path every second time.
The helper sets a flag before it enters the function again, and the reservation clears the flag.
This transformation exists only in test code.
Successful stress fixtures must report at least one collection.
They can specify heap limits through `rts-arguments`.

Thread records are in the global heap, including the initial thread.
`aihc_gc_allocate_global` puts each record in a gen2 slot when the thread starts.
A record never moves, so a `ThreadId#` stays valid when a thread moves to another capability.
The allocation consumes the `fork#` reservation, and the record bytes charge the nursery budget as a pinned block does.
Machine initialization reserves the initial record after GC initialization.
The record has the `AIHC_OBJECT_THREAD` kind.
The collector obtains its size and pointer fields from the C structure.

The resume function, continuation, pointer value, and transaction are roots of the capability.
They are not fields of the global object.
Each collection and the snapshot of each gen2 cycle visit them for every thread that has a stack.
Thus a store into these fields needs no write barrier, and the gen2 marker does not follow them.
The object scan follows only the run queue link, which names another thread record.

The machine retains the current thread and both ends of the run queue.
MVar waiters, blackhole waiters, and pending IO requests retain their threads.
A gen2 cycle decides whether a thread record is live.
An unreachable thread becomes reclaimable after it leaves these runtime queues and other live references.
Its record and its stack stay until the end of the next gen2 cycle or the next full collection.
Thread records count toward managed allocation statistics and the `-M` limit.
Snapshot fixtures reset managed allocation statistics after initialization, so their totals exclude the initial thread.
Process statistics include the initial thread.

Each thread record has a unique number.
The machine assigns number one to the initial thread and increments the number for each new thread.
The number remains at offset eight on every target.
`aihcThreadIdNumber#` reads it with one load.
`myThreadId#` reads the current thread from the machine.

Blackhole waiters and blackhole queues use the managed heap.
Each waiter has a header, thread, continuation, and queue link.
A queue has a header, its owner thread, its thunk, both ends of its waiter list, and the link of the queue list of its owner.
A claimed thunk with waiters names its queue in field zero.
The resumption fields of a thread record include the queues of the thunks that the thread claimed, and they are roots.
The collector traces these fields through the C layout.
Waiter and queue records count toward allocation statistics and the `-M` limit.
Ordinary thunk evaluation allocates no queue.

`aihc_lir_eval` follows indirections and evaluated thunks before it selects a branch.
A ready value requires no reservation or update frame.
The thunk branch pushes a three-slot update frame on the thread stack.
If the push grows the stack and the charge of the new chunk uses the nursery, the branch collects.
It protects the value and the continuation as roots, and then it evaluates the value again.
The new chunk stays empty above the stack pointer for the next push.
The blackhole branch takes a claimed thunk: field zero names a thread record or a queue.
It reserves ten slots, which cover a waiter and a queue on every target.
It protects the resolved value and the continuation as roots across collection.
The thunk branch stores the parent continuation and resolved thunk in the update frame.
It writes nothing into the thunk and transfers to the thunk entry.

A pause claims the thunks of the update frames of a thread.
The thread pauses when it yields, when it blocks on an MVar or a thunk, and when an IO request suspends it.
A collection is a pause too, and the collector claims the thunk of each update frame that it scans.
The pause walks the frames from the top, claims each thunk in the state Thunk, and marks each update frame.
It stops at the first marked frame.
A claim stores the thread into field zero and sets bit zero.
A thunk that the same thread claimed stays as it is: the thread entered the thunk twice.

`aihc_block_on_blackhole` reads the owner from field zero.
When the owner is the current thread, the thread re-entered its thunk, and the runtime stops the program.
Otherwise, it appends a waiter to the queue of the thunk, and the first waiter makes the queue.
The runtime consumes the reserved space without collection.
The scheduler suspends that thread until an update or exception wakes it.

The shared update continuation writes the result into field zero and sets bit zero.
A young thunk that no pause claimed gets the two stores in Lir.
The C update wakes the waiters of a queue of the current thread and removes the queue from its list.
The continuation then evaluates the result with the parent continuation.
Exception unwinding gives a thunk that the thread claimed back the state Thunk and raises the exception in each waiter.
A thunk that no pause claimed needs nothing, because the entry wrote nothing into it.

`aihc_lir_eval_single_entry` enters a thunk with no update frame, evaluation bit, or reservation.
The compiler uses it only for a thunk that no other evaluation can reach.
The thunk is not updated, and the collector can reclaim it as soon as its entry has loaded its fields.
A value with bit zero or an indirection goes to `aihc_lir_eval`.

The inline check that compiled code does before an evaluation tests `needs_eval` against zero first.
Only an object that is not a value then tests for an indirection and for a thunk with bit zero alone.
The check follows an indirection to its target.
It also follows an evaluated thunk to its value: field zero names an object that is not a thread record and not a queue.
Both stay until the next collection that copies their generation.

The collector masks the header tags and traces the original thunk layout and static reference table.
For a thunk with bit zero, it traces field zero as well.
The code and the free variables of an evaluated thunk are dead, so the collector does not trace them.
An update continuation retains its thunk while evaluation is in progress.
The owner thread retains its queues, and each queue retains its thunk and its waiters.
This rule applies to both static and heap thunks.
The scheduler does not scan the table to find completed thunks.

`MVar#` uses a managed empty/full cell with separate FIFO queues for
blocked readers, takers, and putters.
A put into an empty cell wakes all blocked readers with the same value.
It gives the value to the first taker, or leaves the cell full.
A take from a full cell returns its old value.
If a putter is blocked, the take installs that putter's value and wakes that putter.

MVars and waiters have distinct object kinds and C layout visitors.
A live MVar retains its value and all queue heads and tails.
Each waiter retains its continuation, put value, queue link, and thread record.
The machine has no list that retains every MVar.
The collector can reclaim unreachable MVars, waiters, and their values, including cycles.
A wake removes the waiter from its queue without direct memory release.
MVars and waiters count toward managed allocation statistics and the `-M` limit.

## Static objects

Static objects occupy an object-file data section and never move.
An evaluated CAF contains an indirection into the managed heap.
The collector retains its target only while the CAF remains reachable.

Each info table contains a static reference table (SRT).
The SRT names static objects that the code can reach directly.
It also names the tables of called functions and the entry functions of objects that the code can create.
These entries retain the required CAF values before those objects exist.
This includes thunks, partial applications, and continuation closures.
The collector follows these tables from active code and live objects.
It also follows pointer fields in live objects.

A compiled function passes its table to the collector at each of its safepoints.
After CPS conversion, each call is a tail call.
The active function has no heap object to carry its table, and nothing else records it.
A collection inside a runtime helper passes no table: the helper is reached by a tail call, or by a call whose only continuation is a transfer to heap objects, so the calling function's static references are dead.
Suspended code uses a continuation closure with a table in its info table.

No section and no table lists the static objects. The collector finds them by
address: a pointer outside every region of the managed heap names an object
that never moves. A full collection records the addresses it marks in a hash
set and scans each object once through its info table, so an evaluated CAF
gets its target forwarded like any heap field. A smaller collection scans
only the static objects in the remembered set. A nullary constructor has no
fields, so marking it does nothing.

Every object that compiled code can store in a pointer field carries an info
table. Byte arrays have the `AIHC_OBJECT_BYTE_ARRAY` kind.

Stable names use four managed slots on every target: header, weak referent, hash, and weak lookup link.
Their info tables have the `AIHC_OBJECT_STABLE_NAME` kind.
The GRIN GC stage reserves their memory and protects the referent and caller values before `makeStableName#`.
The Lir runtime unit consumes this reservation without collection.

The machine lookup list does not retain names.
A name does not retain its referent or the next name in the list.
After strong tracing, the collector rebuilds the lookup list from live names with live referents.
It updates moved referents and follows heap indirections to live targets.
It clears dead referents and removes dead entries from the lookup list.
This phase does not allocate memory or start further tracing.

A live name keeps its hash even if its referent dies.
Pointer equality between live names remains valid because collection relocates every strong reference to each name.
Dead names and referents become reclaimable, and name records count toward allocation statistics and heap limits.
The lifetime rule follows [System.Mem.StableName](https://downloads.haskell.org/~ghc/latest/docs/libraries/base-4.22.0.0-66f8/System-Mem-StableName.html).

## Byte arrays and pinned storage

A byte array has six eight-byte descriptor slots followed by its payload.
The slots contain the header, current size, contents address, pinned flag, alignment, and allocation size.
An empty array still has one payload byte.
The allocation size remains unchanged after shrink, so the collector can traverse the complete object.

`newByteArray#` uses the movable heap.
After relocation, the collector repairs the contents address to point after the descriptor.
Pinned allocations contain their descriptor and payload in one separate block.
The payload preserves the requested power-of-two alignment.
Two metadata slots precede the object and contain the list link and complete charge.
The metadata address is the allocation base.

The GRIN GC stage calls a size helper before each allocation or resize.
The helper checks the size and alignment and computes the complete reservation in slots.
The existing reservation transformation protects the source array and other live pointers.
The Lir allocation functions consume this reservation without collection.
Resize copies into a new array and preserves the pinned flag and alignment.

`aihc_gc_allocate_pinned` consumes the same budget as movable allocation.
It reduces the available space by the complete block charge and updates allocation statistics.
The machine records physical space capacity separately from the available space limit.
The pinned allocation list does not retain its objects.
Strong tracing marks pinned objects through ordinary roots and object fields.
After weak stable-name processing, the collector releases unmarked pinned blocks.
Their charges then become available to later reservations.

An `Addr#` does not retain an array.
A pinned address remains stable only while its array remains live.
Use `keepAlive#` around an action that uses the address, including the complete asynchronous IO operation.
A final `touch#` also keeps its operand live through earlier GC reservations on that control path.
These lifetime requirements follow the [GHC byte-array contract](https://downloads.haskell.org/~ghc/latest/docs/libraries/ghc-internal-9.1401.0-555c/src/GHC.Internal.Prim.html).

GRIN preserves `keepAlive#` as a lifetime scope through simplification.
CPS conversion gives each pointer owner a three-slot continuation frame.
GRIN GC reserves these frames and relocates their arguments.
The collector traces their parent and owner fields.
Continuation dispatch passes through these frames without a result-layout conversion.
This supports abstract result representations and results with multiple registers.
Exception unwinding treats them as ordinary frames.

Foreign operand conversion evaluates other operands before it obtains byte-array payload addresses.
Thus, a collection during operand evaluation cannot invalidate an extracted movable address.
The path and argument-buffer helpers retain their owners while raw addresses are in use.

## Request allocation and roots

`stmWaitRequest#`, `submitIORead#`, and `submitIOWrite#` reserve sixteen slots in GRIN GC.
`submitIOOpen#` reserves twenty-one slots, which include five slots for its result handle.
`adoptIOHandle#` reserves five slots for a handle.
The runtime consumes these reservations without collection.
The request bound includes two pinned metadata slots.
Requests remain pinned because host calls retain C request addresses across host safepoints.
Handles can move and use `IOHandle#` references.
The GC reclaims handle storage.
Programs must still close open host resources.
Standard handles are static objects with the same header.
Open errors reside in the result handle, so a result never uses an integer as a traced pointer.

`IORequest#` references retain requests before await and after completion.
Only pending operations have scheduler roots.
A request traces its handle, buffer owner, thread, continuation, and next pending request.
Completion removes the scheduler root.
An unreachable completed request can then be reclaimed, even when its result was not consumed.
Result consumption checks the completed state and prevents a second consumption.
The pinned allocation list does not retain objects.

## IO manager

The runtime ABI separates operation submission, scheduler suspension, and
result consumption:

1. A submission primitive consumes reserved memory for an opaque request in the `submitted` state.
2. `awaitIO#` asks the configured backend to make progress. Immediate
   completions continue directly; otherwise the request becomes `pending` and
   retains the current green thread and continuation.
3. Backend polling changes a ready request to `completed` and enqueues its
   thread. A primitive takes the result and changes the request to `consumed`.

Backend workers or readiness mechanisms produce only native completion data;
Haskell continuations are always reconstructed and enqueued on the scheduler
thread. This prevents moving-heap pointers from escaping to an asynchronous
backend. Managed `IORequest#` references supply Haskell heap roots.

IO operations target opaque runtime-owned handles rather than OS descriptor
numbers. Standard input and output are the first preopened handles, while each
backend owns their platform representation. The POSIX backend stores a file
descriptor in each handle, sets it nonblocking, and uses `poll` when buffer
reads or writes report that they would block. Windows can instead store `HANDLE` or
`SOCKET` resources without exposing either representation to generated code.

Reads and writes operate on an offset and length within a pinned `MutableByteArray#` payload.
The collector owns and can reclaim these arrays.
The caller must retain the array through the submission reservation.
A final `touch#` after submission can establish this lifetime.
Submission finds the pinned owner from the payload address and checks the requested slice against its bounds.
The request then retains that owner until result consumption.
Interior payload addresses also retain their owner.
A request rejects a buffer address in the movable heap.
External buffers remain the caller's responsibility.
Use `keepAlive#` around the complete action when a separate Haskell owner controls external storage.
Handles use the same collector and heap budget.
Callers must not access the submitted slice while the request is pending.

`copyAddrToByteArray#` copies an explicit byte count into a checked destination slice.
It does not search for a terminating zero.
The source address must remain valid until the synchronous copy returns.

A non-negative request result is the number of transferred bytes. A non-empty
read returns zero at end-of-file. Either operation can return fewer bytes than
requested, so the future `Handle` layer must resubmit the remaining slice when
it requires a complete transfer. Errors use `-(errno + 1)` in the POSIX proof
of concept. `GHC.IO.Runtime` owns the runtime bindings and generic
suspension (`awaitIO`). `GHC.IO.StdHandles`
exposes buffer allocation, address and indexed byte access, handle operations,
and the standard handles. Text encoding, locking, transfer loops, and full
`Handle` semantics remain above this boundary.
