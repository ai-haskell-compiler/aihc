# Generational incremental garbage collector

This document gives the design of the generational collector in
`core-libs/aihc-rts/native/aihc_gc.c`, which replaced the semispace
collector.

## Goals

- High allocation throughput. The nursery allocation path stays a bump and a
  compare in registers.
- Bounded pauses. Each stop-the-world step has a bound that does not depend
  on the size of the live data.
- One design for all backends. The collector uses no threads and no atomics.
  The AMD64, ARM64, LLVM, and wasm32 backends run the same code.
- Heap memory close to the live data. The semispace collector needs twice
  the live data.

The collector does not change the GRIN safepoint model. Compiled code reaches
the collector only on the allocation slow path, with precise roots. A loop
that does not allocate does not reach a safepoint. GHC has the same limit.

## Heap layout

The heap is a set of runs of regions of `AIHC_REGION_BYTES`, 64 KiB. The
runtime takes each run from the host as one mapping of exactly that size and
reserves no address space in advance. A large reservation is refused on hosts
with strict overcommit, under address space limits, and in some hardened
container policies, so the design does not depend on one. A region is one
WebAssembly page, so the wasm32 host grows its memory by whole regions, and a
large object wastes at most half a region.

A two-level region table gives the kind of the region that holds an
address. The top level is indexed by the address bits above the leaf, and a
leaf covers 4 GiB with one entry for each region. A lookup is two dependent
loads. The collector pays it only for a pointer outside the spaces it
copies, and the write barrier never pays it, because its test is a range
compare against the nursery. The kinds are:

| Kind | Content | Moves |
| --- | --- | --- |
| `FREE` | No objects | - |
| `NURSERY` | Bump allocated young objects | Yes |
| `GEN1` | Bump allocated objects that survived one collection | Yes |
| `GEN2` | Segments of one size class with a mark bitmap | No |
| `LARGE` | One object of at least `AIHC_LARGE_OBJECT_BYTES`, 32 KiB | No |
| `PINNED` | Pinned byte arrays and host buffers | No |
| `STACK` | Sixteen 4 KiB stack chunks | No |

An address outside every mapping has the kind `OUTSIDE` and names a static
object. The header keeps no generation information. Info tables stay 48
bytes. A released run waits in a free list for reuse, and a mapping whose
regions are all free goes back to the host.

A `LARGE` object takes a whole number of regions. A `GEN2` segment is one
block of four regions that holds slots of one size class, with a mark
bitmap of one bit for each slot. Objects above the largest size class go to
`LARGE`. Arrays and
byte arrays already allocate through the runtime, so the runtime selects the
space. Compiled code stores only fixed-size nodes into reserved nursery space.

## Generations and policy

| Generation | Collected by | Survivors go to | Size rule |
| --- | --- | --- | --- |
| Nursery | Every minor collection | Gen1 | Fixed, option `-A` |
| Gen1 | A minor collection when gen1 is above its maximum | Gen2 | Maximum, option `-B` |
| Gen2 | Incremental mark and sweep | Stay | Growth factor `-F`, limit `-M` |

A minor collection copies live nursery objects into gen1. When gen1 is above
its maximum, the same collection also copies live gen1 objects into gen2. Thus
an object survives one collection before it reaches the non-moving heap. This
one aging step limits floating garbage in gen2, which only a full cycle frees.

A gen2 cycle starts at a minor collection that empties gen1. It starts when
gen2 is above `-F` times the live gen2 bytes after the last cycle.

If gen2 reaches the `-M` limit while a cycle is active, the collector finishes
the cycle in one stop-the-world step. This is the only unbounded pause, and it
is the degradation mode. A heap that is still full after it fails as today.

## Minor collection

A minor collection is a Cheney copy. Its roots are:

- The spilled roots of the safepoint and the machine roots that
  `aihc_visit_roots` names today.
- The young stack chunks. See the next section.
- The remembered set. See the write barrier section.
- The dirty cards of large pointer arrays.

The copy applies promotion by reference. When the collector scans an object
or a frame of generation g, it evacuates each referent into a generation that
is not below g. Thus an old object points only at old objects after one scan,
and its remembered-set entry is dropped. The entry is not kept across
collections.

Heap indirections are followed and not copied, as today. An indirection in
gen2 stays until its sweep, because gen2 does not move objects.

Static objects are old. A minor collection does not walk static reference
tables and does not build the static address set. An updated CAF enters the
remembered set through the update barrier. Only a gen2 cycle traces static
objects through their tables.

Objects copied into gen2 during an active cycle are allocated marked.

## Stack chunks as generational objects

Frames are write-once. The runtime writes the fields of a frame directly after
its push and never again. A restored captured continuation is a new push. This
property lets the collector treat a stack chunk as an object with an age.

Each 4 KiB stack chunk header gets one generation byte. The rules are:

- Frames pushed since the last collection are young. The top chunk of the
  running thread is always young. A thread switch marks the top chunk of the
  new running thread young in the scheduler, which already runs in C.
- A collection of the generations up to g walks each live stack from its top
  frame. It scans each chunk of generation g or below, evacuates the referents
  of its frames into generation g plus one, and labels the chunk g plus one.
  The walk stops at the first older chunk.
- A pop into a lower chunk labels that chunk young. New frames then overwrite
  the space above the entered frame.

Young chunks are always a contiguous top segment of a stack. A lower chunk
becomes young only through a pop into it, and that pop releases every chunk
above it. Thus the walk rule is complete.

The pop detection is the one new hot-path cost. The continue helpers compute
the stack limit of the entered frame in `Aihc.Lir.Lower`. One compare against
the previous limit and a cold runtime call on change covers it. An underflow
frame at each chunk boundary has no hot-path cost but changes the frame chain
that continuation capture walks. The compare comes first. A measurement
decides on the underflow frame.

The worst case stack cost of a minor collection is one chunk for each live
thread plus the young chunks. Only young frames need deduplication, so the
address set of the semispace collector shrinks to the young frames.

## Write barrier

Constructors are initialized in reserved nursery space, so allocation has no
barrier. Pointer stores into existing objects happen at these sites:

| Site | Where |
| --- | --- |
| `writeArray#`, `writeSmallArray#`, `writeMutVar#`, and the CAS primitives | Inline stores in `Aihc.Lir.Lower` |
| Thunk update | `aihc_update_blackhole_inline` in `aihc_helpers.lir` |
| MVar, TVar, transaction log, blackhole waiter, IO request, and thread fields | C functions in `aihc_runtime.c` |
| Global and CAF updates | `aihc_update` and the globals array |

The fast path is one test: the object is in the nursery. The nursery is one
contiguous range, so the test is a subtraction and an unsigned compare against
the nursery base and size. Nursery objects are the common case for mutable
stores and for thunk updates. Every other object takes a cold path. The cold
path is a runtime call. The call takes the object and the field index, or the
whole object for an update.

The cold path does two independent things:

- **Remembered set.** The object enters the remembered set. The set is a
  list without a membership bit: a hot object enters it at every store, and
  the list is compacted when it is full and before a collection scans it.
  Header bit 1 is taken by the forwarding pattern of a copied object. A
  large pointer array marks the card of the field as well, and a collection
  scans only its dirty cards.
- **Deletion barrier.** If a gen2 cycle is active and the object is in gen2
  or static, the old value of the field goes to the mark buffer. A thunk
  update pushes every pointer field of the thunk, because the update deletes
  all of them at once.

Deletions in young objects need no shading. The snapshot happens when the
young generations are empty, so every young object is newer than the snapshot
and cannot hold the last snapshot path to an object.

Both buffers are plain arrays. A full buffer triggers a collection or a mark
slice, which bounds their memory.

## Incremental marking of gen2

Gen2 is a snapshot-at-the-beginning mark and sweep collector. Marking runs in
slices on the mutator thread. No other thread exists.

**Snapshot.** The cycle starts at the end of a minor collection that emptied
gen1. At that instant the young generations are empty, and every live object
is in gen2, in `LARGE`, in `PINNED`, in a stack chunk, or static. The
collector pushes the machine roots and the globals to the mark stack. It scans
the top chunk of every live stack at once, which is bounded. It defers lower
chunks: the marker scans them in later slices, and a pop into a deferred chunk
scans it in the pop runtime call before the mutator can overwrite a frame.
Static objects are traced through their static reference tables as ordinary
mark work.

**Slices.** A slice runs at the end of each minor collection and when the mark
buffer is full. It pops the mark stack and the mark buffer, marks each object
in its segment bitmap or its region table entry, and pushes its pointer
fields. A slice stops at a byte budget. Objects promoted into gen2 during the
cycle are allocated marked and are not pushed.

**Pacing.** Each slice does mark work of `-k` times the bytes promoted into
gen2 since the last slice, with a floor and the slice cap. The default factor
finishes a cycle before gen2 grows by the `-F` factor again.

**Termination.** The cycle ends at a slice that finds the mark stack and the
mark buffer empty. No final stop-the-world phase exists. Roots that change
after the snapshot reference objects that are either newer than the snapshot
or reachable at the snapshot, and the barrier covers every deletion.

**Weak references.** At the end of the cycle, stable names with an unmarked
referent, unmarked pinned blocks, and blackhole table entries with an unmarked
key are dropped. The lists are short, and the work is one bounded slice.

**Sweep.** The mark bitmap of a segment becomes its free map. A segment is
swept when the promotion allocator takes it, and each collection sweeps a
bounded slice of the rest, so sweep work is paced by promotion and by
collections. Unmarked `LARGE` regions return to `FREE` at the end of the
cycle.

## Pause bound

A minor collection costs the sum of these terms:

| Term | Bound |
| --- | --- |
| Copy of live young objects | Nursery size plus gen1 maximum |
| Root scan | Machine roots plus remembered set plus dirty cards |
| Stack scan | One chunk for each live thread plus young chunks |
| Mark slice | The slice cap |

No term depends on the live data in gen2, on the depth of a stack, or on the
size of a large object. The degradation mode is the one exception.

## Options and statistics

| Option | Meaning | Default |
| --- | --- | --- |
| `-A<size>` | Nursery size | Decided by measurement, 1 MiB or 4 MiB |
| `-B<size>` | Gen1 maximum | 16 MiB |
| `-F<factor>` | Gen2 growth factor between cycles | 2 |
| `-M<size>` | Heap limit, as today | Off |

The statistics file has `gc_max_pause_ns`, the longest collection, and
`live_bytes`, the occupied space after the last collection. It gains the
number of minor collections, the number of gen1 promotions, the number of gen2
cycles, and the bytes of each generation after the last collection.

## Changes outside the runtime

- `Aihc.Lir.Lower` emits the barrier fast path at the array and MutVar store
  sites and the limit compare in the continue helpers. All backends consume
  Lir, so no backend changes.
- `aihc_helpers.lir` emits the barrier fast path in the inline update.
- `aihc_constants.lir` and `Aihc.Lir.Lower` share the new region constants as
  they share the stack chunk constants today.
- The GC fuzz driver in `bin/aihc/compiler/native/test/gc-fuzz` gains
  commands for promotion, barrier stores, and slices, and its model gains
  generations.

## Build order

1. **Measurement.** Add the longest pause and the live bytes to the
   statistics file. Add four GC workloads under `examples/gc-*`: allocation
   heavy, large live set, deep stack, and mutable arrays.
2. **Regions.** Add the reservation, the region table, `LARGE`, `PINNED`, and
   `STACK` regions. Keep semispace semantics. The fuzz model stays valid.
   The semispace collector keeps a large object on its pinned list, so one
   sweep covers both. Pinned byte arrays below the large bound stay C
   allocations until the segment allocator of step 4 exists.
3. **Generations.** Add the nursery, gen1, the write barrier, the
   remembered set, the CAF list, and the stack chunk generations. Gen2 is a
   copying generation collected stop-the-world in this step, so the fuzz
   model validates the barrier before the segment allocator exists. Cards
   for large pointer arrays wait for step 4. The implemented details are in
   the generations section of `docs/native-runtime-objects.md`.
4. **Non-moving gen2.** Add segments, bitmaps, stop-the-world marking, and
   lazy sweep. Cards for large pointer arrays belong to this step. The
   implemented details are in the gen2 segments section of
   `docs/native-runtime-objects.md`.
5. **Incremental marking.** Add the deletion barrier, the snapshot, slices,
   pacing, and the deferred chunk scan.
6. **Policy.** Set the defaults from the measurements.

Each step leaves the compiler shippable and is its own PR series.

## Open points

- The nursery default. The measurements of step 1 decide between 1 MiB and
  4 MiB.
- The pop detection. The compare in the continue helpers comes first. The
  underflow frame replaces it only if the compare is measurable.
- The size classes of gen2 and the large object threshold. The object size
  distribution of the compile workloads in the benchmarks suite decides them.
