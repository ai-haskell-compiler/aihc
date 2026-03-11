The overall idea of the package is to make programming mistakes
let low-level programs fail reproducibly.

What the routines do:

 *  After allocation fill the memory with the hex string 0xDEADF00D.
    This allows to check whether the caller
    properly initialises allocated buffers.

 *  Allocate some memory before and after the actual buffer
    and fill it with 0xABADCAFE.
    On deallocation it is checked that this pattern is still intact.
    If not, abort with an error.
    This allows to check for range violations.

 *  Before deallocation fill the memory with 0xDEADBEEF.
    This helps to detect when the program reads memory after its deallocation.

 *  The `create` routine additionally makes a copy of the initialized buffer.
    The finalizer compares the contents of the buffer and its copy.
    This way it can detect if an immutable array was altered after its creation.


Range violations might alternatively be detected by range checking techniques.
Allocation problems might be solved using Regions.
The provided functions might overlook range violations
but they help detecting bugs
when you have not full control over the code that processes memory content,
e.g. when calling external routines via FFI.
