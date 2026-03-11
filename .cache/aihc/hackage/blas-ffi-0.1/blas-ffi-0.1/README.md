## cabs1

The reference BLAS implementation provides the functions SCABS1 and DCABS1.
However, it is missing in some optimized alternative implementations
like OpenBLAS and ATLAS.
This caused linker errors in the past.
Since the function is pretty trivial, I removed its binding.
