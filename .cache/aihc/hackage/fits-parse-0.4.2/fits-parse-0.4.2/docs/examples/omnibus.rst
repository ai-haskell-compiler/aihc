Using the *omnibus* Example Program
===================================

The *omnibus* example program is a proving ground for the library by attempting
straightforward use case examples and tying them all into one program.

The example *omnibus* program can be run as follows::

    fits-parse$ stack exec -- omnibus

It has a few options out might want to be aware of::

    fits-parse$ stack exec -- omnibus --help
    fits-parse - a FITS swiss army knife

    Usage: omnibus (FILE | --stdin) ([-o|--outfile ARG] | [--stdout])
      Parse, generate, and render FITS files

    Available options:
      FILE                     Input FITS file to parse
      --stdin                  Input FITS from stdin
      -o,--outfile ARG         Output filename to store
      --stdout                 Output picture to stdout
      -h,--help                Show this help text

Running *omnibus* on the example fits file *Spiral_2_30_0_300_10_0_NoGrad.fits*
gives an output like the following::

    fits-parse$ stack exec omnibus example_fits_files/Spiral_2_30_0_300_10_0_NoGrad.fits
    29/Mar/2019:21:03:27 -0500: [DEBUG] input file size 1545444 bytes
    29/Mar/2019:21:03:27 -0500: [DEBUG] found 1 hdu record(s)
    29/Mar/2019:21:03:27 -0500: [DEBUG] Bit Format 32 bit IEEE single precision float
    29/Mar/2019:21:03:27 -0500: [DEBUG] data block size 1542564 bytes
    29/Mar/2019:21:03:27 -0500: [DEBUG] 2 Axes
    29/Mar/2019:21:03:27 -0500: [DEBUG] Axis: 1 count: 621
    29/Mar/2019:21:03:27 -0500: [DEBUG] Axis: 2 count: 621
    29/Mar/2019:21:03:27 -0500: [DEBUG] Total Pix Count: 385641
    29/Mar/2019:21:03:27 -0500: [DEBUG] Unwrapped Int Count: 0
    29/Mar/2019:21:03:27 -0500: [DEBUG] Unwrapped Double Count: 385641
    29/Mar/2019:21:03:27 -0500: [DEBUG] Vector Int Count: 0
    29/Mar/2019:21:03:27 -0500: [DEBUG] Vector Double Count: 385641
    29/Mar/2019:21:03:27 -0500: [DEBUG] Mean:     3.161230263379672e-2
    29/Mar/2019:21:03:27 -0500: [DEBUG] Range:    1.0
    29/Mar/2019:21:03:27 -0500: [DEBUG] Variance: 3.061296495598597e-2
    29/Mar/2019:21:03:27 -0500: [DEBUG] Std deviation: 0.17496583763186624
    29/Mar/2019:21:03:27 -0500: [DEBUG] Std error of the mean: 2.817485308081582e-4
