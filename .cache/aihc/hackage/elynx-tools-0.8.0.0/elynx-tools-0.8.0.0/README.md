

# The ELynx Suite

Version: 0.8.0.0.
Reproducible evolution made easy.

<p align="center"><img src="https://travis-ci.org/dschrempf/elynx.svg?branch=master"/></p>

A Haskell library and tool set for computational biology. The goal of ELynx is
reproducible research. Evolutionary sequences and phylogenetic trees can be
read, viewed, modified and simulated. The command line with all arguments is
logged consistently, and automatically. Data integrity is verified using SHA256
sums so that validation of past analyses is possible without the need to
recompute the result.

The Elynx Suite consists of library packages and executables providing a range
of sub commands.

The library packages are:

-   **[elynx-nexus](https://hackage.haskell.org/package/elynx-nexus):** Nexus file support.
-   **[elynx-markov](https://hackage.haskell.org/package/elynx-markov):** Simulate multi sequence alignments along phylogenetic trees.
-   **[elynx-seq](https://hackage.haskell.org/package/elynx-seq):** Handle evolutionary sequences and multi sequence alignments.
-   **[elynx-tools](https://hackage.haskell.org/package/elynx-tools):** Tools for the provided executables.
-   **[elynx-tree](https://hackage.haskell.org/package/elynx-tree):** Handle phylogenetic trees.

The executables are:

-   **[slynx](https://hackage.haskell.org/package/slynx):** Analyze, modify, and simulate evolutionary sequences.
-   **[tlynx](https://hackage.haskell.org/package/tlynx):** Analyze, modify, and simulate phylogenetic trees.
-   **[elynx](https://hackage.haskell.org/package/elynx):** Validate and redo past analyses.

Documentation is available on [Hackage](https://hackage.haskell.org/) (use direct links above).

**ELynx is actively developed. We happily receive comments, ideas, feature
requests, and pull requests!**


# Installation

ELynx is written in [Haskell](https://www.haskell.org/) and can be installed with [cabal-install](https://cabal.readthedocs.io/en/3.4/cabal-commands.html) or [Stack](https://docs.haskellstack.org/en/stable/README/).

1.  Install Stack with your package manager, or directly from the web
    page.
    
        curl -sSL https://get.haskellstack.org/ | sh

2.  Clone the ELynx repository.
    
        git clone https://github.com/dschrempf/elynx

3.  Navigate to the newly created `elynx` folder and build the binaries.
    This will take a while.
    
        stack build

4.  Run a binary from within the project directory. For example,
    
        stack exec tlynx -- --help

5.  If needed, install the binaries.
    
        stack install
    
    The binaries are installed into `~/.local/bin/` which has to be added to the
    [PATH](https://en.wikipedia.org/wiki/PATH_(variable)) environment variable. Then, they can be used directly.


# Get help

    cabal run slynx -- --help
    # OR: stack exec slynx -- --help
    # OR: slynx --help

    ELynx Suite version 0.8.0.0.
    Developed by Dominik Schrempf.
    Compiled on October 27, 2024, at 07:14 am, UTC.
    
    Usage: slynx [-v|--verbosity VALUE] [-o|--output-file-basename NAME]
                 [-f|--force] [--no-elynx-file] COMMAND
    
      Analyze, and simulate multi sequence alignments.
    
    Available options:
      -h,--help                Show this help text
      -V,--version             Show version
      -v,--verbosity VALUE     Be verbose; one of: Quiet Warn Info Debug
                               (default: Info)
      -o,--output-file-basename NAME
                               Specify base name of output file
      -f,--force               Ignore previous analysis and overwrite existing
                               output files.
      --no-elynx-file          Do not write data required to reproduce an analysis.
    
    Available commands:
      concatenate              Concatenate sequences found in input files.
      examine                  Examine sequences. If data is a multi sequence alignment, additionally analyze columns.
      filter-columns           Filter columns of multi sequence alignments.
      filter-rows              Filter rows (or sequences) found in input files.
      simulate                 Simulate multi sequence alignments.
      sub-sample               Sub-sample columns from multi sequence alignments.
      translate                Translate from DNA to Protein or DNAX to ProteinX.
    
    
    Available sequence file formats:
      - FASTA
    
    Available alphabets:
      - DNA (nucleotides)
      - DNAX (nucleotides; including gaps)
      - DNAI (nucleotides; including gaps, and IUPAC codes)
      - Protein (amino acids)
      - ProteinX (amino acids; including gaps)
      - ProteinS (amino acids; including gaps, and translation stops)
      - ProteinI (amino acids; including gaps, translation stops, and IUPAC codes)
    
    ELynx
    -----
    A Haskell library and tool set for computational biology. The goal of ELynx is
    reproducible research. Evolutionary sequences and phylogenetic trees can be
    read, viewed, modified and simulated. The command line with all arguments is
    logged consistently, and automatically. Data integrity is verified using SHA256
    sums so that validation of past analyses is possible without the need to
    recompute the result.
    
    slynx     Analyze, modify, and simulate evolutionary sequences.
    tlynx     Analyze, modify, and simulate phylogenetic trees.
    elynx     Validate and redo past analyses.
    
    Get help for commands:
      slynx --help
    
    Get help for sub commands:
      slynx examine --help


## Sub command

The documentation of sub commands can be accessed separately:

    cabal run slynx -- simulate --help
    # OR: stack exec slynx -- simulate --help
    # OR: slynx simulate --help

    ELynx Suite version 0.8.0.0.
    Developed by Dominik Schrempf.
    Compiled on October 27, 2024, at 07:14 am, UTC.
    
    Usage: slynx simulate (-t|--tree-file Name) [-s|--substitution-model MODEL]
                          [-m|--mixture-model MODEL] [-n|--global-normalization]
                          [-e|--edm-file NAME] [-p|--siteprofile-files NAMES]
                          [-w|--mixture-model-weights "[DOUBLE,DOUBLE,...]"]
                          [-g|--gamma-rate-heterogeneity "(NCAT,SHAPE)"]
                          (-l|--length NUMBER) [-S|--seed INT]
    
      Simulate multi sequence alignments.
    
    Available options:
      -h,--help                Show this help text
      -V,--version             Show version
      -t,--tree-file Name      Read tree from Newick file NAME
      -s,--substitution-model MODEL
                               Set the phylogenetic substitution model; available
                               models are shown below (mutually exclusive with -m
                               option)
      -m,--mixture-model MODEL Set the phylogenetic mixture model; available models
                               are shown below (mutually exclusive with -s option)
      -n,--global-normalization
                               Normalize mixture model globally (one normalization
                               constant for all components)
      -e,--edm-file NAME       Empirical distribution model file NAME in Phylobayes
                               format
      -p,--siteprofile-files NAMES
                               File names of site profiles in Phylobayes format
      -w,--mixture-model-weights "[DOUBLE,DOUBLE,...]"
                               Weights of mixture model components
      -g,--gamma-rate-heterogeneity "(NCAT,SHAPE)"
                               Number of gamma rate categories and shape parameter
      -l,--length NUMBER       Set alignment length to NUMBER
      -S,--seed INT            Seed for random number generator (default: random)
      -h,--help                Show this help text
    
    Substitution models:
    -s "MODEL[PARAMETER,PARAMETER,...]{STATIONARY_DISTRIBUTION}"
       Supported DNA models: JC, F81, HKY, GTR4.
         For example,
           -s HKY[KAPPA]{DOUBLE,DOUBLE,DOUBLE,DOUBLE}
           -s GTR4[e_AC,e_AG,e_AT,e_CG,e_CT,e_GT]{DOUBLE,DOUBLE,DOUBLE,DOUBLE}
              where the 'e_XY' are the exchangeabilities from nucleotide X to Y.
       Supported Protein models: Poisson, Poisson-Custom, LG, LG-Custom, WAG, WAG-Custom, GTR20.
         MODEL-Custom means that only the exchangeabilities of MODEL are used,
         and a custom stationary distribution is provided.
         For example,
           -s LG
           -s LG-Custom{...}
           -s GTR20[e_AR,e_AN,...]{...}
              the 'e_XY' are the exchangeabilities from amino acid X to Y (alphabetical order).
       Notes: The F81 model for DNA is equivalent to the Poisson-Custom for proteins.
              The GTR4 model for DNA is equivalent to the GTR20 for proteins.
    
    Mixture models:
    -m "MIXTURE(SUBSTITUTION_MODEL_1,SUBSTITUTION_MODEL_2[PARAMETERS]{STATIONARY_DISTRIBUTION},...)"
       For example,
         -m "MIXTURE(JC,HKY[6.0]{0.3,0.2,0.2,0.3})"
    Mixture weights have to be provided with the -w option.
    
    Special mixture models:
    -m CXX
       where XX is 10, 20, 30, 40, 50, or 60; CXX models, Quang et al., 2008.
    -m "EDM(EXCHANGEABILITIES)"
       Arbitrary empirical distribution mixture (EDM) models.
       Stationary distributions have to be provided with the -e or -p option.
       For example,
         LG exchangeabilities with stationary distributions given in FILE.
         -m "EDM(LG-Custom)" -e FILE
         LG exchangeabilities with site profiles (Phylobayes) given in FILES.
         -m "EDM(LG-Custom)" -p FILES
    For special mixture models, mixture weights are optional.

