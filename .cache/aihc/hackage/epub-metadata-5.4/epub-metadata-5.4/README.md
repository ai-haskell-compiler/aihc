# epub-metadata


## Synopsis

Library for parsing epub document metadata (Haskell)


## Description

Library for parsing and manipulating epub document metadata. Supports epub versions 2 and 3.

This library was constructed by studying the IDPF specifications for epub documents found [here for version 2.0.1](http://www.idpf.org/epub/20/spec/OPF_2.0.1_draft.htm) and [here for version 3.0](http://www.idpf.org/epub/30/spec/epub30-publications.html)


### Why was this done?

The motivation for this project grew out of my desire to take charge
of missing or incorrect epub metadata in books I have purchased. I
started out using the Calibre open source tools for examining this
info. Limitations and incomplete implementation of those tools led
me here to build a more complete library in one of the best
programming languages ever created.


### Why didn't I just use existing solutions?

#### Calibre ebook-meta utility

I experienced various problems using this software, such as:

* Incomplete and in some cases incorrect handling of tags that can
  exist more than once (creator, contributor), particularly when they
  are differentiated using attributes according to the spec.

* Unable to display many fields in the OPF Package Document metadata
  specification. Unable to manipulate data that is represented as
  attributes of tags in the OPF spec.

* Astonishingly slow performance. A command-line renaming tool built with
  epub-metadata is more than 45 times faster at parsing and displaying
  epub metadata. I'm going to blame Python here for Calibre's
  performance. This has had a big impact on projects where I've been
  processing hundreds of epubs in batch operations.


#### epub on Hackage, epub E-Book construction support library

* The focus of this project seems to be with building new documents,
  not parsing existing files. And there is a specific attempt to do more
  than the metadata, to gather up the content and other metafiles that
  make up an epub for creation.

* Examining Codec.Ebook.OPF.Types, most of the metadata fields
  from the OPF Package Document spec are missing or aren't modeled
  thoroughly. I felt that to contribute to this project, I would have
  had to significantly rip up the types and redesign them.

* At this time it seemed like a better solution for me to
  start fresh with modelling the types and code to manipulate them. That
  said, I would be very interested in combining the epub and epub-metadata
  projects at some point in some way that makes sense.


### Using this library

   Please see the [Haddock documentation](https://hackage.haskell.org/package/epub-metadata) or source code for [Codec.Epub](https://github.com/dino-/epub-metadata/blob/master/src/lib/Codec/Epub.hs)

   This file has a working example of using this library.


## Getting source

- Download the cabalized source package [from Hackage](http://hackage.haskell.org/package/epub-metadata)
- Get the source with git: `$ git clone https://github.com/dino-/epub-metadata.git`
- Get the source with cabal-install: `$ cabal get epub-metadata`
- Get the source with stack: `$ stack unpack epub-metadata`
- If you're just looking, [browse the source](https://github.com/dino-/epub-metadata)

And once you have it, building the usual way:

    $ stack build
    $ stack test
    $ stack haddock

### Generating tags

    $ hasktags --ctags .


## Contact

Dino Morelli <[dino@ui3.info](mailto:dino@ui3.info)>
