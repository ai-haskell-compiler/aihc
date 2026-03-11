# tce-conf


## Synopsis

Very simple config file reading (Haskell)


## Description

This package contains modules for runtime reading of very simple
config files of the `key=value` style or as a Haskell data structure
to be deserialized with `Read`. The modules support files with
blank lines and simple single-line comments, but nothing else.

### KVConf

Pros

- Well-known `key=value` config file syntax
- Adding new entries doesn't necessarily mean code changes as they
  are stored in a Map

Cons

- All values are `String` and so may as well be untyped. Conversion
  to other types will be required by users of the library.
- Simulating hierarchies requires hacky key syntax with dots or
  what-have-you
- Only single-line comments supported at this time, with #
- No support for INI sections or variable substitution


### ReadConf

Pros

- Config files are strongly typed Haskell source code. `Read`
  instancing takes care of everything!
- Can easily support hierarchical data because Haskell data types
  do so

Cons

- Config files must adhere to Haskell source syntax (may not be a
  con for some)
- Changing the config file at all requires code changes to keep
  the types synced with each other.
- Only single-line comments supported at this time, with --
- No support for variable substitution


## Getting source

- Get the source with darcs: `$ darcs get http://hub.darcs.net/dino/tce-conf`
- If you're just looking, [browse the source](http://hub.darcs.net/dino/tce-conf)

And once you have it, building the usual way:

    $ stack build
    $ stack test


## Contact

Dino Morelli <[dino@ui3.info](mailto:dino@ui3.info)>
