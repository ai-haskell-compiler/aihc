--------------------------------------------------------------------------------
title: lentil manual
author: Francesco Ariis
summary: lentil issue tracker user manual
tags: lentil,issue,bug,issue tracker,bug tracker,manual
published: 2015-06-09 00:00:00
revised: 2023-03-14 00:00:00
--------------------------------------------------------------------------------


==================================
lentil issue tracker - user manual
==================================


When I start a new project I litter my code with:

::

  -- TODO: does outPop work with b/w terminals? [ansi]

Dealing with these is easy at first (with some help from unix utilities), but
it becomes more and more unwieldy as the project expands. Migrating takes
time and effort and I like the convenience of having bug reports detailed near
the offending functions.

`lentil`_ helps you with housekeeping, querying, exporting issues without
having to modify a single line of code.

.. _`lentil`: http://ariis.it/static/articles/lentil/page.html

.. raw:: html

   <p><br/></p>

Installation
------------

(If you don't want to compile it yourself, these binaries are available:
`linux`_ ◇ `windows`_ ◇ `signed hashes`_.)

.. _`linux`: /static/dist/files/lentil-linux-x86_64.zip
.. _`windows`: /static/dist/files/lentil-windows-x86_64.zip
.. _`signed hashes`: /static/dist/page.html

``lentil`` is written in Haskell, which you can easily grab from your
OS repository or `GHCup`_ (no root/admin needed). Once you do that,
installing lentil is as easy as:

.. _`GHCup`: https://www.haskell.org/ghcup/#

::

  cabal update             # this might take a few minutes!
  cabal install lentil

The executable is located in ``.cabal/bin/``; to make it available
everywhere add this line to your ``.bashrc``:

::

  export PATH=$PATH:~/.cabal/bin

.. raw:: html

   <p><br/></p>

Basic usage
-----------


To test lentil, fetch the `example repository`_, unzip it, enter the folder
and type:

.. _`example repository`: test.zip

::

  lentil .

This should output a basic list of issues:

::

  alpha.hs
     4  add prompt before getline, or the user might complain! [interface]
    11  add a type signature to replace [lint]

  subfolder/beta.hs
     3  make export list explicit in Beta [lint]
     5  I think constants should not be exposed here, but imported from
        another package

You can specify more than one folder/path with ``lentil foldera folderb ...``
or exclude folders/paths with ``lentil . -x foldera/sub``.

lentil scans directory recursively, not following symlinks, hidden folders
(``.git``, ``.cabal``, etc.) or folders starting with an underscore (``_``).
Current working directory is indicated by a single dot (``.``).

.. raw:: html

   <p><br/></p>


Input format
------------

lentil parses basic ``TODO`` issues. The precise syntax is: keyword (one
of ``todo``, ``fixme``, ``xxx``), optional semicolon, followed
by a space, followed by free-form text (text can be multiline). In so many
words, it is quite liberal in what it accepts:

::

  -- These will all be accepted

  // TODO: explanatory issue

  // todo ehy you left an assert out there!

  //Fixme bad coding style

  /* Xxx: add to version control.
          Ask Timothy */

  -- FIXME


You can optionally put tags at the end or at the beginning of your issue,
like this:

::

  // FIXME: Mr. Burns should enter from the *right* side of the
  //        nuclear station [script] [priority:1]

  -- todo: [rhyme] [magic] double trouble... I always forget the
  --       words :s [memory]

Tags are single words which are useful to catalogue and identify issues. Use
them aplenty as they will make slicing and dicing your issue-base a breeze.
Since the semantic of ``FIXME`` and ``XXX`` is specific (in respect to
the usual ``TODO``), they are automatically added as a tag.

You can specify custom flag-words too, by using the ``-w`` option, e.g.:

::

  lentil . -w hack

  // HACK this issue will be parsed

User provided flagwords will have a tag automaticaly added (like ``FIXME``
and ``XXX``).

.. raw:: html

   <p><br/></p>

Recognised files
----------------

As now lentil parses:

- haskell source files (``.hs``, ``.lhs``, ``.chs``, ``.hsc``, ``.cabal``)
- Elm source files (``.elm``)
- PureScript source files (``.purs``)
- C source files (``.c``, ``.h``), C++ (``.cpp``, ``.hpp``), Java (``.java``)
- javascript source files (``.js``)
- TypeScript source files (``.ts``)
- CoffeeScript source files (``.coffee``)
- pascal source files (``.pas``, ``.pp``, ``.inc``)
- python source files (``.py``)
- ruby source files (``.rb``)
- perl source files (``.pl``, ``.pm``, ``.t``)
- shell script source files (``.sh``)
- nix source files (``.nix``)
- xml source files (``.xml``, ``.html``)
- Erlang source files (``.erl``, ``.hrl``, ``.escript``) including YECC
  (``.yrl``) and LEEX (``.xrl``)
- OCaml source files (``.ml``, ``.mli``)
- Scala source files (``.scala``)
- Rust source files (``.rs``, ``.rlib``)
- Standard ML source files (``.sml``)
- OpenGL Shading Language source files (``.glsl``)
- Restructured Text (Sphinx) ``.. ::todo`` directives (``.rst``)
- Org mode for Emacs files (``.org``), both ``TODO`` and list-like items
- Markdown files (``.md``)
- R files (``.r``, ``.R``)
- Forth files (``.fs``, ``.fth``, ``.4th``)
- YAML files (``.yaml``, ``.yml``)
- LaTeX files (``.tex``)
- Zig files (``.zig``)
- Scheme files (``.scm``, ``.ss``)
- Lisp files (``.lisp``, ``.lsp``, ``.l``, ``.cl``, ``.fasl``)
- Agda files (``.agda``, ``.lagda``)
- plain text files (``.txt``)

If you want a file type ``.xyz`` to be recognised as one in the list above,
invoke lentil with an extension alias:

::

  lentil . -a xyz%cpp

  # multiple aliases
  lentil . -a xyz%cpp -a qkw%js

but please *please* **please** contact me to have the extension(s) included
in `lentil` natively, as this will make the program more convenient to use
(if you want to speed up the process, please include a description of the
comment syntax and string/character syntax too).


.. raw:: html

   <p><br/></p>


Query options
-------------

You can filter the issues to be displayed:

- ``-t EXPR`` filters by issue tag
- ``-p EXPR`` filters by issue filepath
- ``-d EXPR`` filters by description

where ``EXPR`` is a valid regular expression. Examples:

::

  # lists all issues with tag containing the string "bug"
  lentil . -t bug

  # lists all issues in files which name contains the
  # string "ACME" or "foo"
  lentil . -p "(ACME|foo)"

  # lists all issues with tag containing the string "bug" and
  # with "urgent" in the description
  lentil . -t bug -d urgent

As the last example highlight, if you use these options together, they
will be chained using a boolean ``AND``.

To negate an ``EXPR`` use the corresponding capitalised option flags
``-T``, ``-P``, ``-D``:

::

  # lists all issues *without* the word "urgent" in their description
  lentil . -D urgent


Orphan (tagless) issues are sometimes a nuisance, a handy way to list
them is:

::

  lentil . -T .

.. raw:: html

   <p><br/></p>


Format options
--------------

The ``-f TYPE`` option modifies the output format, with ``pretty`` being
the default.

``-f csv`` exports issues to CSV (RFC 4180).

``-f xml`` exports issues to XML.

``-f tagpop``, lists tags by their popularity (reverse order),
useful to get a summary of open issues:

::

  lentil . -x test/test-files/ -f tagpop
  Tags popularity:
       8  [feature:intermediate]
       6  [test]
       5  [feature:advanced]
       3  [bug]
       2  [refactor]
       2  [lint]
       2  [feature:basic]
       1  [urgency:3]
       1  [design]

``-f comp`` displays issues in a format similar to the one emitted
by compilers (GCC, GHC, etc.):

::

  alpha.hs:4: add prompt before getline, or the user might complain! [interface]
  alpha.hs:11: add a type signature to replace [lint]

This is useful because it is recognised by editors like Emacs,
which can turn them into active links to the relevant file/position.

``-f file`` exports a list of files where issues are present:

::

  src/Lentil/Export.hs
  src/Lentil/File.hs
  src/Lentil/Parse/Run.hs
  src/Lentil/Parse/Syntaxes.hs
  src/Main.hs

.. raw:: html

   <p><br/></p>


Other options
-------------

``--output FILE`` is used to output the report to a file instead of stdout.

``-v`` outputs the program version.

``--help`` displays cheatsheet option help.


.. raw:: html

   <p><br/></p>

Tips
----

When output gets too big for a single screen, call lentil as:

::

  lentil . | less -R

This will allow you to browse the issues in a pager *without* losing
ANSI colour formatting.
