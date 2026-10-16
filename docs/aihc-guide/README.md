# The Lazy Reader

The Lazy Reader is the illustrated guide to the ideas inside aihc, at
<https://lazy.aihc.app/>. The manual at <https://docs.aihc.app/> is for
people who read manuals. This guide is for the others.

## Layout

- `mkdocs.yml`, `src/`: the site. Material for MkDocs builds it, with the
  theme override in `overrides/` that removes the navigation sidebar.
- `src/javascripts/play.js`: the playgrounds. A `<div class="lazy-play">`
  with a `data-command` of `parse`, `names`, or `types` becomes an editor
  and an output pane.
- `src/play/host.js`: the browser host of the playground helper. It gives
  the helper standard input and output and nothing else.
- `src/play/helper/`: the WebAssembly helper and the bundle of the core
  libraries, `core.bundle.gz`. A build writes them here. They are not in
  git.

## The helper

The helper is `bin/aihc-play`: the front end of aihc behind a small
request protocol, compiled by aihc to `wasm32-wasip3` and transpiled by
jco. `scripts/build-lazy-reader-helper.sh` builds it, and the deploy
workflow runs that script as `nix run .#build-lazy-reader-helper`.

The helper does not compile the core libraries. The build compiles them
once, for the helper itself, and `aihc-dev play-bundle` writes their
naming facts and type facts from the store as one bundle. The page
fetches the bundle and sends it to the helper with the `load-bundle`
request. The helper decodes the exports of every module at once, and
decodes each type fact the first time the type checker looks it up. The
format is in `bin/aihc/src/Aihc/Cli/PlayBundle.hs`.

The page then sends each edit to the helper and shows the answer.

The playground needs JavaScript Promise Integration, which Chrome 137,
Firefox 139 and Safari 27 have.

## Local work

`just guide` builds the helper and the bundle and serves the site at
<http://127.0.0.1:8000/>. The first
helper build compiles the dependencies of aihc-play for wasm and takes
some minutes. Later builds reuse the store under
`.aihc-target/lazy-reader-store`.

`aihc-play --bundle FILE` runs the helper natively over a bundle, for a
check outside the browser.
