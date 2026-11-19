# The Lazy Reader

The Lazy Reader is the illustrated guide to the ideas inside aihc, at
<https://lazy.aihc.app/>. The manual at <https://docs.aihc.app/> is for
people who read manuals. This guide is for the others.

## Layout

- `mkdocs.yml`, `src/`: the site. Material for MkDocs builds it, with the
  theme override in `overrides/` that removes the navigation sidebar.
- `src/javascripts/play.js`: the playgrounds. A `<div class="lazy-play">`
  with a `data-command` of `parse`, `names`, `types`, or `compile` becomes
  an editor and an output pane.
- `src/play/kit/`: the web kit, which is aihc for the browser. A build
  writes it here. It is not in git.
  The documentation preview keeps the compressed files on R2 and not on
  the preview branch. `kit/manifest.json` names their URL.

## The web kit

The playgrounds use the same web kit that each release of aihc ships.
Its source is in `web-kit/`, and `web-kit/README.md` gives its layout and
its JavaScript interface. `play.js` imports `kit/aihc.js` and does not
speak the request protocol of the helper itself.

The helper in the kit is `bin/aihc-play`: the front end of aihc behind a
small request protocol, compiled by aihc to `wasm32-wasip3` and transpiled
by jco. `scripts/build-web-kit.sh` builds the kit, and the deploy workflow
runs that script as `nix run .#build-web-kit`. Thus the guide always has
the compiler of its own commit, and a website pins a release.

The helper does not compile the core libraries. The build compiles them
once, for the helper itself, and `aihc-dev play-bundle` writes their
naming facts and type facts from the store as one bundle. The kit
fetches the bundle and sends it to the helper with the `load-bundle`
request. The helper decodes the exports of every module at once, and
decodes each type fact the first time the type checker looks it up. The
format is in `bin/aihc/src/Aihc/Cli/PlayBundle.hs`.

The page then sends each edit to the kit and shows the answer.

The playground needs JavaScript Promise Integration, which Chrome 137,
Firefox 139 and Safari 27 have.

## Local work

`just guide` builds the kit and serves the site at
<http://127.0.0.1:8000/>. The first
kit build compiles the dependencies of aihc-play for wasm and takes
some minutes. Later builds reuse the store under
`.aihc-target/lazy-reader-store`.

`aihc-play --bundle FILE` runs the helper natively over a bundle, for a
check outside the browser.
