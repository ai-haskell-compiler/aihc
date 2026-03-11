# sdl2-gfx

[![Hackage](https://img.shields.io/hackage/v/sdl2-gfx.svg)](https://hackage.haskell.org/package/sdl2-gfx)
[![GitLab](https://gitlab.homotopic.tech/haskell/sdl2-gfx/badges/master/pipeline.svg)](https://gitlab.homotopic.tech/haskell/sdl2-gfx)

Haskell bindings to SDL2_gfx. Provides both raw and high level bindings.

The
[original SDL2_gfx documentation](http://www.ferzkopp.net/Software/SDL2_gfx/Docs/html/index.html)
can also help, as the bindings are close to a direct mapping.

##### Example

A small example executable is included with the library. It uses many parts of
the library to draw a chaotic jumbled mess on your screen. You can find it in
the `example` directory.

```bash
stack exec -- sdl2-gfx-example
```
