# sdl2-mixer

[![Hackage](https://img.shields.io/hackage/v/sdl2-mixer.svg)](https://hackage.haskell.org/package/sdl2-mixer)
[![GitLab](https://gitlab.homotopic.tech/haskell/sdl2-mixer/badges/master/pipeline.svg)](https://gitlab.homotopic.tech/haskell/sdl2-mixer)

Haskell bindings to SDL2_mixer. Provides both raw and high level bindings.

The
[original SDL2_mixer documentation](http://www.libsdl.org/projects/SDL_mixer/docs/SDL_mixer.html)
can also help, as the bindings are close to a direct mapping.

## Examples

Several example executables are included with the library. You can find them in
the `examples` directory.

```bash
stack exec -- sdl2-mixer-basic <file>
stack exec -- sdl2-mixer-raw <file>
stack exec -- sdl2-mixer-music <file>
stack exec -- sdl2-mixer-jumbled <file1> ... <fileN>
stack exec -- sdl2-mixer-effect <file>
```
