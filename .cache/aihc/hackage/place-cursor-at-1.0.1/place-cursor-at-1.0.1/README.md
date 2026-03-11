![place-cursor-at](https://raw.githubusercontent.com/unclechu/place-cursor-at/b636cb9651c6ededbbda80489c79ff4a68653d96/artwork/logo/horizontal.svg)

A utility for X11 that moves the mouse cursor using the keyboard.
Written in Haskell.

This utility only gives you an ability to quickly move your mouse cursor to one
of 9 common spots of the screen. It’s intended to reduce the use of the mouse,
but not to replace it completely. It doesn’t provide you accurate aiming, only
approximate.

You bind a hotkey (in your WM configuration or by using [xbindkeys]) to run
`place-cursor-at` (say `Super+M`) and when you press it you see 9 windows on
current screen with letters shown on them. When you press a letter on your
keyboard that is associated with one of those windows your mouse cursor appear
at the center of that window and the application closes. Thus you can quickly
move your mouse cursor to one of 9 different spots by two-step combo.

Other key bindings:

1. Press `Escape` to close the application
   (when one of the *letter* windows is focused)

2. Move focus to a *letter* window where you want your mouse cursor to be and
   press `Enter` (this is the same as pressing that *letter* on your keyboard)

I originally created this tool it for myself to use it with
[xmonad](https://github.com/unclechu/xmonadrc)
and [i3](https://github.com/unclechu/i3rc).

![Screenshot](https://raw.githubusercontent.com/unclechu/place-cursor-at/b636cb9651c6ededbbda80489c79ff4a68653d96/artwork/screenshot.png)

## Requirements

Either one:

1. [Nix Package Manager]

2. [The Haskell Tool Stack] and:

   1. `libX11` development files
   2. `libXinerama` development files

## How to use

### With Nix

See [Nix Package Manager].

You can run this application like this:

``` sh
nix-shell --run place-cursor-at
```

See also [shell.nix]’s arguments for available options.
For instance if you want to enter a `nix-shell` with `cabal` available:

``` sh
nix-shell --arg withCabal true
```

#### NixOS

You can add this application into your NixOS `configuration.nix` like this:

``` nix
{ pkgs, ... }:
let
  place-cursor-at-src = pkgs.fetchFromGitHub {
    owner = "unclechu";
    repo = "place-cursor-at";
    rev = "ffffffffffffffffffffffffffffffffffffffff"; # Git commit hash
    sha256 = "0000000000000000000000000000000000000000000000000000";
  };

  place-cursor-at = pkgs.callPackage place-cursor-at-src {};
in
{
  environment.systemPackages = [ place-cursor-at ];
}
```

### With Stack

See [The Haskell Tool Stack].

``` sh
stack build
stack exec place-cursor-at
```

You could install `place-cursor-at` binary to `~/.local/bin` directory
(make sure you have this directory in your `PATH` environment variable):

``` sh
stack install
```

#### NixOS note

If you need to work with Stack under NixOS for whatever reason you would have to
use `--system-ghc` in order to make it work. You can manually add `--system-ghc`
to each call of `stack` or define this alias for your convenience:

``` sh
alias stack='stack --system-ghc'
```

## Using it as script

You also can run this application as a script
(`nix-shell` script, so you have to
[install Nix Package Manager](https://nixos.org/nix/manual/#chap-installation)
first):

``` sh
src/place-cursor-at.hs
```

But this application is intended to be used very often (to be run again and
again many times) and to be very responsive, so it’s better to precompile it to
reduce startup time.

## Xinerama note

By default it appears and do its stuff on display where your mouse cursor is.
But you can specify which display where it should appear on:

``` sh
place-cursor-at 1
```

Or on third display:

``` sh
place-cursor-at 3
```

## Jump to specific position without GUI

You can immediately jump to specific position of a screen without showing any
GUI, like this:

``` sh
place-cursor-at LT
```

It’s case-insensitive, this also would work:

``` sh
place-cursor-at lt
```

And you also can specify a display you want to jump to:

``` sh
place-cursor-at lt 1
```

In any order of the arguments:

``` sh
place-cursor-at 1 lt
```

### Codes of available positions

```
| Code | Deciphering   |
| -    | -             |
| LT   | Left-Top      |
| LC   | Left-Center   |
| LB   | Left-Bottom   |
| CT   | Center-Top    |
| CC   | Center-Center |
| CB   | Center-Bottom |
| RT   | Right-Top     |
| RC   | Right-Center  |
| RB   | Right-Bottom  |
```
## Author

[Viacheslav Lotsmanov]

## Contributors

The [logo] is created by [psikoz] and
some technical adjustments are made by [Viacheslav Lotsmanov].

## Licenses

1. **place-cursor-at** — [GNU/GPLv3](https://raw.githubusercontent.com/unclechu/place-cursor-at/b636cb9651c6ededbbda80489c79ff4a68653d96/LICENSE)

2. The logo —
   [Creative Commons — Attribution 4.0 International (CC BY 4.0)][CC BY 4.0]

   These files in [artwork/logo][logo] directory in particular:

   - [artwork/logo/horizontal.svg](https://raw.githubusercontent.com/unclechu/place-cursor-at/b636cb9651c6ededbbda80489c79ff4a68653d96/artwork/logo/horizontal.svg)
   - [artwork/logo/horizontal.png](https://raw.githubusercontent.com/unclechu/place-cursor-at/b636cb9651c6ededbbda80489c79ff4a68653d96/artwork/logo/horizontal.png)
   - [artwork/logo/vertical.svg](https://raw.githubusercontent.com/unclechu/place-cursor-at/b636cb9651c6ededbbda80489c79ff4a68653d96/artwork/logo/vertical.svg)
   - [artwork/logo/vertical.png](https://raw.githubusercontent.com/unclechu/place-cursor-at/b636cb9651c6ededbbda80489c79ff4a68653d96/artwork/logo/vertical.png)
   - [artwork/logo/icon.svg](https://raw.githubusercontent.com/unclechu/place-cursor-at/b636cb9651c6ededbbda80489c79ff4a68653d96/artwork/logo/icon.svg)
   - [artwork/logo/icon.png](https://raw.githubusercontent.com/unclechu/place-cursor-at/b636cb9651c6ededbbda80489c79ff4a68653d96/artwork/logo/icon.png)

[The Haskell Tool Stack]: https://docs.haskellstack.org/en/stable/README/
[Nix Package Manager]: https://nixos.org/manual/nix/stable/#ch-about-nix
[xbindkeys]: https://linux.die.net/man/1/xbindkeys

[shell.nix]: https://github.com/unclechu/place-cursor-at/blob/b636cb9651c6ededbbda80489c79ff4a68653d96/shell.nix
[logo]: https://github.com/unclechu/place-cursor-at/blob/b636cb9651c6ededbbda80489c79ff4a68653d96/artwork/logo

[CC BY 4.0]: https://creativecommons.org/licenses/by/4.0/

[Viacheslav Lotsmanov]: https://github.com/unclechu
[psikoz]: https://github.com/psikoz
