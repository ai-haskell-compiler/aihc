# nerd-font-icons
## Nerd Fonts Icons for use in Haskell

A library for using [NerdFonts](https://www.nerdfonts.com/) Icons in Haskell
Source code. 


### Usage

Import the module and use the icons in your strings.

```haskell
import qualified Graphics.Icons.NerdFonts as NF

memInfo :: String ->  String
memInfo memUsedPercent = NF.mdMemory <> " " <> memUsedPercent <> "%"
```

Autocompletion and [this Cheat Sheet](https://www.nerdfonts.com/cheat-sheet)
are useful to find the icons you want.

### Requirements

Requires a [patched font](https://www.nerdfonts.com/font-downloads) to properly
display the icons.

You can find more info on how to patch your own fonts
[here](https://github.com/ryanoasis/nerd-fonts?tab=readme-ov-file#font-patcher)

### Info

The module is generated from the [Nerd Fonts
Repo](https://github.com/ryanoasis/nerd-fonts) using
[this](https://github.com/ccntrq/nerd-fonts-icons-hs/blob/master/scripts/generate.sh)
shell script.

---

Based on **Nerd Fonts Version**:
[`v3.3.0`](https://github.com/ryanoasis/nerd-fonts/tree/v3.3.0)
