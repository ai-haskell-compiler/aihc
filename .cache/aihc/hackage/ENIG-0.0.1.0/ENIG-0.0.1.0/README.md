ENIG
====

ENIG is an acronym of typical Korean particles "Eun Neun I Ga (은는이가)".
Korean postposition particles change their form depending on the pronounciation of the previous consonant.
However, in trouble, because figuring out the previous consonant is not an easy problem.
Therefore, most of the programmers who handle Korean treat Korean by starting with "SOMETHING은(는) ~~".
This seems to be awkward in a sentence like "SOMETHING is(are), Two apple(s)."

ENIG focuses on programmers to handle Korean strings.

## How to use

### Core functions

#### enigPPP

You can get proper Korean particle by giving `enigPPP` post word and a Korean postposition particle category `

### Handy functions (Not yet implemented)

You can use ENIG without any integration on your code by `enigAuto`.
When you give a text like "과자이(가) 두개입니다." to `enigAuto` like `enigAuto "과자이(가) 두개입니다."`, the function returns `"과자가 두개입니다."`


## Long-range Plans

### Korean

* Managing Korean string which involves Korean particle.

### English

* Managing English string which involves plural/singular form.

## Known Issues

### When you get error like `invalid argument` or `invalid character`

When you get a message during test the library like `invalid argument` or `invalid character`, check your language configuration.

This is not program error.

Because of this library handling Hangul itself, the test code should contains Korean characters.
Therefore, You should set your shell's encoding by `chcp 65001` for Windows or `export LC_ALL=en_US.UTF-8` for *nix platforms.
