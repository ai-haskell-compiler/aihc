# Real\* Dice\* [![Hackage](https://img.shields.io/hackage/v/real-dice.svg)](https://hackage.haskell.org/package/real-dice)

Random number generation using real dice and other media that I actually touched!

## Introduction

Sometimes the digital world can feel cold and sterile compared to the messy and unpredictable nature of our physical environment. Purely functional languages like Haskell are, for better and for worse, particularly susceptible to this digital de-messification

In celebration of Haskell's determinative nature, this package provides random data from my interaction with the physical world as well as utilities to leverage this data for random number generation and other RNG-based tasks

This package can be used to bring a human touch to our digital world

## Use

The API of the Coin, Die, and RNG modules more or less match the API of System.Random's `random` and `randomR` functions, but with each function handling a specific type and domain. For example, `flipCoin` returns Boolean values and `roll1d n` returns Integers in the range [1, n]

Running the executable will produce the standard randomized data as defined in the Generate modules (see `app/Generate/` and `src/RealDice/Generate/`). If valid seed data exists in the `data-seed/` directory, this will be randomized with the Real\*Dice\* data to generate custom balanced data in the `data-generated/` directory (Coming Soon!)

## FAQ

### How does Real\*Dice\* work?

Check the source, [Hackage](https://hackage.haskell.org/package/real-dice), or Hoogle for documentation! 

The RNG system itself is a basic randomized table lookup. If it's good enough for [the most perfect software ever written](https://www.youtube.com/watch?v=pq3x1Jy8pYM&t=32s), it's good enough for me!

### What changes are planned?

Check the [issues](https://github.com/kaBeech/real-dice/issues`) and `TODO.md`!

### I encountered a bug or have a cool feature idea!

Please [open an issue](https://github.com/kaBeech/real-dice/issues/new) or [submit a PR](https://github.com/kaBeech/real-dice/compare)!

### Why are there asterisks in "Real\*Dice\*"?

[Ce ne sont pas des dés](https://en.wikipedia.org/wiki/The_Treachery_of_Images)

Data contained in this project was generated with physical dice, but the project itself is not a physical object

Additionally, not all of the gathered data comes from rolling dice. Most of it comes from drawing poker chips, which can be much faster. Methodology for gathering the data is documented in `src/RealDice/Generate/RawData.hs`

### ...but why? On the whole thing?

Because it's cool.

The interaction between the digital world and the physical world fascinates me. Musicians love to talk about warmth and saturation in audio signals - see [Variety Of Sound](https://varietyofsound.wordpress.com/blog/) for some mind-blowing information and audio plugins. This warmth traditionally comes from signal imperfections springing from physical components. This package doesn't scratch the surface of this kind of signal theory, but it's an attempt to use the physical world to influence the digital one

Plus, we all know that observer interaction affects quantum systems, so it seems fun to have some random data touched by a human 😸
