---
title: The Lazy Reader
hide:
  - navigation
  - toc
---

<div class="lazy-cover" markdown>

![A sleepy cat lies on a book. Its tail is a lambda.](assets/mascot.svg)

# The Lazy Reader

<p class="lazy-tagline">For people who will not read the manual.</p>

This is a small, illustrated guide to the ideas inside
[aihc](https://github.com/ai-haskell-compiler/aihc), a Haskell compiler.
Each chapter has one idea, one picture, and one playground.
The playground is the compiler itself. It runs in your browser.

</div>

## Chapters

<div class="lazy-cards" markdown>

<a class="lazy-card" href="01-hello-compiler/">
<span class="lazy-card-number">Chapter 1</span>
<span class="lazy-card-title">Hello, compiler</span>
<span class="lazy-card-note">Text becomes a tree. Watch the parser work.</span>
</a>

<a class="lazy-card" href="02-who-is-who/">
<span class="lazy-card-number">Chapter 2</span>
<span class="lazy-card-title">Who is who</span>
<span class="lazy-card-note">Every name gets a color. Find out which x is which.</span>
</a>

<a class="lazy-card" href="03-what-is-what/">
<span class="lazy-card-number">Chapter 3</span>
<span class="lazy-card-title">What is what</span>
<span class="lazy-card-note">The compiler guesses the types. It guesses well.</span>
</a>

<span class="lazy-card lazy-card-soon">
<span class="lazy-card-number">Chapter 4</span>
<span class="lazy-card-title">The honest language</span>
<span class="lazy-card-note">System FC, where nothing is implicit. Not written yet.</span>
</span>

</div>

## How to read this guide

You do not need to know Haskell well.
You do not need to know how a compiler works.
That is the point.

Each chapter ends with a playground.
Change the code on the left, and the compiler answers on the right.
The compiler is aihc, compiled to WebAssembly by aihc.
It needs a recent browser: Chrome, Firefox, or Safari 27.

If you want the long version, the [manual](https://docs.aihc.app/) has it.
This guide has the short version, with pictures.
