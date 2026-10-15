---
title: Hello, compiler
hide:
  - toc
---

<p class="lazy-chapter">Chapter 1</p>

# Hello, compiler

<p class="lazy-lede">A compiler is a program that reads a program. It reads text that a person wrote, and it writes a program that a machine can run. This chapter is about the first step: how text becomes a tree.</p>

<figure class="lazy-figure lazy-anim">
<svg viewBox="0 0 720 220" xmlns="http://www.w3.org/2000/svg" role="img" aria-label="Source text becomes a syntax tree, and the tree becomes a program.">
  <defs>
    <marker id="arrow" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" orient="auto-start-reverse">
      <path d="M0 0 L10 5 L0 10 Z" fill="#7d6fa6"/>
    </marker>
  </defs>
  <!-- The text -->
  <rect x="12" y="40" width="196" height="140" rx="14" fill="#faf8f3" stroke="#e6e0d6"/>
  <text x="110" y="28" text-anchor="middle" font-family="Inter, system-ui, sans-serif" font-size="12" font-weight="600" letter-spacing="1.5" fill="#6b655d">TEXT</text>
  <g font-family="ui-monospace, Menlo, monospace" font-size="17" fill="#1d1a17">
    <text class="lazy-anim-token" style="--i: 0" x="30" y="88">double</text>
    <text class="lazy-anim-token" style="--i: 1" x="98" y="88">x</text>
    <text class="lazy-anim-token" style="--i: 2" x="118" y="88">=</text>
    <text class="lazy-anim-token" style="--i: 3" x="30" y="122">x</text>
    <text class="lazy-anim-token" style="--i: 4" x="50" y="122">+</text>
    <text class="lazy-anim-token" style="--i: 5" x="70" y="122">x</text>
  </g>
  <!-- The tree -->
  <rect x="262" y="40" width="196" height="140" rx="14" fill="#faf8f3" stroke="#e6e0d6"/>
  <text x="360" y="28" text-anchor="middle" font-family="Inter, system-ui, sans-serif" font-size="12" font-weight="600" letter-spacing="1.5" fill="#6b655d">TREE</text>
  <g stroke="#7d6fa6" stroke-width="2" fill="none">
    <path class="lazy-anim-edge" style="--i: 10" d="M360 78 L316 112"/>
    <path class="lazy-anim-edge" style="--i: 11" d="M360 78 L404 112"/>
    <path class="lazy-anim-edge" style="--i: 12" d="M404 118 L380 152"/>
    <path class="lazy-anim-edge" style="--i: 13" d="M404 118 L428 152"/>
  </g>
  <g font-family="ui-monospace, Menlo, monospace" font-size="13" text-anchor="middle">
    <g class="lazy-anim-node" style="--i: 8">
      <rect x="326" y="58" width="68" height="24" rx="12" fill="#5e5086"/>
      <text x="360" y="75" fill="#ffffff">define</text>
    </g>
    <g class="lazy-anim-node" style="--i: 10">
      <rect x="290" y="104" width="52" height="24" rx="12" fill="#eee9f7"/>
      <text x="316" y="121" fill="#46396b">double</text>
    </g>
    <g class="lazy-anim-node" style="--i: 11">
      <rect x="390" y="104" width="28" height="24" rx="12" fill="#eee9f7"/>
      <text x="404" y="121" fill="#46396b">+</text>
    </g>
    <g class="lazy-anim-node" style="--i: 13">
      <rect x="366" y="146" width="28" height="24" rx="12" fill="#eee9f7"/>
      <text x="380" y="163" fill="#46396b">x</text>
    </g>
    <g class="lazy-anim-node" style="--i: 14">
      <rect x="414" y="146" width="28" height="24" rx="12" fill="#eee9f7"/>
      <text x="428" y="163" fill="#46396b">x</text>
    </g>
  </g>
  <!-- The program -->
  <rect x="512" y="40" width="196" height="140" rx="14" fill="#faf8f3" stroke="#e6e0d6"/>
  <text x="610" y="28" text-anchor="middle" font-family="Inter, system-ui, sans-serif" font-size="12" font-weight="600" letter-spacing="1.5" fill="#6b655d">PROGRAM</text>
  <rect x="560" y="74" width="100" height="72" rx="10" fill="#1d1a17"/>
  <g fill="#f2c14e">
    <circle class="lazy-anim-bit" style="--i: 22" cx="580" cy="94" r="5"/>
    <circle class="lazy-anim-bit" style="--i: 23" cx="600" cy="94" r="5"/>
    <circle class="lazy-anim-bit" style="--i: 24" cx="620" cy="94" r="5"/>
    <circle class="lazy-anim-bit" style="--i: 25" cx="640" cy="94" r="5"/>
    <circle class="lazy-anim-bit" style="--i: 26" cx="580" cy="110" r="5"/>
    <circle class="lazy-anim-bit" style="--i: 27" cx="600" cy="110" r="5"/>
    <circle class="lazy-anim-bit" style="--i: 28" cx="620" cy="110" r="5"/>
    <circle class="lazy-anim-bit" style="--i: 29" cx="640" cy="110" r="5"/>
    <circle class="lazy-anim-bit" style="--i: 30" cx="580" cy="126" r="5"/>
    <circle class="lazy-anim-bit" style="--i: 31" cx="600" cy="126" r="5"/>
    <circle class="lazy-anim-bit" style="--i: 32" cx="620" cy="126" r="5"/>
    <circle class="lazy-anim-bit" style="--i: 33" cx="640" cy="126" r="5"/>
  </g>
  <!-- Arrows -->
  <path class="lazy-anim-arrow" d="M214 110 L256 110" stroke="#7d6fa6" stroke-width="2.5" fill="none" marker-end="url(#arrow)"/>
  <path class="lazy-anim-arrow" d="M464 110 L506 110" stroke="#7d6fa6" stroke-width="2.5" fill="none" marker-end="url(#arrow)"/>
  <text x="360" y="208" text-anchor="middle" font-family="Inter, system-ui, sans-serif" font-size="12" fill="#6b655d">This chapter is about the first arrow. The second arrow takes the rest of the book.</text>
</svg>
</figure>

## Words first

To a computer, a program is one long line of characters.
The letters `d`, `o`, `u`, `b`, `l`, `e` are six characters.
They are not yet a word.

The first thing the compiler does is to read the characters and to group them into words.
A programmer calls these words *tokens*.
`double` is a token. So is `=`, and so is `+`.
A space between two tokens is not a token. It only keeps them apart.

This step is small, and the compiler does it fast.
The interesting part comes next.

## A tree, not a list

Read this sentence: *the cat sleeps on the book*.
You did not read it as six separate words.
You read *the cat* as one thing, *on the book* as another, and *sleeps* as the action that connects them.
The words have a shape.

A program has a shape too.
In `double x = x + x`, the `+` connects two things, and the `=` connects a name to a body.
The compiler finds this shape.
The result is a tree: one node at the top, and smaller pieces below it.
The part of the compiler that does this is the *parser*.

Here is the parser of aihc.
Write a little Haskell on the left, and look at the tree on the right.

<div class="lazy-play" data-command="parse">
<pre>
double x = x + x

greeting = "hello"
</pre>
</div>

<div class="lazy-try" markdown>

Try this

- Add a third line: `big = double (double 10)`. The parentheses become a node of their own.
- Remove the `=` from the first line. The parser tells you where it got lost.
- Write `answer = if True then 1 else 0`. An `if` has exactly three children. Find them.

</div>

## What the tree does not show

The tree on the right hides a few things.
Each node also knows the line and the column where it came from.
The compiler keeps them for error messages, so that a message can point at the right place.
The tree also hides small tags, such as whether a number was written with a hash.
A reader does not need them, so the playground leaves them out.

One more thing is missing, and it is important.
The tree has two nodes that say `x`.
The parser does not know whether they are the same `x`.
It does not know what `+` is, or where `double` came from.
A parser finds the shape of the text, and nothing else.
Finding out who is who is the next job.

<a class="lazy-next" href="../02-who-is-who/"><small>Next chapter</small>Who is who</a>
