---
title: Help! My Haskell Became Machine Code
hide:
  - toc
---

<p class="lazy-chapter">Chapter 1</p>

# Help! My Haskell Became Machine Code

<p class="lazy-lede">A compiler is a program that reads a program. It reads text that a person wrote, and it writes a program that a machine can run. This chapter gives the full journey in one picture. The next chapters look at each step.</p>

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
  <!-- The program: WebAssembly instructions, built block by block -->
  <rect x="512" y="40" width="196" height="140" rx="14" fill="#faf8f3" stroke="#e6e0d6"/>
  <text x="610" y="28" text-anchor="middle" font-family="Inter, system-ui, sans-serif" font-size="12" font-weight="600" letter-spacing="1.5" fill="#6b655d">PROGRAM</text>
  <g font-family="ui-monospace, Menlo, monospace" font-size="13">
    <g class="lazy-anim-block" style="--i: 24">
      <rect x="528" y="52" width="164" height="21" rx="6" fill="#5e5086"/>
      <text x="538" y="67" fill="#ffffff">func $double</text>
    </g>
    <g class="lazy-anim-block" style="--i: 26">
      <rect x="544" y="77" width="148" height="21" rx="6" fill="#eee9f7"/>
      <text x="554" y="92" fill="#46396b">local.get 0</text>
    </g>
    <g class="lazy-anim-block" style="--i: 22">
      <rect x="544" y="102" width="148" height="21" rx="6" fill="#eee9f7"/>
      <text x="554" y="117" fill="#46396b">local.get 0</text>
    </g>
    <g class="lazy-anim-block" style="--i: 24">
      <rect x="544" y="127" width="148" height="21" rx="6" fill="#f2c14e"/>
      <text x="554" y="142" fill="#1d1a17">i64.add</text>
    </g>
    <g class="lazy-anim-block" style="--i: 26">
      <rect x="528" y="152" width="164" height="21" rx="6" fill="#5e5086"/>
      <text x="538" y="167" fill="#ffffff">end</text>
    </g>
  </g>
  <!-- Arrows -->
  <path class="lazy-anim-arrow" d="M214 110 L256 110" stroke="#7d6fa6" stroke-width="2.5" fill="none" marker-end="url(#arrow)"/>
  <path class="lazy-anim-arrow" d="M464 110 L506 110" stroke="#7d6fa6" stroke-width="2.5" fill="none" marker-end="url(#arrow)"/>
  <text x="360" y="208" text-anchor="middle" font-family="Inter, system-ui, sans-serif" font-size="12" fill="#6b655d">The next chapter is about the first arrow. The second arrow takes the rest of the book.</text>
</svg>
</figure>

## The journey

A machine does not understand Haskell.
It understands small instructions: get a value, add two values, jump to a different instruction.
The compiler changes your Haskell into these instructions.
It does this in a sequence of steps, and each step does one job.

1. The compiler reads the text and finds its shape. The result is a tree.
2. The compiler finds, for each name, where that name was defined.
3. The compiler finds the type of each part of the program.
4. The compiler writes the program again in a small language where nothing is implicit.
5. The compiler writes the instructions that the machine runs.

Each chapter of this guide is about one of these steps.

## Run a program

Here is the full compiler.
Change the Haskell module on the left.
Select **Run** to compile it and see its output on the right.
Select **Stop** to stop the program.

<div class="lazy-play" data-command="compile">
<pre>
module Main where

main :: IO ()
main = putStrLn "Hello, compiler!"
</pre>
</div>

The module must have the name `Main` and a `main` function.
You can use the `Prelude`.
The browser compiles the module and runs the WASI P3 program.
The first run must load the compiler tools and the libraries.

<div class="lazy-try" markdown>

Try this

- Change the text in the quotes. Then select **Run** again.
- Write `main = print (sum [1 .. 100])`. The program adds the numbers from 1 to 100.
- Write `main = putStrLn 42`. A number is not a string, and the compiler tells you.

</div>

<a class="lazy-next" href="../02-text-to-tree/"><small>Next chapter</small>From text to tree</a>
