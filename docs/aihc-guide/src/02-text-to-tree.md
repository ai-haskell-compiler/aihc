---
title: From text to tree
hide:
  - toc
---

<p class="lazy-chapter">Chapter 2</p>

# From text to tree

<p class="lazy-lede">The first step of the compiler is to read the text and find its shape. This chapter shows how text becomes a tree.</p>

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

- Add a third line: `big = double (double 10)`. The parentheses are in the text, but not in the tree.
- Remove the `=` from the first line. The parser tells you where it got lost.
- Write `answer = if True then 1 else 0`. An `if` has exactly three children. Find them.

</div>

## One question left

The tree has two nodes that say `x`.
Are they the same `x`?
The parser cannot tell.
It finds the shape of the text, and nothing else.

<a class="lazy-next" href="../03-who-is-who/"><small>Next chapter</small>Who is who</a>
