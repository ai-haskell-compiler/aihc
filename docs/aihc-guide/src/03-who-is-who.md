---
title: Who is who
hide:
  - toc
---

<p class="lazy-chapter">Chapter 3</p>

# Who is who

<p class="lazy-lede">The parse tree is full of names. Two names can look the same and mean different things. Before the compiler can do anything with a program, it must know, for every name, where that name was defined.</p>

## The same letter, twice

Look at this program.

```haskell
double x = x + x

twice f x = f (f x)
```

There are four `x` in it.
The first two belong to `double`.
The other two belong to `twice`.
They are different things that happen to share a letter.
If `double` changed its `x`, `twice` would not notice.

The compiler must know this.
So it walks through the tree, and for each name it finds the place where that name was born.
A programmer calls this step *name resolution*.
In aihc, a component called `aihc-resolve` does it, and it does nothing else.

## Every name gets a color

Here is name resolution, with colors.
Each thing that was defined gets one color.
Every use of that thing gets the same color.
A name with a darker underline is the place where the thing was born.
Hold the mouse over a name to see where it comes from.

<div class="lazy-play" data-command="names">
<pre>
double x = x + x

twice f x = f (f x)

four = twice double 1
</pre>
</div>

Some names come from outside the program.
`+` is one of them.
The reader did not define it, and the compiler finds it in the Prelude, the library that every Haskell program gets for free.
The list under the code says where each name comes from.

<div class="lazy-try" markdown>

Try this

- Rename the `x` of `double` to `n`. Only the colors of `double` change.
- Write `shadow x = (\x -> x) x`. The inner `x` hides the outer one, and gets its own color.
- Write `oops = missing 1`. The compiler does not find `missing`, and says so.

</div>

## Why this comes first

Name resolution looks like a small step, and it is.
But everything after it depends on it.
The next chapter asks what type each name has.
That question only makes sense when the compiler knows which name is which.
A good compiler answers the questions in order, and keeps each answer in its own place.

<a class="lazy-next" href="../04-what-is-what/"><small>Next chapter</small>What is what</a>
