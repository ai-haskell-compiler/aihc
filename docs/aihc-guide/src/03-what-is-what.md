---
title: What is what
hide:
  - toc
---

<p class="lazy-chapter">Chapter 3</p>

# What is what

<p class="lazy-lede">A Haskell program can be written with no types at all. The compiler finds them anyway. This chapter shows how the compiler guesses the type of everything, and how its guesses are always right.</p>

## Everything has a type

`True` is a `Bool`.
`'a'` is a `Char`.
`(True, 'a')` is a pair of the two.
A function that takes a `Bool` and gives a `Char` has the type `Bool -> Char`.

In Haskell, you can write the type of a thing above its definition.
Most programmers do, because a type is a good comment that cannot go stale.
But you do not have to.
When you leave the type out, the compiler works it out from the code.
A programmer calls this *type inference*.
In aihc, the component `aihc-tc` does it.

## Watch the compiler infer

The playground below runs the type checker of aihc on your code.
On the right, it shows the same code with the types it inferred, as signatures above each definition.

<div class="lazy-play" data-command="types">
<pre>
x = ()

double n = n + n

twice f a = f (f a)

pair = (True, "hello")
</pre>
</div>

The signature of `double` says `(Num a) => a -> a`.
Read it as: for any type `a` that is a number, `double` takes an `a` and gives an `a`.
The compiler does not know whether you will use `double` on an `Int` or on a `Double`.
So it does not decide.
It gives the most general type that works.

The signature of `twice` has no `Num` in it.
`twice` never adds anything.
It only calls `f` two times, so `f` must give back the same type that it takes.
That is all the compiler knows about `f`, and that is exactly what the signature says.

<div class="lazy-try" markdown>

Try this

- Write `swap (a, b) = (b, a)`. The compiler finds the two types and swaps them.
- Write `apply f = f 1`. The argument of `f` must be a number, so `Num` appears.
- Write `oops = True + 1`. `True` is not a number. The compiler says what it could not match.

</div>

## The road ahead

Three chapters in, the compiler has read the text, found every name, and worked out every type.
It has not changed the program at all.
That changes in the next chapter.
The compiler rewrites the program into a smaller, stricter language, where nothing is left implicit.

<span class="lazy-next"><small>Next chapter</small>The honest language. Not written yet.</span>
