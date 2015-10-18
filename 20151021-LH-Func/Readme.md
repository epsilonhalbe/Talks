% Tricks up the functional sleeve
% by Martin Heuschober
% Vienna, Oct. 20<sup>th</sup> 2015,<br /> License: [CC-BY-SA-4.0][1]

<!-- compile with
> git clone https://github.com/hakimel/reveal.js
> pandoc -t html5 \
         --variable theme=solarized \
         --template=template-revealjs.html
         --self-contained \
         --section-divs \
         -s Readme.md -o index.html
-->

# Nullpointer fun

## Object Orientation

In your favourite OO language you see quite often constructs like

```
if (obj == null) {
  return null;
} elif (obj.member == null){
  return null;
} else {
  obj.member.method();
}
```

## or more concise

```
if (obj != null && obj.member != null){
  return obj.member.method();
}
return null;
```

## what if

One could have a function that captivates the `if null then null` idea in the
`.` operator.

Well usually the `.` operator is a language construct therefore it can't be
overloaded or reinterpreted.

## `(?)`

So let us use a different operator `(?)` for a new "function composition", that
returns `null` in case of `null`-input.

But as class methods are attached to their classes, using `(?)` won't help there.

So it's probably a good idea to separate those functions from their classes.

# Objective-F

## intro

Imagine an OO language, where there are class methods and a keyword `pure` that
indicates a 'non-null' object, but every normal object works as usual, i.e. can
be `null`.

And for every multi argument function can be applied partially, for example

```
add3(1,2,3)  = add3(1,2)(3) = add3(1)(2,3) = add3(1)(2)(3)
```
## back to our `(?)`

```
obj ? func -> obj
```

Remember `obj` could be `null` or a real `object`.

## example

```
public class Point {
  private Integer x;
  private Integer y;
}
```
furthermore imagine the compiler created some accessor functions `x` and `y`.

## type signatures

What type signatures would you imagine `x` or `y` to have?

<div class=fragment>
```
pure Integer x (pure Point p)
```
</div>

## type signatures

So what would it be like using this function `x`?

<div class=fragment>
```
pure Point pt = new Point (pure 1,pure 2);
// our imaginary compiler checks if we lied
Point good    = new Point (1,2);
Point bad     = null;

> pt.x
pure 1
> good?x
1
> bad?x
null
```
</div>

## Do you see some problems?

<div class=fragment>
1. If our language is true OO - functions should be objects too and therefore
   could be null too? Let us denote functions that could be null as `objFunc`.
</div>
<div class=fragment>
2. What happens if we have multi-argument pure functions and many impure inputs?
</div>
<div class=fragment>
3. What happens if we have a function that takes pure input and spits out
   ordinary objects and we want to apply it to impure input?
</div>

# We could quit now!

## You have learnt a cool functional concept

## Still courious?

# Yes

## <span style="color:#FFFFFF;">Functor</span>

Note the first operator is something like `Functor`¹ in Haskell.

 

 

¹: Well not really Objects are equivalent to the so called `Maybe Functor` and
`?` is something like `fmap`, but with arguments reversed

# Still courious??

# More Functional

## <span style="color:#FFFFFF;">Applicative</span>

Problem 1 and 2 actually have the same solution just add another (composition)
operator `(??)`.

```
obj ?? objFunc -> obj
```

## So how should this behave?

```
null ?? func -> null
obj  ?? null -> null
obj  ?? func -> obj
```

## The multi argument problem?!

If we combine the operators `(?)`,`(??)` and partial function application².

```
3??2??(1?add3)
```

It is clear that our OO is kind of weird, OO is already weird in the first place.

 

²: That's where the term applicative came from I presume

## Lift

Also it would be nice to have a function that lifts a pure result to obj-level
such that

```
3??2??(1?add3) == lift (add3(1,2,3))
```


# Still curious ?

# <span style="color:#FFFFFF;">Monad</span>

## Problem 3

## You might have guessed it

## Add Another composition operator

## `(?.?)`

```
obj ?.? (pure -> obj) -> obj
              ↑
              +------ something like func, but maybe with user input
```
## wtf - example please fast

```
> 2 ?.? addToUserInput
//      (user enters 'a')
null
> 2 ?.? addToUserInput
//      (user enters 3)
3
> null ?.? addToUserInput
//         (don't care what user does - but I'll ask)
null
```
## why tf?

- Testing with pure data and think of `fetchFromDB` instead of `addToUserInput`

## Bah you lost me with this third example

- That's why this is bonus material - you have asked for it!


[1]: https://creativecommons.org/licenses/by-sa/4.0/

