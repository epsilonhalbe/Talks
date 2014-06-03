% Monads à la Dan Piponi
% Martin Heuschober;
  [CC-BY-NC-SA 4.0](http://creativecommons.org/licenses/by-nc-sa/4.0/)
% 11. June 2014

<link rel="stylesheet" href="highlight.js/styles/solarized_light.css">
<link rel="stylesheet" href="reveal.js/css/reveal.css"/>
<script src="highlight.js/highlight.pack.js"></script>
<script>hljs.initHighlightingOnLoad();</script>

Typeclasses
===========

a short recap
-------------

Haskell has a quite uniqe way of polymorphism. So what you might know as classes
from object orientation are not equivalent to typeclasses, but rather like
interfaces in Java.

The keyword for creating them is `class`. Though there are mechanisms for
automatically deriving typeclasses you also need to know how to implement them
on your own, with the keyword `instance`.

exempli gratia
--------------

```haskell
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  x /= y = not (x == y
```

or

```haskell
class Functor f where
  fmap :: a -> b -> f a -> f b
```

--------------------------------------------------------------------------------

```haskell
data Color = Red | Green | Blue

instance Eq Color where
  Red   == Red   = True
  Green == Green = True
  Blue  == Blue  = True
  _     == _     = False
```
or more easily

```haskell
data Color = Red | Green | Blue deriving (Eq)
```

Use vs. Implementation
----------------------

So there is but one problem I want to talk about when it comes to typeclasses.
One has to differentiate if between **usage** and **implementation** of a given
typeclass. The former is more common and easier than the latter and before
stepping to the latter, a familiarity with using typeclasses is definitely
recommended.

Side Effects
============

Intro
-----

A common statement about monads is that they make pure programming with side
effects possible - so let us look at a first example of functions that allow to
have debugging informations.

As a simple example I will only consider functions that have signature

```haskell
f,g :: Double -> Double
```

then it is no problem at all to do calculations like $(g . f) x = g (f x)$

Modelling side effects
----------------------

If I want to include debug info, in a pure language, I have to augment the above
signature to something like `f',g' :: Double -> (Double, String)` in order to get
an acompanying debug message.

```haskell
f' x = (f x, "f has been called: "++show (f x) )
g' x = (g x, "g has been called: "++show (g x) )
```

--------------------------------------------------------------------------------

But now we end up with a crappy composition for

1. We want to be able to collect the debug messages with every function call and
2. It is now not obvious how to compose `f'` with `g'` with the dot-operator

So let us define a new function composition and let us denote it `(•)` _the
all-knowing eye sees all debug messages_, but to keep it simple we first try to
solve the task of verarbeiting a (value,message)-pair with a function that only
takes a single `Double` argument.

One ring to `>==` them all
--------------------------

This operation of `f' >== (x,msg)` should be implemented quite easily, in a
first step we want to produce the value and message of `f'` and then 'compose'
the debug messages.

--------------------------------------------------------------------------------

And indeed:

```haskell
(x,msg) >>= f' = let (fx,fmsg) = f' x
```

. . .

```haskell
                 in (fx, msg++"\n"++fmsg)
```

So the next challenge is to write down the `(•)` all-knowing eye operator

Sauron
------

```haskell
(g' • f') x = let (fx , fmsg) = f'x
                  (gfx, gmsg) = g fx
              in  (gfx, fmsg++"\n"++gmsg)
```
or equivalently but shorter

```haskell
(g' • f') x = f' x >>= g'
```

Pure/Unit/Return
----------------

Another nice feature would be if we had a function that 'lifted' a given double
kind of automatically in the debuggable interface - for historical reasons this
function has many names unit/pure/return, which reflect the various settings
in which monads came up and sometimes only make sense in the respective situations.

In this situation I'd like to call this function inject.

```haskell
inject :: Double -> (Double, String)
inject x = (x, "")
```

Lift
----

Going back to our initial setting, note that `f` and `g` have both been
functions that neither produced nor recieved (val, msg)-pairs, so we'd like to
have a formalism to make an `f'` given `f`, or in other words lift `f` into the
monad. Thus we define

```haskell
lift :: (Double -> Double) -> Double -> (Double, String)
(lift f) x = (f x, "")
```

or more abstractly
```haskell
lift f = inject . f
```

Law and Order
-------------

At last in this first chapter we want to observe that every well behaved monad
respects a few laws, that lead to certain simplifications or optimizations.

```haskell
lift f • lift g = lift (f • g)
```

<!--
```haskell
lift f • return = lift f
```
-->

Multivalued Functions
=====================

Intro
-----

Another use-case for monads are functions that have more than one reasonable
choice for results - Dan Piponi uses complex square roots as an example, but
other examples could be a function that chooses which ice-cream flavour you want
to have on your ice-cone. Everyone knows vanilla and chocolate go well with
another, but chocolate and lemon is rarely a good coice. Another example would
be musical chords - not all notes make a harmonic sound. And a last example I
see is all possible moves in a game of chess or go.

Modelling multivalue functions
------------------------------

Staying with Piponi we analyze complex roots of complex values, you might
remember that the equation $x^2 = a$ has exactly two solutions for all $a\neq 0$.
For example the equation
$$
 x^2 = 4
$$
admits both $x = +2$ and $x = -2$ as solutions. For complex numbers this gets a
bit more complicated but mathematicians have solved this problem long time ago
and now we can defne the `root`-functions in haskell as follows.

```haskell
root :: Int -> Complex Double -> [Complex Double]
root n x = let (r,φ) = polar x
               rep = recip $ fromIntegral n
           in [(r**rep:+0) * cis (φ*rep+(2*fromIntegral k*pi*rep))| k <- [0..(n-1)]]
```

Let us look at two special cases: `sqt = root 2` and `cbt = root 3`, if we now
want to compute the 6th root by composing the above functions, we have but one
problem - the types don't match. Of course we could do something like `map` but
still the resulting type is `[[Complex Double]]`.

What's the right solution
-------------------------

We need a function of type `[Complex Double] -> (Complex Double -> [Complex Double]) -> [Complex Double]`
to tie the result of a computation with `sqt` or `cbt`.

```haskell
cplx >>= nroot = concatMap nroot cplx
```

And if we want a plain old `Complex Double`s to work with `(>>=)` we need to pack'em in
brackets so `return`/`unit`/`pure` is defined as:

```haskell
pure z = [z]
```

that was easy, wasn't it? The last pieces `(•)` and `lift`, should be easy as
well

And …
-----

```haskell
lift :: (Complex Double -> Complex Double) -> Complex Double -> [Complex Double]
lift f x = [f x]

(•) :: (Complex Double -> [Complex Double]) -> (Complex Double -> [Complex Double]) -> (Complex Double -> [Complex Double])
(g • f) x = f x >>= g
```

Random Numbers with their generators
====================================

Can also be modeled with the help of monads
-------------------------------------------

So what do mean when we speak of random "numbers"?
In haskell we mean functions of the following type:

```haskell
random :: StdGen -> (a, StdGen)
```

we could hide this in a `data`-declaration

```haskell
data Randomized a = StdGen -> (a, StdGen)
```

Which means given a Standard Random "Number" Generator we get both a random
value **and** a new Generator, you can think of the Generator as a seed.

So if we want some random in functions `f :: a -> b`, `g: b -> c`, we can
view it as a more complex function

```haskell
f̃ :: a -> StdGen -> (b,StdGen)
g̃ :: b -> StdGen -> (c,StdGen)
```

with `Randomized` this would look much nicer 

But how the f\*\*\* does one combine those

Bind to the rescue
------------------

We start small and find out how to cram a `gen -> (value, gen)` into such a `f̃`

```haskell
(>>=) :: (a -> StdGen -> (b,StdGen)) ->(StdGen -> (a, StdGen)) -> (StdGen -> (b,StdGen))
(f̃ >>= rx) seed = let (x,seed') = rx seed
                  in f̃ x seed'
```




Abstract Monads
===============

Intro
-----

Before we are able to define monads we have to do a bit of work

Functor
-------

```haskell
class Functor f where
  fmap :: a -> b -> f a -> f b
```

Applicative
-----------

```haskell
class (Functor f) => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```

Definition
----------

A monad is an applicative functor that has the following additional operation

```haskell
class (Applicative m) => Monad m a where
  (>>=) :: m a -> (a -> m b) -> m b
```


Sources
=======

Links
-----

- [You could've invented monads!](http://blog.sigfpe.com/2006/08/you-could-have-invented-monads-and.html)
- [Monads in javascript](https://blog.jcoglan.com/2011/03/05/translation-from-haskell-to-javascript-of-selected-portions-of-the-best-introduction-to-monads-ive-ever-read/)
- [What a monad is **NOT**](http://www.haskell.org/haskellwiki/What_a_Monad_is_not)
- [haskellcast|Brent Yorgey](http://www.haskellcast.com/episode/005-brent-yorgey-on-diagrams-and-the-typeclassopedia)
- [RWH|Chapter 14. Monads](http://book.realworldhaskell.org/read/monads.html)
- [LYAH|A Fistful of Monads](http://learnyouahaskell.com/a-fistful-of-monads)
