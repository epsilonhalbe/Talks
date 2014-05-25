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

Logging
=======

Functions
---------

State
=====

State
-----

Random
======

RandomGen
---------

Monads
======

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

- [You could've invented monads!](http://blog.sigfpe.com/2006/08/you-could-have-invented-monads-and.html)
- [Monads in javascript](https://blog.jcoglan.com/2011/03/05/translation-from-haskell-to-javascript-of-selected-portions-of-the-best-introduction-to-monads-ive-ever-read/)
- [What a monad is **NOT**](http://www.haskell.org/haskellwiki/What_a_Monad_is_not)
- [haskellcast|Brent Yorgey](http://www.haskellcast.com/episode/005-brent-yorgey-on-diagrams-and-the-typeclassopedia)
- [RWH|Chapter 14. Monads](http://book.realworldhaskell.org/read/monads.html)
- [LYAH|A Fistful of Monads](http://learnyouahaskell.com/a-fistful-of-monads)
