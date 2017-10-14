---
title: Haskell
subtitle: a little bit like LEGO<sup>®</sup>
author: Martin Heuschober
date: "London, 15. October 2018,<br />License: <a href='https://creativecommons.org/licenses/by-sa/4.0/'>CC-BY-SA-4.0</a>"
to: revealjs
standalone: true
mathjax: true
revealjs-url: '../reveal.js'
css: ['./custom.css']
height: "'100%'"
width: "'100%'"
controls: true
progress: true
history: true
theme: 'solarized'
transition: 'default'
...

Intro
=====

Why haskell like legos?
-----------------------

During the preparation of this Talk, I was pondering how I can make something as
abstract as haskell to people who might not even have heard of programming.

When thinking about "immutability" I remembered a paragraph in "Sophie's World"
which was comparing the greek atomic model to lego bricks. And starting with
this idea I found more and more parallels between haskell and lego.

Haskell Buzzwords
-----------------

 - pure
 - strongly typed
 - statically typed
 - lazy
 - functional

Haskell is "pure"
==================

--------------------------------------------------------------------------------

Much alike lego bricks - variables in haskell are immutable. If I assign
something to a variable in a haskell program - it is bound and cannot change.

``` haskell
haskell :: String
haskell = "Haskell"
```

will always be the same, no matter what. Same goes for legos - a brick has a
certain size and color - and that's not going to change.

Garbage-Collection
------------------

If I want to change a lego brick, I simply discard the brick put it aside and
search for a new brick in a new color or size. Afterwards the garbage collector
(a.k.a. me) comes and puts everything back into a box.

In haskell every "manipulation" creates a new instance.

``` haskell
Prelude> haskell ++ " yay!"
"Haskell yay!"
```

--------------------------------------------------------------------------------

A consequence of this is that common constructs in other languages will not work
here - `for`-loops that increment a counter in the style of `x++` cannot exist.


How to solve those problems
---------------------------

 - recursion
 - higher order functions
 - local variablen
 - data structures like lists, trees …


Haskell is strongly typed
=========================

---------------

As every lego brick has a fixed length width and color everything haskell has
its type. `Brick (1 × 4) Red` is of type `Lego`, or `Brick (1 × 4) Red :: Lego`.

Those types are checked and enforced by the compiler - where I declare to use a
`Lego` in my source code I cannot use a `Duplo` or a `WoodBrick`

Examples
---------

Here a few examples you can try in the interpreter `ghci` or `winghci.exe` if yo
are using windows. In on a mac or linux just open a terminal window and run `ghci`
after installing haskell.

--------------------------------------------------------------------------------

```haskell
Prelude> let a = 'a'
Prelude> :type a
a :: Char
Prelude> let b = ['T','e','x','t']
Prelude> :t b
b :: [Char]
Prelude> let c = "Text"
Prelude> :t c
c :: [Char]
Prelude> b == c
True
Prelude> :t b == c
b == c :: Bool
```

Haskell is statically typed
===========================

--------------------------------------------------------------------------------

Whereas it is common to expect letters like `'a'` work like numbers and `'a' + 3`
yields `'d'` - haskell throws an error - because the operator `(+)` expects both
its arguments to be of the same (numerical) type.

--------------------------------------------------------------------------------

Same goes for the function `putStrLn`, - it expects a `String` which it then
prints to the command line - there is **no** implicit type conversion involved.

```haskell
Prelude> putStrLn 3
-- lengthy error message
```

Everything has a type
---------------------

This includes functions - if we write a file e.g. `MyFile.hs` we usually include
this - as it is part specification, part documentation and it makes error
messages usually a lot more readable.

```haskell
f :: Int -> Int -> Int
f x y = 2 * x + y
```

 - `f :: Int -> Int -> Int`

     means  `f` takes an `Int` and a second `Int` and gives you an `Int` back.
     the result type is always the last item in such an `->` chain.
 - Brackets are only used where precedence demands it - and function application
   binds tightest.

Define your own types
---------------------

Of course haskell allows you to do that - and you should do that often to make
your code more expressive and more safe at the same time.

. . .

```haskell
data Color = Black | Red     | Green | Yellow
           | Blue  | Magenta | Cyan  | White
data Dimension = Dim {x :: Int, y :: Int}
data Lego = Brick {dim :: Dimension, color :: Color}
```

--------------------------------------------------------------------------------

You cannot cast your own lego bricks - but with 3D printers available - you can
print them!


Haskell is functional
=====================


----------

Functions are no special things in haskell, compared to everything else - they
have a type and you can put them into variables, or lists, or use it
as a parameters or results of other functions.

Funktionen haben keinen besonderen Status, man kann sie wie jeden anderen Typ in
Variablen speichern, in Listen packen oder als Parameter in anderen Funktionen
verwenden.


What is functional about LEGO
-----------------------------

Building plans!

--------------------------------------------------------------------------------

Building plans are the analogue of haskell functions in a lego-world.

If I have a plan and build something according to it, I will get the same result
everytime (unless I run out of resources)

Also if you look closely at those build plans - there are often plans contained
in a plan, for example how to build parts of the wall when constructing a house.


Examples
--------

```haskell
Prelude> let a x = 10*x
Prelude> a 10
100
Prelude> let b = (+)
Prelude> b 1 2
3
Prelude> let c = \x -> x*2
Prelude> c 1
2
Prelude> let d = \x y -> x*y
Prelude> d 2 3
6
```

Haskell is "lazy"
==================

--------------------------------------------------------------------------------

If I build a lego building - I usually do not look for all bricks upfront, but I
search every piece, when I need it. Same goes for talks to give or homework,
you'll probably write them at the last possible moment.

Haskell does the same - if you not say so it will defer work to be done, right
untill you need it, be it for `print`ing or something else.

--------------------------------------------------------------------------------

```haskell
Prelude> let a = 3
Prelude> :sprint a
a = 3
Prelude> let b = [1..10]
Prelude> :sprint b
b = _
Prelude> let c = map (*2) b
Prelude> :sprint c
c = _
Prelude> length c
Prelude> :sprint b
b = [1,2,3,4,5,6,7,8,9,10]
Prelude> :sprint c
c = [_,_,_,_,_,_,_,_,_,_]
```

Here some lazyness in action
----------------------------

```haskell
Prelude> let fib = 1:1:zipWith (+) fib (tail fib)
Prelude> take 10 fib
[1,1,2,3,5,8,13,21,34,55]
```

Back to LEGO
============

GHCi
----

```bash
$> ghci LikeLegos.hs
```


```haskell
let lego = Brick (1×4)
let colors = [Black .. White]
map lego colors
pprint $ map lego colors
pprint $ map lego (colors ++ colors)
pprint $ map lego (colors ++ reverse colors)
pprint $ map lego (colors ++ tail (reverse colors))
let legos = map lego (colors ++ tail (reverse colors))
```

--------------------------------------------------------------------------------

```haskell
pprint $ map (setX 2) $ legos'
pprint $ map (setColor Red . setX 2) legos'
pprint $ zipWith setY [1..] (map (setColor Red) legos')
pprint $ zipWith setY ([1..8]++[7,6..1]) legos'
pprint $ zipWith setY (map (\x -> 9-x) ([1..8]++[7,6..1])) legos'
```

What do I need to play around?
==============================

Compiler
--------

 - Download and install GHC from [haskell.org](https://www.haskell.org)
 - This includes the interpreter `GHCi`

Editors/IDEs
------------

 - (Neo)Vim + Plugins (systastic, hlint, ghc-mod, haskellmode, hdevtools,...)
 - Emacs + Plugins (ghc-mod, intero ...)
 - Eclipse + EclipseFP
 - FPComplete has an online editor
 - atom.io
 - visual studio code
 - leksah

Books
-----

 - [Learn you a haskell for great good](http://learnyouahaskell.com) (short lyah)
 - [Real World Haskell](http://book.realworldhaskell.org/read/)
 - [Parallel and Concurrent Programming in Haskell](http://chimera.labs.oreilly.com/books/1230000000929/index.html)
 - [Haskell and Yesod](http://www.yesodweb.com/)

Blogs & Podcasts
----------------

 - [Reddit](http://www.reddit.com/r/haskell)
 - [Haskellcast](http://www.haskellcast.com/)
 - haskell-cafe - mailinglist
 - go to your local meetup - or create one.

Other Tools
-----------

 - HLint - a really good tool that enhances code quality and helped me alot
   improving my coding style
 - [Hackage](http://hackage.haskell.org/) - the central code-repository
 - [Stackage](http://stackage.org/) - a stable subset of hackage
 - Cabal - the package manager and build tool
 - Stack - the other build tool
 - Hoogle - a search engine where you can search for functions and type signatures
 - Hayoo - another search engine much alike hayoo
 - [stackoverflow/haskell](http://stackoverflow.com/questions/tagged/haskell)
