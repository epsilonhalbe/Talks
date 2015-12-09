---
title: Category Theory
subtitle: General abstract nonsense
author: Martin Heuschober
date: "Vienna, Dec. 9<sup>th</sup> 2015,<br />License: <a href='https://creativecommons.org/licenses/by-sa/4.0/'>CC-BY-SA-4.0</a>"
to: revealjs
standalone: true
theme: solarized
mathjax: true
revealjs-url: '../javascript/reveal.js'
css: 'custom.css'
height: "'80%'"
width: "'80%'"
controls: true
progress: true
history: true
center: true
theme: 'solarized'
transition: 'default'
...

Foundation
==========

Math
----

blablabla

Category
========

Definition
----------

A category $\mathcal C$ is

1. a class with members called **Objects** $obj(\mathcal C)$

2. for every two objects $X$, $Y$ we have a set $\mathcal C(A,B)$
with members called **morphisms from $A$ to $B$** such that
$$\mathcal C(A,B)\cap\mathcal C(A',B') \neq \emptyset \Longrightarrow (A,B) = (A',B')$$

3. for all objects $X$, $Y$ and $Z$ we have a map
$$\circ: \mathcal C(Y,Z)\times\mathcal C(X,Y)\rightarrow\mathcal C(X,Z)$$
called **composition** such that the law of associativity
$$f \circ (g \circ h) = (f \circ g) \circ h$$
holds.

--------------------------------------------------------------------------------

~~~ { .diagram width=900 height=600}
ptX = p2 (-1,2)
ptY = p2 ( 1,2)
ptZ = p2 (1,0)

arr = arrowBetween' (with & arrowHead .~ dart
                              & headGap .~ large
                              & tailGap .~ large)
spot pt = circle 0.05 # lw none # fc black # moveTo pt
text' str =text str # scale 0.15
example = ptX `arr` ptY
       <> ptY `arr` ptZ
       <> ptX `arr` ptZ
       <> spot ptX
       <> spot ptY
       <> spot ptZ
       <> text' "X"   # moveTo (ptX .+^ r2 (-0.15,0))
       <> text' "Y"   # moveTo (ptY .+^ r2 ( 0.15,0))
       <> text' "Z"   # moveTo (ptZ .+^ r2 (0,-0.25))
       <> text' "f"   # moveTo (p2 (0, 2.15))
       <> text' "g"   # moveTo (p2 (1.15,1))
       <> text' "g∘f" # moveTo (p2 (-0.25,1))
~~~

Examples small
--------------

- every monoid $\mathcal{M}$ with composition being the monoidal $\bullet$ and
  identity given by the identity element of the monoid
- every set $\mathcal{S}$ with composition being good old function composition
  $\circ$ and identity given by the identity function.

Examples medium
---------------

Every set of sets $\mathcal{P}$ with arrows being given by set inclusion
$\subseteq$.

. . .

So for $A\subseteq B$ we have $A\rightarrow B$

. . .

So $A\subseteq B\subseteq C$ we get $A\rightarrow B\rightarrow C$ of course by
transitivity of "$\subseteq$" we get $A\subseteq C$.


Examples BIG
------------

- $\mathcal{Set}$ … the category of all mathematical sets with functions between
  them
- $\mathbb{R}-\mathcal{VectorSpace}$ … the category of all linear spaces over
  the field of real numbers, with arrows being linear functions
- $\mathcal{PO-Sets}$ … the category of partially ordered sets with arrows being
  given by the inclusion


Connecting Categories
=====================

Functor
-------

~~~ { .diagram width=900 height=600}
ptX  = p2 (-1,2)
ptY  = p2 ( 0,1)
ptZ  = p2 ( 1,2)
ptFX = p2 (-1,0)
ptFY = p2 ( 0,-1)
ptFZ = p2 ( 1,0)

arr = arrowBetween' (with & arrowHead .~ dart
                          & headGap   .~ large
                          & tailGap   .~ large)
spot pt = circle 0.05 # lw none # fc black # moveTo pt
text' str =text str # scale 0.15
example = ptX `arr` ptZ
       <> ptX `arr` ptY
       <> ptY `arr` ptZ
       <> ptFX `arr` ptFZ
       <> ptFX `arr` ptFY
       <> ptFY `arr` ptFZ
--     <> ptX  `arr` ptFX
--     <> ptY  `arr` ptFY
--     <> ptZ  `arr` ptFZ
       <> spot ptX
       <> spot ptY
       <> spot ptZ
       <> spot ptFX
       <> spot ptFY
       <> spot ptFZ
       <> text' "X"      # moveTo (ptX  .+^ r2 (-0.15,0))
       <> text' "Y"      # moveTo (ptY  .+^ r2 (0, 0.15))
       <> text' "Z"      # moveTo (ptZ  .+^ r2 ( 0.15,0))
       <> text' "F(X)"   # moveTo (ptFX .+^ r2 (-0.35,0))
       <> text' "F(Y)"   # moveTo (ptFY .+^ r2 (0,-0.25))
       <> text' "F(Z)"   # moveTo (ptFZ .+^ r2 ( 0.35,0))
       <> text' "f"      # moveTo (p2 (-0.75, 1.55))
       <> text' "g"      # moveTo (p2 ( 0.75, 1.55))
       <> text' "g∘f"    # moveTo (p2 ( 0   , 2.15))
       <> text' "F(g∘f)" # moveTo (p2 (0,-0.15))
       <> text' "F(f)"   # moveTo (p2 (-0.75, -0.60))
       <> text' "F(g)"   # moveTo (p2 ( 0.75, -0.60))
       <> text' "F⇓"      # moveTo (p2 (-1.15,1))
       <> text' "⇓F"      # moveTo (p2 ( 1.15,1))
~~~

Functor

Examples
--------

The fundamental group of a topological space
$$\pi_1 : \mathcal{Top}\rightarrow\mathcal{Grp}$$

Examples small
--------------

- every homomorphism between two monoids $\mathcal M$, $\mathcal N$ can be
    viewed as a functor
- thus `length :: [a] -> Int` is a functor

- every type `a` we get `[a]` as a functor
- I think this is the same as the free monoid over a set $\mathcal S$

Examples BIG
------------

- for every (small) category we have the forgetful functor
  $$F : \mathcal C \rightarrow\mathcal{Set}$$
- for every algebraic structure we have a functor from a more specialised into a
  general structure - for example every group is a monoid, therefore we have a
  functor $\mathcal{Grp}\rightarrow\mathcal{Mon}$


Natural Transformation
----------------------

Of course one can make the existing theory a bit more interesting and associate
functors with each other - we call a map between two functors $F,G$ a **natural
transformation**, if for all objects $X$ of $\mathcal C$ we get a morphism $φ_X$,
such that for all morphisms $f: X \rightarrow Y$ the following diagram commutes.

--------------------------------------------------------------------------------

~~~ { .diagram width=900 height=600}
ptFX = p2 (-1,2)
ptFY = p2 ( 1,2)
ptGX = p2 (-1,0)
ptGY = p2 ( 1,0)

arr = arrowBetween' (with & arrowHead .~ dart
                          & headGap   .~ large
                          & tailGap   .~ large)
spot pt = circle 0.05 # lw none # fc black # moveTo pt
text' str =text str # scale 0.15
example = ptFX `arr` ptFY
       <> ptGX `arr` ptGY
       <> ptFX `arr` ptGX
       <> ptFY `arr` ptGY
       <> spot ptFX
       <> spot ptFY
       <> spot ptGX
       <> spot ptGY
       <> text' "F(X)" # moveTo (ptFX .+^ r2 (-0.35,0))
       <> text' "F(Y)" # moveTo (ptFY .+^ r2 ( 0.35,0))
       <> text' "G(X)" # moveTo (ptGX .+^ r2 (-0.35,0))
       <> text' "G(Y)" # moveTo (ptGY .+^ r2 ( 0.35,0))
       <> text' "F(f)" # moveTo (p2 (0, 2.15))
       <> text' "G(f)" # moveTo (p2 (0,-0.25))
       <> text' "φ"    # moveTo (p2 (-1.15,1))
       <> text' "φ"    # moveTo (p2 ( 1.15,1))
~~~

Examples - please
-----------------

- `flatten :: Tree a  -> [a]`
- ??


Concepts
========

Duality
-------

For every category $C$ we have the opposite category $\mathcal C^{op}$, where
the composition is defined as $f \circ^{op} g = g \circ f$, we get it by simply
reversing all arrows. For each 'concept' we thus get a 'concept' in the opposite
category - we call such concepts **dual** and prefix the existing concept with
'co', as for example in *co*functor.

--------------------------------------------------------------------------------

~~~ { .diagram width=900 height=600}
ptX = p2 (-1,2)
ptY = p2 ( 1,2)
ptZ = p2 (1,0)

arr = arrowBetween' (with & arrowHead .~ dart
                              & headGap .~ large
                              & tailGap .~ large)
spot pt = circle 0.05 # lw none # fc black # moveTo pt
text' str =text str # scale 0.15
example = ptY `arr` ptX
       <> ptZ `arr` ptY
       <> ptZ `arr` ptX
       <> spot ptX
       <> spot ptY
       <> spot ptZ
       <> text' "X"   # moveTo (ptX .+^ r2 (-0.15,0))
       <> text' "Y"   # moveTo (ptY .+^ r2 ( 0.15,0))
       <> text' "Z"   # moveTo (ptZ .+^ r2 (0,-0.25))
       <> text' "f"   # moveTo (p2 (0, 2.15))
       <> text' "g"   # moveTo (p2 (1.15,1))
       <> text' "f∘g" # moveTo (p2 (-0.25,1))
~~~

Special Objects
===============

Terminal Objects
----------------

An object $T$ in a category $\mathcal C$ is called **terminal**, if for every
object $X$ in this category we have a unique morphism $f_X : X \rightarrow T$.

--------------------------------------------------------------------------------

~~~ { .diagram width=900 height=600}
ptX1 = p2 (-1,2)
ptdots = p2 ( -1,1)
ptXn = p2 (-1,0)

ptT = p2 (1,1)

arr = arrowBetween' (with & arrowHead .~ dart
                              & headGap .~ large
                              & tailGap .~ large)
spot pt = circle 0.05 # lw none # fc black # moveTo pt
text' str =text str # scale 0.15
example = ptX1 `arr` ptT
       <> (ptdots .+^ r2 (0, -0.5)) `arr` ptT
       <> ptdots `arr` ptT
       <> (ptdots .+^ r2 (0, 0.5)) `arr` ptT
       <> ptXn `arr` ptT
       <> spot ptX1
       <> spot ptT
       <> spot ptXn
       <> text' "X₁" # moveTo (ptX1   .+^ r2 (-0.25,0))
       <> text' "⋮"  # moveTo (ptdots .+^ r2 (0, 0.5))
       <> text' "⋮"  # moveTo ptdots
       <> text' "⋮"  # moveTo (ptdots .+^ r2 (0, -0.5))
       <> text' "Xₙ" # moveTo (ptXn .+^ r2 (-0.25,0))
       <> text' "T"  # moveTo (ptT .+^ r2 (0.25,0))
       <> text' "!f₁" # moveTo (p2 (0,1 + 0.65))
       <> text' "!fₙ" # moveTo (p2 (0,1 - 0.65))
~~~
<small>
Note: The index $n$ should not indicate that there are finitely many objects but
just that there are many.
</small>

Initial Objects
---------------

An object $I$ in a category $\mathcal C$ is called **terminal**, if for every
object $X$ in this category we have a unique morphism $f_X : I \rightarrow X$.

--------------------------------------------------------------------------------

~~~ { .diagram width=900 height=600}
ptX1   = p2 ( 1,2)
ptdots = p2 ( 1,1)
ptXn   = p2 ( 1,0)

ptT = p2 (-1,1)

arr = arrowBetween' (with & arrowHead .~ dart
                              & headGap .~ large
                              & tailGap .~ large)
spot pt = circle 0.05 # lw none # fc black # moveTo pt
text' str =text str # scale 0.15
example = ptT `arr` ptX1
       <> ptT `arr` (ptdots .+^ r2 (0, 0.5))
       <> ptT `arr` ptdots
       <> ptT `arr` (ptdots .+^ r2 (0, -0.5))
       <> ptT `arr` ptXn
       <> spot ptX1
       <> spot ptT
       <> spot ptXn
       <> text' "X₁" # moveTo (ptX1   .+^ r2 ( 0.25,0))
       <> text' "⋮"  # moveTo (ptdots .+^ r2 (0, 0.5))
       <> text' "⋮"  # moveTo ptdots
       <> text' "⋮"  # moveTo (ptdots .+^ r2 (0, -0.5))
       <> text' "Xₙ" # moveTo (ptXn   .+^ r2 ( 0.25,0))
       <> text' "T"  # moveTo (ptT    .+^ r2 (-0.25,0))
       <> text' "!f₁" # moveTo (p2 (0,1 + 0.65))
       <> text' "!fₙ" # moveTo (p2 (0,1 - 0.65))
~~~
<small>
Note: The index $n$ should not indicate that there are finitely many objects but
just that there are many.
</small>

Product Objects
---------------

An object in a category is called **product** of $X_1$ and $X_2$, if it has two
morphisms $pr_1$ and $pr_2$, and for all other objects $Y$ and morphisms
$f_1 : Y \rightarrow X_1$ and $f_2 : Y \rightarrow X_2$ we get a unique map $f$
from $Y$ to this object. We write this object $X_1\times X_2$.

--------------------------------------------------------------------------------

~~~ { .diagram width=900 height=600}
ptY   = p2 ( 0,2)
ptX1  = p2 (-1,0)
ptX2  = p2 ( 1,0)
ptProd = p2 ( 0,0)

arr = arrowBetween' (with & arrowHead .~ dart
                          & headGap   .~ large
                          & tailGap   .~ large)

drr = arrowBetween' (with & arrowHead  .~ dart
                          & headGap    .~ large
                          & tailGap    .~ large
                          & shaftStyle %~ dashingG [0.2,0.05] 0)
spot pt = circle 0.05 # lw none # fc black # moveTo pt
text' str =text str # scale 0.15
example = ptY   `arr` ptX1
       <> ptY   `arr` ptX2
       <> ptProd `arr` ptX1
       <> ptProd `arr` ptX2
       <> ptY   `drr` ptProd
       <> spot ptX1
       <> spot ptX2
       <> spot ptY
       <> spot ptProd
       <> text' "Y"      # moveTo (ptY    .+^ r2 (0, 0.25))
       <> text' "X₁"     # moveTo (ptX1   .+^ r2 (0,-0.25))
       <> text' "X₂"     # moveTo (ptX2   .+^ r2 (0,-0.25))
       <> text' "X₁×X₂"  # moveTo (ptProd .+^ r2 (0,-0.25))
       <> text' "f₁"  # moveTo (ptX1   .+^ r2 ( 0.35,1))
       <> text' "f₂"  # moveTo (ptX2   .+^ r2 (-0.35,1))
       <> text' "!f"  # moveTo (ptProd .+^ r2 ( 0.10,1))
       <> text' "pr₁" # moveTo (ptX1   .+^ r2 ( 0.50, 0.15))
       <> text' "pr₂" # moveTo (ptX2   .+^ r2 (-0.50, 0.15))
~~~

Haskell
-------

- $pr_1$ = `fst`
- $pr_2$ = `snd`

. . .

`import Control.Arrow`

. . .

- `(***) :: Arrow a => a b c -> a b' c' -> a (b,b') (c,c')`

. . .

- $f$ = `f₁ *** f₂`

And with duality
----------------

Sum Objects
-----------

An object in a category is called **coproduct** or **sum** of $X_1$ and $X_2$,
if it has two morphisms $\iota_1$ and $\iota_2$, and for all other objects $Y$
and morphisms $f_1 : X_1 \rightarrow Y$ and $f_2 : X_2 \rightarrow Y$ we get a
unique map $f$ from this object to $Y$. We write this object $X_1⊕X_2$.

--------------------------------------------------------------------------------

~~~ { .diagram width=900 height=600}
ptY   = p2 ( 0,2)
ptX1  = p2 (-1,0)
ptX2  = p2 ( 1,0)
ptSum = p2 ( 0,0)

arr = flip $ arrowBetween' (with & arrowHead .~ dart
                          & headGap   .~ large
                          & tailGap   .~ large)

drr = flip $ arrowBetween' (with & arrowHead  .~ dart
                          & headGap    .~ large
                          & tailGap    .~ large
                          & shaftStyle %~ dashingG [0.2,0.05] 0)
spot pt = circle 0.05 # lw none # fc black # moveTo pt
text' str =text str # scale 0.15
example = ptY   `arr` ptX1
       <> ptY   `arr` ptX2
       <> ptSum `arr` ptX1
       <> ptSum `arr` ptX2
       <> ptY   `drr` ptSum
       <> spot ptX1
       <> spot ptX2
       <> spot ptY
       <> spot ptSum
       <> text' "Y"      # moveTo (ptY    .+^ r2 (0, 0.25))
       <> text' "X₁"     # moveTo (ptX1   .+^ r2 (0,-0.25))
       <> text' "X₂"     # moveTo (ptX2   .+^ r2 (0,-0.25))
       <> text' "X₁⊕X₂"  # moveTo (ptSum  .+^ r2 (0,-0.25))
       <> text' "f₁"  # moveTo (ptX1  .+^ r2 ( 0.35,1))
       <> text' "f₂"  # moveTo (ptX2  .+^ r2 (-0.35,1))
       <> text' "!f"  # moveTo (ptSum .+^ r2 ( 0.10,1))
       <> text' "ɩ₁"  # moveTo (ptX1  .+^ r2 ( 0.50, 0.15))
       <> text' "ɩ₂"  # moveTo (ptX2  .+^ r2 (-0.50, 0.15))
~~~

Haskell
-------

- $\iota_1$ = `Left`
- $\iota_2$ = `Right`

. . .

`import Data.Either`

. . .

- `either :: (a -> c) -> (b -> c) -> (Either a b) -> c`

. . .

- $f$ = `either f₁ f₂`

--------------------------------------------------------------------------------

Note that every sum/product is unique up to isomorphism.

Proof
-----

~~~ { .diagram width=900 height=600}
ptY   = p2 ( 0,1)
ptX1  = p2 (-2,0)
ptX2  = p2 ( 2,0)
ptSum = p2 ( 0,-1)

arr = flip $ arrowBetween' (with & arrowHead .~ dart
                          & headGap   .~ large
                          & tailGap   .~ large)

drr = flip $ arrowBetween' (with & arrowHead  .~ dart
                          & headGap    .~ large
                          & tailGap    .~ large
                          & shaftStyle %~ dashingG [0.02,0.05] 0)
spot pt = circle 0.05 # lw none # fc black # moveTo pt
text' str =text str # scale 0.15
example = ptY   `arr` ptX1
       <> ptY   `arr` ptX2
       <> ptSum `arr` ptX1
       <> ptSum `arr` ptX2
       <> ptY   `drr` ptSum
       <> ptSum `drr` ptY
       <> spot ptX1
       <> spot ptX2
       <> spot ptY
       <> spot ptSum
       <> text' "X₁⊞X₂"  # moveTo (ptY    .+^ r2 (0, 0.25))
       <> text' "X₁"     # moveTo (ptX1   .+^ r2 (0,-0.25))
       <> text' "X₂"     # moveTo (ptX2   .+^ r2 (0,-0.25))
       <> text' "X₁⊕X₂"  # moveTo (ptSum  .+^ r2 (0,-0.25))
       <> text' "!u"  # moveTo (ptSum .+^ r2 ( 0.20,1 + 0.1))
       <> text' "!d"  # moveTo (ptSum .+^ r2 (-0.20,1 - 0.1))
       <> text' "ɩ₁"  # moveTo (ptX1  .+^ r2 ( 0.80, 0.55))
       <> text' "ɩ₂"  # moveTo (ptX2  .+^ r2 (-0.80, 0.55))
       <> text' "ɩ₁"  # moveTo (ptX1  .+^ r2 ( 0.80,-0.55))
       <> text' "ɩ₂"  # moveTo (ptX2  .+^ r2 (-0.80,-0.55))
~~~

`Hask`
======

is not a category
-----------------

why?
----

because of
----------

`undefined`
-----------
see [haskell-wiki](https://wiki.haskell.org/Hask)

