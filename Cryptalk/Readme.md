% Cryptol 🔑 A DSL for cryptography
% Martin Heuschober;
  [CC-BY-NC-SA 4.0](http://creativecommons.org/licenses/by-nc-sa/4.0/)
% 11. June 2014

<link rel="stylesheet" href="highlight.js/styles/solarized_light.css">
<link rel="stylesheet" href="reveal.js/css/reveal.css"/>
<script src="highlight.js/highlight.pack.js"></script>
<script>hljs.initHighlightingOnLoad();</script>

Cryptol
=======

Facts & Features
----------------

 - Written by Galois Inc.
 - Written in Haskell \o/
 - Uncommon syntax
 - Strongly typed
 - High Level
 - Abstract
 - Lazy
 - Polymorphic
 - Functional

--------------------------------------------------------------------------------

 - Included SAT-solver
 - Included SMT-solver
 - Interpreter
 - NOT good for production
 - BUT good for prototypes
 - good for learning
 - Open Source
 - ~150 pages book/documentation
. . .
 - Approved by the NSA

Syntax
======

Uncommon doesn't even grasp it
------------------------------

1. Everything has a type
2. Every number has a bit-size
```haskell
 12 : [8]
```
this number `12` is represented as 8-bit number.

There are Bits/Booleans
-----------------------

- True
- False

With the operations

- `&&`, `||`, if … then … else, ^ (xor), ~(complement)

Numbers/Words/Characters
------------------------

Cryptol supports only **nonnegative integers** with no upper bound

- default base 16 (reset with `:set base=n` for 0≤n≤36)
- write with prefixes `0b…`, `0o…`, `0x…` or `0<base>…`

```haskell
> 0<36>cryptol
> 0b1010101
```

But usually one writes characters like 'a','b' and `z`

Tupels
------

```haskell
> ('a',1+3)
> ('a',1,2,3,4)
```

And there are accessors, called projections, written `(1,'c').2`, which work polymorphic for all
tuples. But you can also use pattern matching like `(one, _ ) = (1, "useless")`,
where `_` is the discard variable.

Sequences
---------

Lists in Cryptol are the main workhorse syntax is as usual `[1]` or `[1..10]` or
`[1,3..100]` or `[100,97..1]`. Like any other modern language it has list
comprehensions `[x*y| x <- [1..10], y <- [11,12]]` but we also have parallel
comprehensions, which work like a zip.

```haskell
[(i,j)| i <- [1..10]
      | j <- [10,9..1]] = [(1,10),(2,9),(3,8),…,(10,1)]
```

Operations on lists
-------------------

We have operators `#`, `@`, `@@`, `!` and `!!`; `>>`, `>>>`, `<<`, `<<<`

