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

- `&&`, `||`, `if _ then _ else`, `^` (xor), `~`(complement)

Numbers/Words/Characters
------------------------

Cryptol supports only **nonnegative integers** with no upper bound

- default base 16 (reset with `:set base=n` for 0≤n≤36)
- write with prefixes `0b_`, `0o_`, `0x_` or `0<base>_`

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

And there are accessors, called projections, written `(1,'c').2`, which work
polymorphic for all tuples. But you can also use pattern matching like
`(one, _ ) = (1, "useless")`, where `_` is the discard variable.

Sequences
---------

Lists in Cryptol are the main workhorse syntax is as usual `[1]` or `[1..10]` or
`[1,3..100]` or `[100,97..1]`. Like any other modern language it has list
comprehensions `[x*y| x <- [1..10], y <- [11,12]]` but we also have parallel
comprehensions, which work like a zip.

```haskell
[(i,j)| i <- [1..10]
      | j <- [10,9..1]] = [(1,10),(2,9),(3,8),..,(10,1)]
```

Lists have types too
--------------------

```haskell
[1..10] : [10][64]
```
I am not too happy with that syntax but it means [1..10] is alist of lenght 10
with each element being a 64-bit integer. But this allows for really cool type
level trickery and advanced awsomeness like doing algebra on type level.

Operations on lists
-------------------

We have operators

- `#` = append
- `@` = index
- `@@`= slice
- `!` = reverse index
- `!!`= reverse slice
- `>>` = shift right
- `>>>`= rotate right
- `<<` = shift left
- `<<<`= rotate left

other list functions
--------------------

```haskell
take`{3} [1 .. 12]
drop`{3} [1 .. 12]
split`{3} [1 .. 12]
groupBy`{3} [1 .. 12]
join [[1 .. 4], [5 .. 8], [9 .. 12]]
join [[1, 2, 3], [4, 5, 6], [7, 8, 9], [10, 11, 12]]
transpose [[1, 2, 3, 4], [5, 6, 7, 8]]
transpose [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
```


