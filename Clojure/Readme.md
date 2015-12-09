%Clojure syntax walkthrough
%Martin Heuschober;;
 [CC-BY-NC-SA 4.0](http://creativecommons.org/licenses/by-nc-sa/4.0/)
%14. Mai 2014

<link rel="stylesheet" href="highlight.js/styles/solarized_light.css">
 <link rel="stylesheet" href="reveal.js/css/reveal.css"/>
<script src="highlight.js/highlight.pack.js"></script>
<script>hljs.initHighlightingOnLoad();;</script>

Clojure
=======

Language features
-----------------

|                |                        |
|----------------|------------------------|
| * a LISP       | * on the JVM           |
| * functional   | * pure                 |
| * lazy         | * concurrency friendly |


Scalars
=======

Booleans
--------

```clojure
user=> true
true
user=> false
false
```

Numbers
-------

```clojure
user=> 4
4;; integral numbers
```

But for floating point numbers be aware of rounding errors.

```clojure
user=> 4.0
4.0
```

But Clojure has rational numbers, which should be the safe choice.

```clojure
user=> 4/9
4/9
```

Strings
-------

```clojure
user=> "some text"
"some text"
```

Characters
----------

```clojure
user=> \a
\a
user=> \newline
\newline
```

Keywords
--------

```clojure
user=> :atom
:atom
```

Nil
---

```clojure
user=> nil
nil
```

Collections
===========

Intro
-----

Lists have a special place in the heart of every lisper as they are the basic
building block of - everything in an old school LISP. But clojure is the new kid
on the block and has a few extra data structures: vectors, (hash)maps and sets.

Lists
-----

```clojure
user=> (function arg1 arg2 arg3 etc)
;;result
```
__Note__: Lines starting with ';' are comments.

In order to avoid any function call you have to quote lists

```clojure
user=> '(1 2 3 4 5)
(1 2 3 4 5)
```

By the way you can use ',' or whitespace to separate list elements

```clojure
user=> '(1 2 3 4 5)
(1 2 3 4 5)
```

--------------------------------------------------------------------------------

Accessing list elements are usually done with unpronouncable functions in old
school LISPs, clojure sticks to a more readable syntax

```clojure
user=> (first '(1 2 3 4 5))
1
user=> (second '(1 2 3 4 5))
2
user=> (last '(1 2 3 4 5))
5
user=> (rest '(1 2 3 4 5))
(2 3 4 5)
user=> (nth '(1 2 3 4 5) 3)
4
```

Vectors
-------

Vectors don't suffer from needing a function in the first position and accessing
an argument at position n, is done in (amortized) constant time.

```clojure
user=> [1 2 3 4 5]
[1 2 3 4 5]
```

Again we can use the `first`, `second`, `last`, `rest` and `nth` to access the
elements of a vector.

And __note__: Both vectors lists and all other collections do __not__ need to
be homogenuous! So constructs like `'(1 "foo" \a)` or `[1 2.0 :three]` are
perfectly valid.

(Hash)Maps
----------

Hashmaps or associative arrays are key value pairs which are usually denoted by

```clojure
user=> {:first-name "Martin", :lastname "Heuschober",
        :nick "ε/2", :age 29}

```

__Note__: usually one uses keywords as keys, but one does not need to!

```clojure
user=> (keys {:a 5 :b 12 :c 13})
(:a :b :c)
user=> (vals {:a 5 :b 12 :c 13})
(5 12 13)
```

--------------------------------------------------------------------------------

One can retrieve the values corresponding to a key by more than one way:

```clojure
user=> (:first-name {:first-name "Martin",
                     :last-name "Heuschober"})
"Martin"
user=> ({:first-name "Martin",
         :last-name "Heuschober"} :first-name)
"Martin"
```

__Note__: `{:a 5, :a 12, :c 13}` is an invalid expression as well as `{:a 5, :b}`.


Sets
----

```clojure
user=> #{1 2 3 4}
#{1 2 3 4}
```

A set may not have any duplicate entries, if constructed by `#{..}`.

Functions
=========

Basics
------

In the section on lists we have already learnt that function application
is done via lists, this solves one of the most common problems in other
programming languages, the order of operator precedence.

So we call any function by putting it in the first positon of a list.

```clojure
user=> (+ 1 2 3 4 5)
15
```

--------------------------------------------------------------------------------

```clojure
user=> (clojure.string/split
        "The following statement is wrong.
         The previous statement is right."
        #" |\n")
```
1. `clojure.string/split` is a fully qualified function

2. `#" |\n"` is a regular expression that matches a whitespace or a newline-character

Defn
----

The `defn` keyword works in a very simple way

```clojure
(defn functionname "docstring"
      ([arg] function-expression)
      ([multiple args] function-expression))
user=> (defn square "guess what, it squares numbers"
             ([x] x*x))
user=> (square -1)
1
```

Fn
--

As we have a functional programming language we also want anonymous functions,
i.e. functions without names, Clojure supports at least two ways to define such
anonymous functions.

```clojure
user=> (fn [x] (* x x))
#<user$eval1$fn__2 user$eval1$fn__2@6be968ce>
user=> (#(* % %) 3)
9
user=> (#(* %1 %2) 3 4)
12
```

Loop .. Recur
-------------

As there are some inconveniences with the JVM we need to explicitly add tail
recursion to prevent stack overflows, and we will use the above `fn` (a lot).

```clojure
user=> (defn fac [n] (if (<= 1 n)
                         1
                         (* n (fac (dec n)))))
;; bad idea
```

--------------------------------------------------------------------------------

```clojure
user=> (defn fac [n]
         (loop [n' n acc 1]
           (if (zero? n')
             acc
             (recur (dec n') (* acc n')))))
;; good idea
```

Higher-order functions
----------------------

Higher-order functions is just a fancy shwancy word for a function that has a
function as argument or result, you say nothing special - and well ... you would
be right ;-).

We have the two prominent `map` and `reduce`, but there is also `filter` and
many more that I have not seen yet as I myself am a noob.

Have fun and keep on hacking
----------------------------
<br>
<div style="text-align: center;">&nbsp;♥♥♥&emsp;&emsp;♥♥♥&nbsp;</div>
<div style="text-align: center;">&nbsp;♥♥♥♥&emsp;♥♥♥♥&nbsp;</div>
<div style="text-align: center;">♥♥♥♥♥♥♥♥♥</div>
<div style="text-align: center;">♥♥&emsp;♥&emsp;♥♥</div>
<div style="text-align: center;">♥&emsp;&emsp;&emsp;♥</div>
<div style="text-align: center;">♥&emsp;♥</div>
<div style="text-align: center;">♥</div>
