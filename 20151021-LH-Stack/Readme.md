% Stack - a haskell build tool
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

# [Stack][2]

## standing on the shoulders of giants

- Built on top of `cabal`
- Built by [fpcomplete][3] and used in-house for quite some time
- Download and install instructions on [gh:commercialhaskell/stack][3]

## Available for

- Windows
- MAC OS X
- Linux in various sorts

# Cabal

## what is wrong

- Cabal builds are not reproducible
- Cabal-Hell is a very real place
- Cabal-sandbox fix some problems

##

- but not all

## personal experience

- building projects consistently/reproducibly
- disk usage of sandboxes

<aside class="notes">
- getting an existing project from my predecessor at work which built on his
  laptop, to build on my laptop took two days and was very messy
- dependency solving got in the way
- sandboxes used in every project take up loads of space
</aside>

# Stack

## What does it differently?

- it uses a "global" db of already built libraries per resolver
- which minimizes disk usage
- and even more importantly (re)building time

## so what is a resolver

It is just a snapshot of packages which work with each other and have an
associated ghc version, they come in two flavours

- `lts-xxx`
- `nightly`

## it downloads and installs

- ghc in the 'right' version (if not already present)
- all necessary packages from stackage
- building happens per default in parallel and sandboxed

## new packages

if you are writing a new package it is just a

```
stack new ${packagename} ${template}
```
away.

## so let's see it in action

demotime

<aside class="notes">
```
git clone -b sqlite git@github.com:epsilonhalbe/snap-api-tutorial.git
cd snap-api-tutorial
mv stack.yaml _stack.yaml
stack init
```

big can't do message with good suggestion at the end

```
stack init --resolver 3.9
```

writes dummy stack.yaml

```
stack build
```

again big can't do message with hint what to put in extra debs

- see new packages getting downloaded from hackage and new directory `.stack-work`
- note that I have already installed plenty of packages - they get not rebuilt or
downloaded

- mention flags, multi-packages
- docker intgration
- file watcher
- stack test/bench/exec
</aside>

## more info, guide

see the [gh-wiki][4]

[1]: https://creativecommons.org/licenses/by-sa/4.0/
[2]: https://github.com/commercialhaskell/stack
[3]: https://www.fpcomplete.com/
[4]: https://github.com/commercialhaskell/stack/wiki

