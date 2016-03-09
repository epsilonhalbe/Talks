---
title: Parsing
subtitle:
author: Martin Heuschober
date: "Vienna, Mar. 9<sup>th</sup> 2015,<br />License: <a href='https://creativecommons.org/licenses/by-sa/4.0/'>CC-BY-SA-4.0</a>"
to: revealjs
standalone: true
mathjax: true
revealjs-url: '../javascript/revealjs'
css: ['custom.css']
height: "'90%'"
width: "'80%'"
controls: true
progress: true
history: true
theme: 'solarized'
transition: 'default'
...

Parsing
=======

TOC
---

- JSON
- BSON
- attoparsec
- parsec

What is parsing?
----------------

The process of transforming a

> `String` ⇒ `value`

where `value` usually means
a custom datatype, describing a problem.

Before we start
---------------

This presentation is accompanied by a haskell module, execute

> stack ghci --ghci-options -XOverloadedStrings

to play around with it

Value
-----

The values we will have a look at in the following chapters will be

Seaman.hs
---------

```haskell
data Seaman = Captain  { name   :: Text
                       , vessel :: Text
                       , age    :: Int }
            | Mate { name         :: Text
                   , nextDutyTime :: UTCTime }
```
for the `aeson` example.


Body.hs
-------

```haskell
data Body = Body { bodyID :: Int
                 , brandID :: Int }
```
for the `bson` example.

Commands.hs
-----------

```haskell
data Command = POST {_message :: Message}
             | READ {_userName :: UserName }
             | FOLLOW {_who :: UserName , _whom :: UserName}
             | WALL {_userName :: UserName}
             deriving (Show, Eq)
```
for the `attoparsec` example

FilmDB.hs
---------

```haskell
type UserRatings = (String, Int)
type Title = String
type Director = String
type Year = Int
type Film = (Title, Director, Year, [UserRatings])
```

Aeson
=====

API
---

```haskell
class ToJSON a where
    toJSON :: a -> Value
    toEncoding :: a -> Encoding
class FromJSON a where
    parseJSON :: Value -> Parser a
```

Using `aeson`
-------------

The simple case

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Seaman where

import           Data.Aeson
import           GHC.Generics
-- import some more

data Seaman = Captain  { name   :: Text
                       , vessel :: Text
                       , age    :: Int }
            | Mate { name         :: Text
                   , nextDutyTime :: UTCTime }
            deriving (Show, Generic)

instance FromJSON Seaman
instance ToJSON Seaman
```

Example
-------

```haskell
λ> encode mrd
λ> eitherDecode mrdFail
```

BSON
====

API
---

```haskell
class (Typeable a, Show a, Eq a) => Val a where
    val :: a -> Value
    ...
    cast' :: Value -> Maybe a
    ...
```

val
---

```haskell
instance Val Body where
  val (Body body brand) = Doc [ "bodyID"  := (Int64 $ fromIntegral body)
                              , "brandID" := (Int64 $ fromIntegral brand)]
```
cast'
-----

```haskell
cast' (Doc bson) = case (bson !? "bodyID") of
            Just (Int64 x) ->
               case (bson !? "brandID") of
                     Just (Int64 y) -> Just (Body (fromIntegral x)
                                  (fromIntegral y))
                     Just _  -> Nothing
                     Nothing -> Nothing
            Just _ -> Nothing
            Nothing -> Nothing
cast' (Array _) = undefined
cast' _ = Nothing
```

--------------------------------------------------------------------------------

or simpler

val + cast'
-----------

```haskell
instance Val Body where
  val (Body body brand) = Doc [ "bodyID"  := (val body)
                              , "brandID" := (val brand)]
  cast' (Doc bson) = do body  <- (bson !? "bodyID")
                        brand <- (bson !? "brandID")
                        Body <$> cast' body <*> cast' brand

  cast' _ = Nothing
```

Example
-------
```haskell
λ> val (Body 30 4)
λ> cast' $ val (Body 30 4) :: Maybe Body
```

Attoparsec
==========

Value
-----

```haskell
data Command = POST {_message :: Message}
             | READ {_userName :: UserName }
             | FOLLOW {_who :: UserName , _whom :: UserName}
             | WALL {_userName :: UserName}
```
from the [social networking kata](https://github.com/sandromancuso/social_networking_kata)

Parsing
-------

```haskell
command :: Parser Command
command = choice $ map (skipSpace >>)[post_, wall_, follow_, read_]
  where post_ :: Parser Command
        post_ = do _author <- T.strip  <$> X.takeWhile (/= '-')
                   string "->"
                   _content <- T.strip <$> takeText
                   return (POST Message{..})
        ..
```
Example
-------

```haskell
λ> parseOnly command ..
```

Parsec
======

Value
-----

```haskell
type UserRatings = (String, Int)
type Title = String
type Director = String
type Year = Int
type Film = (Title, Director, Year, [UserRatings])
```

Helpers
-------

```haskell
str :: Parser String
str = many1 (noneOf ",")

int :: Parser Int
int = read <$> many1 digit

userRating :: Parser UserRatings
userRating = do user <- str
                comma
                rating <- int
                return (user, rating)

comma :: Parser Char
comma = char ','
```

Parsing the Film
----------------

```haskell
film :: Parser Film
film = do title <- str
          comma
          director <- str
          comma
          year <- int
          comma
          ratings <- userRating `sepBy` comma
          eof
          return (title, director, year, ratings)
```

Example
-------

```haskell
λ> myEitherParse film "message string" testString
```

