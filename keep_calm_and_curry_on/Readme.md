---
title: Keep calm and curry on
subtitle: Funktionales Programmieren mit Beispielen in Haskell und Java
author: Martin Heuschober
date: "Wien, 28. Juni 2017,<br />License: <a href='https://creativecommons.org/licenses/by-sa/4.0/'>CC-BY-SA-4.0</a>"
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

Überblick
---------

![Tiobe - Programmiersprachen]

![Tiobe - Haskell]

<aside class="notes">
Welche Sprachen haben "funktionale Features"?
Welche Sprachen haben ein starkes Typsystem?
</aside>

Timeline
--------

![Timeline]

Syntax & Features
-----------------

+---------------------------------------------------+---------------------------------------------+
| ```haskell                                        | ```java                                     |
| main :: IO ()                                     | public class HelloWorld {                   |
| main = putStrLn "Hello Haskell"                   |   public static void Main (String[] args) { |
| ```                                               |     System.out.printLn("Hello Java!");      |
|                                                   |   }                                         |
|                                                   | }                                           |
|                                                   | ```                                         |
+---------------------------------------------------+---------------------------------------------+
| - Whitespace sensitive                            | - Objektorientiert + Vererbung              |
| - Funktionsaufrufe ohne ()                        | -`NullPointer`                              |
| - Typ-Signatur (optional) getrennt von Definition | - Riesiges Ökosystem/Libraries Entwickler   |
| - Funktionen sind "first level citizens"          | - IDEs                                      |
| - Starkes Typsystem + Typinferenz                 | - bisschen Funktional (seit Java8)          |
+---------------------------------------------------+---------------------------------------------+

Funktionales Programmieren
==========================

Was ist funktional?
-------------------

Erst einmal ein Beispiel

+------------------------------------------------------------+--------------------------------------------------+
| ```haskell                                                 | ```java                                          |
| module Card where (sortBy, on, module Card)                | public enum Suit {                               |
| import Data.List                                           |   Heart , Clubs , Diamond , Spades               |
| import Data.Function                                       | }                                                |
|                                                            |                                                  |
| data Suit = Heart | Clubs | Diamond | Spades               | public enum Rank {                               |
|           deriving (Eq, Show, Ord, Enum)                   |   Ace , King , Queen , Jack , Ten , Nine , Eight,|
|                                                            |   Seven , Six , Five , Four , Three , Two        |
| data Rank = Ace   | King  | Queen | Jack | Ten | Nine      | }                                                |
|           | Eight | Seven | Six   | Five | Four| Three     |                                                  |
|           | Two deriving (Eq, Show, Ord, Enum)             | public class Card {                              |
|                                                            |   public Suit suit;                              |
| data Card = Card {suit :: Suit, rank :: Rank}              |   public Rank rank;                              |
|           deriving (Show, Eq)                              |                                                  |
|                                                            |   public class Card(Suit suit, Rank rank) {      |
| allCards :: [Card]                                         |      this.suit = suit;                           |
| allCards = [Card s v | s <- [Heart..], v <- [Ace..]]       |      this.rank = rank;                           |
| ```                                                        |   }                                              |
|                                                            | }                                                |
|                                                            |                                                  |
|                                                            | public List<Card> allCards() {                   |
|                                                            |   List<Card> result = new LinkedList<>();        |
|                                                            |   for (Suit s : Suit.values()) {                 |
|                                                            |     for (Rank r : Rank.values()) {               |
|                                                            |        result.append(Card(r,s));                 |
|                                                            |     }                                            |
|                                                            |   }                                              |
|                                                            |   return result;                                 |
|                                                            | }                                                |
|                                                            | ```                                              |
+------------------------------------------------------------+--------------------------------------------------+

Funktional jetzt aber wirklich
------------------------------

Eine Programmiersprache darf sich (meiner Meinung nach) **funktional** nennen
wenn:

 - Funktionen als Parameter bzw. Rückgabewerte von anderen Funktionen
auftauchen können
 - Funktionen in "Variablen" gesteckt werden können
 - jede Funktion einen Rückgabewert hat.

Bonuspunkte für partielle Funktionsaufrufe, i.e.

 -  wenn eine Funktion mit mehreren Parametern mit nur einem Parameter aufrufbar ist,
    und eine Funktion mit um einen Parameter weniger zurückgibt.

--------------------------------------------------------------------------------

+------------------------------------------------------------+------------------------------------------------------------+
| ```haskell                                                 | ```java                                                    |
| λ> :set -XOverloadedStrings                                |                                                            |
| λ> import qualified Data.Text as T                         |                                                            |
| λ> uppercased = T.toUpper "lowercase"                      | ☕> "lowercase".toUpperCase()                              |
| λ> uppercaser = T.toUpper                                  | ☕> ????                                                   |
| ```                                                        | ```                                                        |
+------------------------------------------------------------+------------------------------------------------------------+

--------------------------------------------------------------------------------

[Aufgabe: (Stackoverflow)](https://stackoverflow.com/questions/43493437/haskell-replicate-function/43494383#43494383) Mache aus einer Liste: `[5,4,2]` => `"+-----+----+--+"`

+--------------------------------------------------------+---------------------------------------------------------------+
|```{.java .fragment}                                    | ```{.haskell .fragment}                                       |
|String fence (List<Integer> lst){                       | fence :: [Int] -> String                                      |
|    String result = "+";                                | fence = between '+' . intercalate "+" . map (`replicate` '-') |
|    for( Integer i : lst) {                             |   where between c str =  c:(str ++[c])                        |
|        for (int j = 0; j < i,j++){                     | ```                                                           |
|          result += "-";                                |                                                               |
|        }                                               |                                                               |
|        result += "+";                                  |                                                               |
|    }                                                   |                                                               |
|    return result;                                      |                                                               |
|}                                                       |                                                               |
|```                                                     |                                                               |
+--------------------------------------------------------+---------------------------------------------------------------+
|```{.java .fragment}                                    | ```{.haskell .fragment}                                       |
|String fence (List<Integer> lst){                       | fence :: [Int] -> String                                      |
|    String result = "+";                                | fence = between '|' . sepBy '+' . fillWith '-'                |
|    for( Integer i : lst) {                             |                                                               |
|        for (int j = 0; j < i,j++){                     | wall :: [Int] -> String                                       |
|          result += "-";                                | wall = between '|' . sepBy '+' . fillWith ' '                 |
|        }                                               |                                                               |
|        result += "+";                                  | fillWith :: Char -> [Int] -> [String]                         |
|    }                                                   | fillWith c ints = map (flip replicate c) ints                 |
|    return result;                                      |                                                               |
|}                                                       | sepBy :: String -> [String] -> String                         |
|                                                        | sepBy i str = intercalate i strs                              |
|String wall (List<Integer> lst){                        |                                                               |
|    String result = "|";                                | between :: Char -> String -> String                           |
|    for( Integer i : lst) {                             | between c str = c:(str++[c])                                  |
|        for (int j = 0; j < i,j++){                     | ```                                                           |
|          result += " ";                                |                                                               |
|        }                                               |                                                               |
|        result += "+";                                  |                                                               |
|    }                                                   |                                                               |
|    return result.subString(0,result.length()-1) + "|"; |                                                               |
|}                                                       |                                                               |
|```                                                     |                                                               |
+--------------------------------------------------------+---------------------------------------------------------------+

--------------------------------------------------------------------------------

+------------------------------------------------------------+---------------------------------------------------------------+
| ```haskell                                                 | ```java                                                       |
| λ> sortBy (compare `on` rank) allCards                     | ☕> Collections.sort(allCards(),new Comparator(){..}          |
| λ> :t sortBy                                               | ☕> typeOf(sortBy)                                            |
| sortBy :: (a -> a -> Ordering) -> [a] -> [a]               | List<T> sortBy (BiFunction<T,T,int> cmp, List<T> in)          |
| λ> :t on                                                   | ☕> typeOf(sortBy)                                            |
| on :: (b -> b -> c) -> (a -> b) -> a -> a -> c             | BiFunction<A,A,C> on (BiFunction<B,B,C> cmp, Function<A,B> f) |
| ```                                                        | ```                                                           |
+------------------------------------------------------------+---------------------------------------------------------------+

Partielle Funktionsaufrufe
--------------------------

+----------------------------------------------------+----------------------------------------------------------------------+
| ```{.haskell .fragment}                            | ```{.java .fragment}                                                 |
| Employee :: Company -> Name -> Employee            | Employee (String company, String name) {..}                          |
| tSystems = Employee "T-Systems"                    | Employee tSystems(String name) = Employee("T-Systems", name)         |
| map tSystems ["Martin Heuschober" ..]              | List<String> employeeNames = new ArrayList<> (["Martin Heuschober"..]) |
| map (Employee "T-Systems") ["Martin Heuschober"..] | employeeNames.map(tSystems)                                          |
| ```                                                | ```                                                                  |
+----------------------------------------------------+----------------------------------------------------------------------+

Higher order functions
======================

`map`
-----
- macht aus einer Liste eine neue Liste

. . .

- Quasi eine eingeschränkte `for`-loop<sup>†</sup>

+-------------------------------+------------------------------------------+
| ```{.haskell .fragment}       | ```{.java .fragment}                     |
| map :: (a -> b) -> [a] -> [b] | foreach (Elmtclass e : lst) {            |
| map _ [] = []                 |   e' = f(e)                              |
| map f (x:xs) = f x : map f xs | }                                        |
| ```                           | ```                                      |
+-------------------------------+------------------------------------------+
|                               | ```{.java .fragment}                     |
|                               | for( int i = 1, i < lst.length(), i++) { |
|                               |   e' = f(lst[i]) + f(lst[i-1])           |
|                               | }                                        |
|                               | ```                                      |
+-------------------------------+------------------------------------------+

<sup>†</sup>: Ich glaube aber fast das gleiche wie eine `foreach`-loop

Aber was bringt uns das?
------------------------

- Parallelisation for "free"
- equational reasoning
- (stream) fusion

`fold`/`reduce`
---------------

+-----------------------------------------+-------------------------------+
| ```{.haskell .fragment}                 | ```{.java .fragment}          |
| fold :: (a -> b -> a) -> a -> [b] -> a  | foreach (Elmtclass e : lst) { |
| fold _ acc [] = acc                     |   acc = f(acc, e);            |
| fold f acc (x:xs) = fold f (f acc x) xs | }                             |
| ```                                     | return acc;                   |
|                                         | ```                           |
+-----------------------------------------+-------------------------------+

`filter`
--------

+------------------------------------------------------------+--------------------------------+
| ```{.haskell .fragment}                                    | ```{.java .fragment}           |
| filter :: (a -> Bool) -> [a] -> [a]                        | foreach (Elmtclass e : lst) {  |
| filter _ [] = []                                           |   if (p(e)) newlist.append(e); |
| filter p (x:xs) = let xs' = filter p xs                    | }                              |
|                    in if p x then x : xs' else xs'         | return newlist;                |
| ```                                                        | ```                            |
+------------------------------------------------------------+--------------------------------+
| ```{.haskell .fragment}                                    |                                |
| filter p = foldr (\y acc -> if p y then y:acc else acc) [] |                                |
| ```                                                        |                                |
+------------------------------------------------------------+--------------------------------+

`all`/`any`
-----------

+---------------------------------------------+-------------------------------+
| ```{.haskell .fragment}                     | ```{.java .fragment}          |
| any :: (a -> Bool) -> [a] -> Bool           | foreach (Elmtclass e : lst) { |
| any _ [] = False                            |   if (p(e)) return true;      |
| any p (x:xs) = if p x then True             | }                             |
|                       else any p xs         | return false;                 |
| ```                                         | ```                           |
+---------------------------------------------+-------------------------------+
| ```{.haskell .fragment}                     | ```{.java .fragment}          |
| all :: (a -> Bool) -> [a] -> Bool           | foreach (Elmtclass e : lst) { |
| all _ [] = True                             |   if (!p(e)) return false;    |
| all p (x:xs) = p x && all p xs              | }                             |
| ```                                         | return true;                  |
|                                             | ```                           |
+---------------------------------------------+-------------------------------+
| ```{.haskell .fragment}                     |                               |
| any p = foldr (\y acc -> p y || acc ) False |                               |
| all p = foldr (\y acc -> p y && acc ) True  |                               |
| ```                                         |                               |
+---------------------------------------------+-------------------------------+
| ```{.haskell .fragment}                     |                               |
| all p = not . all (not . p)                 |                               |
| ```                                         |                               |
+---------------------------------------------+-------------------------------+

Typsystem
=========

Info
----

- Hindley-Milner
- stark, statisch

Mein Problem mit schwachen Typsystemen
--------------------------------------

(Almost) From the documentation of nodejs:
[`fs.openSync(path, flags[, mode])`](https://nodejs.org/api/fs.html#fs_fs_opensync_path_flags_mode)

. . .

 - `path <string> | <Buffer> | <URL>`
 - `flags <string> | <number>`
 - `mode <integer>`

```{.javascript .fragment}
// get the file descriptor of the file to be truncated
const fd = fs.openSync('temp.txt', 'r+');
```

. . .

Synchronous version of fs.open(). Returns an integer representing the file descriptor.

Offene Fragen
-------------

- Welche Flags gibt es?
- Was ist das '+'?
- Welche numerischen Flags gibt es?
- Sind das jetzt auch `<integer>` oder auch Kommazahlen?

. . .

- Aber ok das ist ja nur die Dokumentation - gute Programmierer kümmern sich ja
  gern darum, dass ihre Doku brandaktuell und vollständig ist

Typsysteme sind ja da den Programmierern helfen keine Fehler zu machen
----------------------------------------------------------------------

[`fs.truncate(<integer>, <integer>, <Function>)`](https://nodejs.org/api/fs.html#fs_fs_ftruncate_fd_len_callback)
```{.javascript .fragment}
// truncate the file to first four bytes
fs.ftruncate(4, fd, (err) => {
  assert.ifError(err);
  console.log(fs.readFileSync('temp.txt', 'utf8'));
});
```
Ok das kann ja nicht passieren wenn man die Doku ordentlich liest
-----------------------------------------------------------------

machen ja auch alle - und kopieren nicht einfach Code von Stackoverflow

. . .

[`fs.truncate(fd <integer>, len <integer>, callback <Function>)`](https://nodejs.org/api/fs.html#fs_fs_ftruncate_fd_len_callback)
```{.javascript .fragment}
fs.ftruncate(fd+1,4, (err) => {
  assert.ifError(err);
  console.log(fs.readFileSync('temp.txt', 'utf8'));
});
```

Wozu Typsysteme also?
---------------------

1. Soll unterstützen.
2. Soll so viel Info wie möglich beinalten.
3. Soll so wenig einschränken wie möglich.
4. Soll weitgehend "automatisch" passieren.

Jeder Fehler der zur Compile-time erkannt wird, landet nie bei einem Kunden.

Dinge die das Haskell Typsystem gut macht
=========================================

Und andere Systeme nicht können:
--------------------------------

IO eingrenzen
-------------

```haskell
readLn :: Read a => IO a
fmap readMay . getLine :: Read a => IO (Maybe a)
withSqliteConn :: ConnString -> Query a -> IO a
```

Sum types
---------

```{.haskell .fragment}
data Either a b = Left a | Right b
```

```{.haskell .fragment}
data JSON = JsonString Text
          | JsonNumber Rational
          | JsonBool   Bool
          | JsonNull
          | JsonObject (Map Text JSON)
          | JsonArray [JSON]
```
Pattern matching
----------------

```haskell
data IPv4 = IPv4 Word8 Word8 Word8 Word8
runParser :: Parser a -> String -> Either String a
```

```{.haskell .fragment}
ipv4 :: Parser IPv4
ipv4 = do a <- check =<< decimal
          dot
          b <- check =<< decimal
          dot
          c <- check =<< decimal
          dot
          d <- check =<< decimal
          return IPv4 a b c d
  where dot :: Parser ()
        dot = void $ char '.'
        check :: Integral -> Parser Word8
        check x = do unless (0 <= x && x <= 256) $
                       fail ("failed parsing IPv4 address for " ++ show x)
                     return $ fromIntegral x
```

```{.haskell .fragment}
case runParser ipv4 "127.-1.120.255"
  of Left msg -> do something with errormessage
     Right x -> do something with result
```

Typinferez
----------

```haskell
map :: _
map f [] = []
map f (x:xx) = f x: map f xx
```

--------------------------------------------------------------------------------

`[]` und `(:)` sind Listenkonstruktoren (leere Liste und `cons`-Operator)
  => 2tes Argument von `map` muss eine Liste sein.
```{.haskell .fragment}
    map :: _ -> [a] -> _
```

. . .

ebenso das Ergebnis
```{.haskell .fragment}
    map :: _ -> [a] -> [b]
```

--------------------------------------------------------------------------------

`f` wird in Zeile 2 auf das erste Element der Argument-Liste angewandt =>
damit muss `f` eine Funktion sein
```{.haskell .fragment}
    map :: (x -> y) -> [a] -> [b]
```

. . .

`f` wird in Zeile 2 auf das erste Element der Argument-Liste angewandt =>
damit muss `f` der Wertebereich von `f` `a` sein und der Zielbereich `b`
```{.haskell .fragment}
    map :: (a -> b) -> [a] -> [b]
```


Und man sich klauen kann
------------------------

- Generics
- Arrays vermeiden
- `@NonNullable`/`@Nullable`
- IO minimieren
- mutable State minimieren - z.B. `List.append(x)` verändert eine Liste

[Tiobe - Programmiersprachen]: ./img/tiobe.com-12.6.2017.png "Tiobe - Programmiersprachen" {width=700px}
[Tiobe - Haskell]: ./img/tiobe.com-12.6.2017-haskell.png "Tiobe - Haskell" {width=700px}
