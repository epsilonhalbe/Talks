{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Attoparsec.ByteString as A (parseOnly)
import qualified Text.Parsec as P (runP)
import qualified Text.Megaparsec as M

import qualified Lib.Attoparsec as A
import qualified Lib.Parsec as P
import qualified Lib.Megaparsec as M

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [attoparsec
                          , parsec
                          , megaparsec
                          ]

attoparsec :: TestTree
attoparsec = testGroup "Attoparsec Unit tests"
  [ testCase "01 - 'q)'"                              $ A.parseOnly A.s "q)"                              @?= Right ()
  , testCase "01 - 'sq)'"                             $ A.parseOnly A.s "sq)"                             @?= Right ()
  , testCase "02 - '('"                               $ A.parseOnly A.s "("                               @?= Right ()
  , testCase "03 - '( sq)'"                           $ A.parseOnly A.s "( sq)"                           @?= Right ()
  , testCase "04 - '( )'"                             $ A.parseOnly A.s "( )"                             @?= Right ()
  , testCase "05 - '( ) sq)'"                         $ A.parseOnly A.s "( ) sq)"                         @?= Right ()
  , testCase "06 - '( ) ('"                           $ A.parseOnly A.s "( ) ("                           @?= Right ()
  , testCase "07 - '( ) ( sq)'"                       $ A.parseOnly A.s "( ) ( sq)"                       @?= Right ()
  , testCase "08 - '( ) ( )'"                         $ A.parseOnly A.s "( ) ( )"                         @?= Right ()
  , testCase "09 - '( ) ( ) sq)'"                     $ A.parseOnly A.s "( ) ( ) sq)"                     @?= Right ()
  , testCase "10 - '( ) ( ) ('"                       $ A.parseOnly A.s "( ) ( ) ("                       @?= Right ()
  , testCase "11 - '( ) ( ) ( sq)'"                   $ A.parseOnly A.s "( ) ( ) ( sq)"                   @?= Right ()
  , testCase "12 - '( ) ( ) ( )'"                     $ A.parseOnly A.s "( ) ( ) ( )"                     @?= Right ()
  , testCase "13 - '( ) ( ) ( ) sq)'"                 $ A.parseOnly A.s "( ) ( ) ( ) sq)"                 @?= Right ()
  , testCase "14 - '( ) ( ) ( ) ('"                   $ A.parseOnly A.s "( ) ( ) ( ) ("                   @?= Right ()
  , testCase "15 - '( ) ( ) ( ) ( sq)'"               $ A.parseOnly A.s "( ) ( ) ( ) ( sq)"               @?= Right ()
  , testCase "16 - '( ) ( ) ( ) ( )'"                 $ A.parseOnly A.s "( ) ( ) ( ) ( )"                 @?= Right ()
  , testCase "17 - '( ) ( ) ( ) ( ) ('"               $ A.parseOnly A.s "( ) ( ) ( ) ( ) ("               @?= Right ()
  , testCase "18 - '( ) ( ) ( ) ( ) sq)'"             $ A.parseOnly A.s "( ) ( ) ( ) ( ) sq)"             @?= Right ()
  , testCase "19 - '( ) ( ) ( ) ( ) ( sq)'"           $ A.parseOnly A.s "( ) ( ) ( ) ( ) ( sq)"           @?= Right ()
  , testCase "20 - '( ) ( ) ( ) ( ) ( )'"             $ A.parseOnly A.s "( ) ( ) ( ) ( ) ( )"             @?= Right ()
  , testCase "21 - '( ) ( ) ( ) ( ) ( ) sq)'"         $ A.parseOnly A.s "( ) ( ) ( ) ( ) ( ) sq)"         @?= Right ()
  , testCase "22 - '( ) ( ) ( ) ( ) ( ) ('"           $ A.parseOnly A.s "( ) ( ) ( ) ( ) ( ) ("           @?= Right ()
  , testCase "23 - '( ) ( ) ( ) ( ) ( ) ( sq)'"       $ A.parseOnly A.s "( ) ( ) ( ) ( ) ( ) ( sq)"       @?= Right ()
  , testCase "24 - '( ) ( ) ( ) ( ) ( ) ( )'"         $ A.parseOnly A.s "( ) ( ) ( ) ( ) ( ) ( )"         @?= Right ()
  , testCase "25 - '( ) ( ) ( ) ( ) ( ) ( ) sq)'"     $ A.parseOnly A.s "( ) ( ) ( ) ( ) ( ) ( ) sq)"     @?= Right ()
  , testCase "26 - '( ) ( ) ( ) ( ) ( ) ( ) ('"       $ A.parseOnly A.s "( ) ( ) ( ) ( ) ( ) ( ) ("       @?= Right ()
  , testCase "27 - '( ) ( ) ( ) ( ) ( ) ( ) ( sq)'"   $ A.parseOnly A.s "( ) ( ) ( ) ( ) ( ) ( ) ( sq)"   @?= Right ()
  , testCase "28 - '( ) ( ) ( ) ( ) ( ) ( ) ( )'"     $ A.parseOnly A.s "( ) ( ) ( ) ( ) ( ) ( ) ( )"     @?= Right ()
  , testCase "29 - '( ) ( ) ( ) ( ) ( ) ( ) ( ) sq)'" $ A.parseOnly A.s "( ) ( ) ( ) ( ) ( ) ( ) ( ) sq)" @?= Right ()
  , testCase "30 - '( ) ( ) ( ) ( ) ( ) ( ) ( ) ('"   $ A.parseOnly A.s "( ) ( ) ( ) ( ) ( ) ( ) ( ) ("   @?= Right ()
  ]


parsec :: TestTree
parsec = testGroup "parsec Unit tests"
  [ testCase "01 - 'q)'"                              $ P.runP P.s () "" "q)"                              @?= Right ()
  , testCase "00 - 'sq)'"                             $ P.runP P.s () "" "sq)"                             @?= Right ()
  , testCase "02 - '('"                               $ P.runP P.s () "" "("                               @?= Right ()
  , testCase "03 - '( sq)'"                           $ P.runP P.s () "" "( sq)"                           @?= Right ()
  , testCase "04 - '( )'"                             $ P.runP P.s () "" "( )"                             @?= Right ()
  , testCase "05 - '( ) sq)'"                         $ P.runP P.s () "" "( ) sq)"                         @?= Right ()
  , testCase "06 - '( ) ('"                           $ P.runP P.s () "" "( ) ("                           @?= Right ()
  , testCase "07 - '( ) ( sq)'"                       $ P.runP P.s () "" "( ) ( sq)"                       @?= Right ()
  , testCase "08 - '( ) ( )'"                         $ P.runP P.s () "" "( ) ( )"                         @?= Right ()
  , testCase "09 - '( ) ( ) sq)'"                     $ P.runP P.s () "" "( ) ( ) sq)"                     @?= Right ()
  , testCase "10 - '( ) ( ) ('"                       $ P.runP P.s () "" "( ) ( ) ("                       @?= Right ()
  , testCase "11 - '( ) ( ) ( sq)'"                   $ P.runP P.s () "" "( ) ( ) ( sq)"                   @?= Right ()
  , testCase "12 - '( ) ( ) ( )'"                     $ P.runP P.s () "" "( ) ( ) ( )"                     @?= Right ()
  , testCase "13 - '( ) ( ) ( ) sq)'"                 $ P.runP P.s () "" "( ) ( ) ( ) sq)"                 @?= Right ()
  , testCase "14 - '( ) ( ) ( ) ('"                   $ P.runP P.s () "" "( ) ( ) ( ) ("                   @?= Right ()
  , testCase "15 - '( ) ( ) ( ) ( sq)'"               $ P.runP P.s () "" "( ) ( ) ( ) ( sq)"               @?= Right ()
  , testCase "16 - '( ) ( ) ( ) ( )'"                 $ P.runP P.s () "" "( ) ( ) ( ) ( )"                 @?= Right ()
  , testCase "17 - '( ) ( ) ( ) ( ) ('"               $ P.runP P.s () "" "( ) ( ) ( ) ( ) ("               @?= Right ()
  , testCase "18 - '( ) ( ) ( ) ( ) sq)'"             $ P.runP P.s () "" "( ) ( ) ( ) ( ) sq)"             @?= Right ()
  , testCase "19 - '( ) ( ) ( ) ( ) ( sq)'"           $ P.runP P.s () "" "( ) ( ) ( ) ( ) ( sq)"           @?= Right ()
  , testCase "20 - '( ) ( ) ( ) ( ) ( )'"             $ P.runP P.s () "" "( ) ( ) ( ) ( ) ( )"             @?= Right ()
  , testCase "21 - '( ) ( ) ( ) ( ) ( ) sq)'"         $ P.runP P.s () "" "( ) ( ) ( ) ( ) ( ) sq)"         @?= Right ()
  , testCase "22 - '( ) ( ) ( ) ( ) ( ) ('"           $ P.runP P.s () "" "( ) ( ) ( ) ( ) ( ) ("           @?= Right ()
  , testCase "23 - '( ) ( ) ( ) ( ) ( ) ( sq)'"       $ P.runP P.s () "" "( ) ( ) ( ) ( ) ( ) ( sq)"       @?= Right ()
  , testCase "24 - '( ) ( ) ( ) ( ) ( ) ( )'"         $ P.runP P.s () "" "( ) ( ) ( ) ( ) ( ) ( )"         @?= Right ()
  , testCase "25 - '( ) ( ) ( ) ( ) ( ) ( ) sq)'"     $ P.runP P.s () "" "( ) ( ) ( ) ( ) ( ) ( ) sq)"     @?= Right ()
  , testCase "26 - '( ) ( ) ( ) ( ) ( ) ( ) ('"       $ P.runP P.s () "" "( ) ( ) ( ) ( ) ( ) ( ) ("       @?= Right ()
  , testCase "27 - '( ) ( ) ( ) ( ) ( ) ( ) ( sq)'"   $ P.runP P.s () "" "( ) ( ) ( ) ( ) ( ) ( ) ( sq)"   @?= Right ()
  , testCase "28 - '( ) ( ) ( ) ( ) ( ) ( ) ( )'"     $ P.runP P.s () "" "( ) ( ) ( ) ( ) ( ) ( ) ( )"     @?= Right ()
  , testCase "29 - '( ) ( ) ( ) ( ) ( ) ( ) ( ) sq)'" $ P.runP P.s () "" "( ) ( ) ( ) ( ) ( ) ( ) ( ) sq)" @?= Right ()
  , testCase "30 - '( ) ( ) ( ) ( ) ( ) ( ) ( ) ('"   $ P.runP P.s () "" "( ) ( ) ( ) ( ) ( ) ( ) ( ) ("   @?= Right ()
  ]

megaparsec :: TestTree
megaparsec = testGroup "megaparsec Unit tests"
  [ testCase "00 - 'sq)'"                             $ M.parse M.s "" "q)"                              @?= Right ()
  , testCase "01 - 'sq)'"                             $ M.parse M.s "" "sq)"                             @?= Right ()
  , testCase "02 - '('"                               $ M.parse M.s "" "("                               @?= Right ()
  , testCase "03 - '( sq)'"                           $ M.parse M.s "" "( sq)"                           @?= Right ()
  , testCase "04 - '( )'"                             $ M.parse M.s "" "( )"                             @?= Right ()
  , testCase "05 - '( ) sq)'"                         $ M.parse M.s "" "( ) sq)"                         @?= Right ()
  , testCase "06 - '( ) ('"                           $ M.parse M.s "" "( ) ("                           @?= Right ()
  , testCase "07 - '( ) ( sq)'"                       $ M.parse M.s "" "( ) ( sq)"                       @?= Right ()
  , testCase "08 - '( ) ( )'"                         $ M.parse M.s "" "( ) ( )"                         @?= Right ()
  , testCase "09 - '( ) ( ) sq)'"                     $ M.parse M.s "" "( ) ( ) sq)"                     @?= Right ()
  , testCase "10 - '( ) ( ) ('"                       $ M.parse M.s "" "( ) ( ) ("                       @?= Right ()
  , testCase "11 - '( ) ( ) ( sq)'"                   $ M.parse M.s "" "( ) ( ) ( sq)"                   @?= Right ()
  , testCase "12 - '( ) ( ) ( )'"                     $ M.parse M.s "" "( ) ( ) ( )"                     @?= Right ()
  , testCase "13 - '( ) ( ) ( ) sq)'"                 $ M.parse M.s "" "( ) ( ) ( ) sq)"                 @?= Right ()
  , testCase "14 - '( ) ( ) ( ) ('"                   $ M.parse M.s "" "( ) ( ) ( ) ("                   @?= Right ()
  , testCase "15 - '( ) ( ) ( ) ( sq)'"               $ M.parse M.s "" "( ) ( ) ( ) ( sq)"               @?= Right ()
  , testCase "16 - '( ) ( ) ( ) ( )'"                 $ M.parse M.s "" "( ) ( ) ( ) ( )"                 @?= Right ()
  , testCase "17 - '( ) ( ) ( ) ( ) ('"               $ M.parse M.s "" "( ) ( ) ( ) ( ) ("               @?= Right ()
  , testCase "18 - '( ) ( ) ( ) ( ) sq)'"             $ M.parse M.s "" "( ) ( ) ( ) ( ) sq)"             @?= Right ()
  , testCase "19 - '( ) ( ) ( ) ( ) ( sq)'"           $ M.parse M.s "" "( ) ( ) ( ) ( ) ( sq)"           @?= Right ()
  , testCase "20 - '( ) ( ) ( ) ( ) ( )'"             $ M.parse M.s "" "( ) ( ) ( ) ( ) ( )"             @?= Right ()
  , testCase "21 - '( ) ( ) ( ) ( ) ( ) sq)'"         $ M.parse M.s "" "( ) ( ) ( ) ( ) ( ) sq)"         @?= Right ()
  , testCase "22 - '( ) ( ) ( ) ( ) ( ) ('"           $ M.parse M.s "" "( ) ( ) ( ) ( ) ( ) ("           @?= Right ()
  , testCase "23 - '( ) ( ) ( ) ( ) ( ) ( sq)'"       $ M.parse M.s "" "( ) ( ) ( ) ( ) ( ) ( sq)"       @?= Right ()
  , testCase "24 - '( ) ( ) ( ) ( ) ( ) ( )'"         $ M.parse M.s "" "( ) ( ) ( ) ( ) ( ) ( )"         @?= Right ()
  , testCase "25 - '( ) ( ) ( ) ( ) ( ) ( ) sq)'"     $ M.parse M.s "" "( ) ( ) ( ) ( ) ( ) ( ) sq)"     @?= Right ()
  , testCase "26 - '( ) ( ) ( ) ( ) ( ) ( ) ('"       $ M.parse M.s "" "( ) ( ) ( ) ( ) ( ) ( ) ("       @?= Right ()
  , testCase "27 - '( ) ( ) ( ) ( ) ( ) ( ) ( sq)'"   $ M.parse M.s "" "( ) ( ) ( ) ( ) ( ) ( ) ( sq)"   @?= Right ()
  , testCase "28 - '( ) ( ) ( ) ( ) ( ) ( ) ( )'"     $ M.parse M.s "" "( ) ( ) ( ) ( ) ( ) ( ) ( )"     @?= Right ()
  , testCase "29 - '( ) ( ) ( ) ( ) ( ) ( ) ( ) sq)'" $ M.parse M.s "" "( ) ( ) ( ) ( ) ( ) ( ) ( ) sq)" @?= Right ()
  , testCase "30 - '( ) ( ) ( ) ( ) ( ) ( ) ( ) ('"   $ M.parse M.s "" "( ) ( ) ( ) ( ) ( ) ( ) ( ) ("   @?= Right ()
  ]
