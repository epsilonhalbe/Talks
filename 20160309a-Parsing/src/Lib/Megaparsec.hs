{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Lib.Megaparsec where

-- "oth-lr2-0.cfg":
--
-- "S -> ( X
--     | ( ) S
--     | E sq)
--     | F (.
--
-- X -> E )
--    | F sq).
--
-- E -> A.
-- F -> A.
-- A -> .",

import Text.Megaparsec
import Text.Megaparsec.ByteString

s, x, e, f, a :: Parser ()
s = let s1 = do string "("
                x
                return ()
        s2 = do string "( ) "
                s
                return ()
        s3 = do e
                string "sq)"
                return ()
        s4 = do f
                string "("
                return ()
     in try s1 <|> try s2 <|> try s3 <|> try s4

x = let x1 = do e
                string " )"
                return ()
        x2 = do f
                string "sq)"
                return ()
     in try x1 <|> try x2

e = a
f = a
a = do string ""
       return ()
