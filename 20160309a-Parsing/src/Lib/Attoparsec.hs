{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Lib.Attoparsec where

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

import Data.Attoparsec.ByteString
import Control.Applicative ((<|>))

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
    in s1 <|> s2 <|> s3 <|> s4

x = let x1 = do e
                string " )"
                return ()
        x2 = do f
                string "sq)"
                return ()
     in x1 <|> x2

e = a
f = a
a = do string ""
       return ()
