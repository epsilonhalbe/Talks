module Lego where

import LikeLegos

legos :: [Lego]
legos = [ brick22
        , brick31
        , setColor Black $ turn brick31
        , setX 5 . setColor Blue $ brick22]

brick31 :: Lego
brick31 = Lego (Dim 3 1) Red

brick22 :: Lego
brick22 = Lego (Dim 2 2) Green

