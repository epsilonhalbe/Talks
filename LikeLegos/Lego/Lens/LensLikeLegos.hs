{-# LANGUAGE TemplateHaskell #-}

module LikeLego where

import Data.List (intersperse,
                  foldl1')
import Control.Lens

data Color = Black | Red
           | Green | Yellow
           | Blue  | Magenta
           | Cyan  | White
           deriving (Enum)

type Brick = [String]

data Dimension = Dim {_x :: Int, _y :: Int}
makeLenses ''Dimension

data Lego = Lego {_dim :: Dimension, _color :: Color}
makeLenses ''Lego

data STRego = STRego {_top :: String, _mid :: [String], _bot ::String}
makeLenses ''STRego

instance Show Dimension where
  show d = show (d^.x) ++" × "++show (d^.y)

instance Show Lego where
  show lego = show $ strLego lego

instance Show STRego where
  show (STRego t m b) = unlines ([""]++[t]++m++[b])

legos :: [Lego]
legos = [ brick22
        , brick31
        , setColor Black $ turn brick31
        , setX 5 . setColor Blue $ brick22]

brick :: Int -> Int -> Color -> Lego
brick a b = Lego (a × b)

brick31 :: Lego
brick31 = Lego (Dim 3 1) Red

brick22 :: Lego
brick22 = Lego (Dim 2 2) Green

strLego ::  Lego -> STRego
strLego (Lego (Dim a b) col)= STRego _t (replicate b _m) _b
    where _t = fg col (" " ++ replicate (2*a-1) '_' ++" ")
          _b = fg col (" " ++ replicate (2*a-1) '‾' ++" ")
          _m = fg col "│" ++
               bg col (intersperse ' ' (replicate a '◯'))++
               fg col "│"

-- the lens variant
strLego' ::  Lego -> STRego
strLego' lego = STRego t (replicate (lego^.dim.y) m) b
    where t = fg (lego^.color) (" " ++ replicate (2*(lego^.dim.x)-1) '_' ++" ")
          b = fg (lego^.color) (" " ++ replicate (2*(lego^.dim.x)-1) '‾' ++" ")
          m = fg (lego^.color) "│" ++
              bg (lego^.color) (intersperse ' ' (replicate (lego^.dim.x) '◯'))++
              fg (lego^.color) "│"

strLegos :: [Lego] -> [Brick]
strLegos [] = [] -- catches the maximum [] = error case
strLegos ll= let mx = maximum $ map (^.dim.y) ll
             in map (strExt mx) ll

combine :: [Brick] -> Brick
combine [] = []
combine b = foldl1' (<>) b

(<>) :: Brick -> Brick -> Brick
(<>) = zipWith (++)

pprint :: [Lego] -> IO ()
pprint = putStr . unlines . combine . strLegos

strExt :: Int -> Lego -> Brick
strExt n lego@(Lego (Dim α β) _)
    = let STRego t m b = strLego lego
          line   = replicate (2*α+1) ' '
      in [t] ++ m ++ [b] ++
         replicate (n-β) line

strExt' :: Int -> Lego -> Brick
strExt' n lego = let strego = strLego lego
                     line   = replicate (2*(lego^.dim.x)+1) ' '
                 in [strego^.top] ++ strego^.mid ++ [strego^.bot] ++
                    replicate (n-lego^.dim.y) line

fg :: Color -> String -> String
fg c str = "\x1b["++show (30+fromEnum c) ++";1m"++str++reset

bg :: Color -> String -> String
bg c str = "\x1b["++show (40+fromEnum c) ++";1m"++str++reset

reset :: String
reset = "\x1b[0m"

(×) :: Int -> Int -> Dimension
a × b = Dim a b

_turn :: Dimension -> Dimension
_turn (Dim a b) = b × a

turn :: Lego -> Lego
turn (Lego d c) = Lego (_turn d) c

setX ::  Int -> Lego -> Lego
setX n = dim.x .~ n

setY ::  Int -> Lego -> Lego
setY n = dim.y .~ n

setColor :: Color -> Lego -> Lego
setColor c = color .~ c
