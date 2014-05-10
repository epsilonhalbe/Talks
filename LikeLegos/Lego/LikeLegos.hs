module LikeLegos (Lego(..), Color(..), Dimension(..),brick, pprint, turn, setX, setY, setColor) where

import Data.List (intersperse,
                  foldl1')

data Color = Black | Red     | Green | Yellow
           | Blue  | Magenta | Cyan  | White
           deriving (Enum)

type Brick = [String]
data Dimension = Dim {_x :: Int, y :: Int}
data Lego = Lego {dim :: Dimension, _color :: Color}
data STRego = STRego String [String] String

instance Show Dimension where
  show (Dim α β) = show α ++" × "++show β

instance Show Lego where
  show lego = show $ strLego lego

instance Show STRego where
  show (STRego t m b) = unlines ([""]++[t]++m++[b])

brick :: Int -> Int -> Color -> Lego
brick a b = Lego (a × b)

strLego ::  Lego -> STRego
strLego (Lego (Dim α β) col)= STRego t (replicate β m) b
    where t = fg col (" " ++ replicate (2*α-1) '_' ++" ")
          b = fg col (" " ++ replicate (2*α-1) '‾' ++" ")
          m = fg col "│" ++
              bg col (intersperse ' ' (replicate α '◯'))++
              fg col "│"


strLegos :: [Lego] -> [Brick]
strLegos [] = [] -- catches the maximum [] = error case
strLegos ll= let mx = maximum $ map (y.dim) ll
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
setX n (Lego (Dim _ β) c)= Lego (Dim n β) c

setY ::  Int -> Lego -> Lego
setY n (Lego (Dim α _) c)= Lego (Dim α n) c

setColor :: Color -> Lego -> Lego
setColor c (Lego (Dim α β) _)= Lego (Dim α β) c
