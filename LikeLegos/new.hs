module New where

data Old = New {old :: String, new :: Int}
         | Old String Int
         | EvenOlder String
         deriving (Show)
