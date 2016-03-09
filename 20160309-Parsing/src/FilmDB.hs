{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

-- taken from https://stackoverflow.com/questions/35893862 answer by @obadz
-- slightly modified

module FilmDB ( Film
              , film
              , testString
              , myEitherParse
              ) where

import           Text.Parsec as P
import           Text.Parsec.String (Parser)

type UserRatings = (String, Int)
type Title = String
type Director = String
type Year = Int
type Film = (Title, Director, Year, [UserRatings])

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

testString :: String
testString = "Blade Runner,Ridley Scott,1982,Amy,5,Bill,8,Ian,7,Kevin,9,Emma,4,Sam,7,Megan,4"

myEitherParse :: Parser a -> String -> String -> Either ParseError a
myEitherParse parser msg input = runParser parser () msg input
