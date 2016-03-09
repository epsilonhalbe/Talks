{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Body ( Body(..)
            , val, cast')
            where

import Data.Bson
import Data.Typeable

data Body = Body { bodyID :: Int
                 , brandID :: Int }
                 deriving (Show,Typeable,Eq)


instance Val Body where
  val (Body body brand) = Doc [ "bodyID"  := (val body)
                              , "brandID" := (val brand)]

--cast' (Doc bson) = case (bson !? "bodyID") of
--            Just (Int64 x) ->
--               case (bson !? "brandID") of
--                     Just (Int64 y) -> Just (Body (fromIntegral x)
--                                  (fromIntegral y))
--                     Just _  -> Nothing
--                     Nothing -> Nothing
--            Just _ -> Nothing
--            Nothing -> Nothing
--cast' (Array _) = undefined
--cast' _ = Nothing

  cast' (Doc bson) = do body  <- (bson !? "bodyID")
                        brand <- (bson !? "brandID")
                        Body <$> cast' body <*> cast' brand

  cast' _ = Nothing
