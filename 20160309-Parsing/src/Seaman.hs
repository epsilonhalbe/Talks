{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Seaman ( Seaman(..)
              , mrd, cpt
              , mrdFail, mrdGood
              , encode, decode
              , eitherDecode)
              where

import           Data.Text (Text)
import           Data.Monoid
import           Data.Aeson
import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Time
import           GHC.Generics
import           System.IO.Unsafe

data Seaman = Captain  { name   :: Text
                       , vessel :: Text
                       , age    :: Int }
            | Mate { name         :: Text
                   , nextDutyTime :: UTCTime }
            deriving (Show, Generic)

instance FromJSON Seaman
instance ToJSON Seaman

cpt, mrd :: Seaman
cpt = Captain "Nemo" "Nautilus" 57
mrd = let now = unsafePerformIO getCurrentTime
          tomorrow = addUTCTime (24*60*60) now
      in Mate "Queequeg" tomorrow

mrdFail, mrdGood :: ByteString
mrdFail = "{\"tg\":\"Mate\"," <>
          "\"nme\":\"Queequeg\","<>
          "\"nextDutyTime\":\"2016-03-10T14:16:27.16408Z\"}"
mrdGood = "{\"tag\":\"Mate\","<>
           "\"name\":\"Queequeg\","<>
           "\"nextDutyTime\":\"2016-03-10T14:16:27.16408Z\"}"
