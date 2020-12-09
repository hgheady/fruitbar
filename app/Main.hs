{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Lib
import Control.Applicative
import Control.Monad (mzero)
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.Time
import Data.Time.Format.ISO8601
import Data.Time (UTCTime)
import qualified Data.Vector as V

-- main :: IO ()
-- main = someFunc

data Consumption = Consumption
    { name :: !String
    , bar  :: !String
    , time :: !UTCTime
    }

instance FromNamedRecord Consumption where
    parseNamedRecord r = r .: "date" >>= \s -> case iso8601ParseM s of
      Just (t :: UTCTime) ->
        Consumption
        <$> r .: "person"
        <*> r .: "meat-bar-type"
        <*> pure t
      Nothing -> fail "Invalid timestamp"

main :: IO ()
main = do
    csvData <- BL.readFile "data.csv"
    case decodeByName csvData of
        Left err -> putStrLn err
        Right (_, v) -> V.forM_ v $ \ p ->
            putStrLn $ name p ++ " ate " ++ show (bar p) ++ " on " ++ show (time p)
