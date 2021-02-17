{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
-- ^ minimal persistent
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE DeriveGeneric              #-}

module Lib
    ( Consumption(..)
    , Request(..)
    , Response(..)
    , Parsed(..)
    , Person(..)
    , PersonId
    , ConsumptionId
    , migrateAll
    , streakRes
    , perDay
    , streaks
    , streak
    ) where

import Data.Aeson
import qualified Data.Csv as C
import Data.Text (Text)
import Data.Time (UTCTime(..), Day)
import Data.Time.Format.ISO8601
import Database.Persist.TH
import GHC.Generics

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person json
    name Text
    UniqueName name
    deriving Show
Consumption json
    personId PersonId
    fruit Text
    time UTCTime
|]

data Request = ConsumptionPost Text Text UTCTime deriving (Show, Generic)

instance FromJSON Request where
  parseJSON (Object v) = v .: "date" >>= \s -> case iso8601ParseM s of
    Just (t :: UTCTime) -> ConsumptionPost
                           <$> v .: "name"
                           <*> v .: "fruit-bar-type"
                           <*> pure t
    Nothing -> fail "Invalid timestamp"
  parseJSON _ = fail "Malformed JSON body"

instance ToJSON Request where
  toEncoding (ConsumptionPost n b t) =
    pairs ("name" .= n <> "bar" .= b <> "time" .= t)

data Response = PersonRes Text
              | ConsumptionRes Text Text UTCTime
              | Consumption201 Response
              | StreakDay (Day, Int)
              | StreakRes [Response]
              | ApiRes [Response]
  deriving (Show, Generic)

instance ToJSON Response where
  toEncoding (PersonRes name) =
    pairs ("name" .= name)

  toEncoding (ConsumptionRes n b t) =
    pairs ("name" .= n <> "bar" .= b <> "time" .= t)

  toEncoding (Consumption201 c) =
    pairs ("created" .= c)

  toEncoding (StreakDay (d, n)) =
    pairs ("date" .= d <> "consumed" .= n)

  toEncoding (StreakRes s) = foldable s

  toEncoding (ApiRes l) =
    pairs ("results" .= l)

data Parsed = Parsed !Text !Text !UTCTime

instance C.FromNamedRecord Parsed where
    parseNamedRecord r = r C..: "date" >>= \s -> case iso8601ParseM s of
      Just (t :: UTCTime) ->
        Parsed
        <$> r C..: "person"
        <*> r C..: "fruit-bar-type"
        <*> pure t
      Nothing -> fail "Invalid timestamp"

streakRes :: [[(Day, Int)]] -> [Response]
streakRes ls = map StreakRes $ map (map StreakDay) ls

perDay :: [Response] -> [(Day,Int)]
perDay all@((ConsumptionRes _ _ time):rest) =
  let day             = utctDay time
      (dayCs, restCs) =
        span (\(ConsumptionRes _ _ t) -> day == (utctDay t)) all
  in (day, (length dayCs)) : perDay restCs
perDay _      = []

streaks :: [(Day, Int)] -> [[(Day, Int)]]
streaks (c@(_,i) : c'@(_,i') : rest) =
  if i < i'
  then let s = concat [ [c, c'], streak i' rest ]
       in s : (streaks $ drop ((length s) -2) rest)
  else streaks (c' : rest)
streaks (c:[]) = []
streaks [] = []

streak :: Int -> [(Day, Int)] -> [(Day, Int)]
streak i (c@(_,i') : rest) =
  if i < i'
  then (c : (streak i' rest))
  else []
streak _ [] = []

