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
    , Response(..)
    , Parsed(..)
    , Person(..)
    , PersonId
    , ConsumptionId
    , migrateAll
    , perDay
    , streaks
    , streak
    ) where

import Data.Aeson (Encoding, ToJSON(..), pairs, (.=))
import Data.Csv hiding ((.=))
import Data.Maybe (fromJust)
import Data.Text (Text)--hiding (length, span)
import Data.Time (UTCTime(..), Day, fromGregorianValid)
import Data.Time.Format.ISO8601
-- import Database.Persist
-- import Database.Persist.Sql (rawSql)
-- import Database.Persist.Sqlite
import Database.Persist.TH
import GHC.Generics

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person json
    name Text
    UniqueName name
    deriving Show
Consumption json
    personId PersonId
    meat Text
    time UTCTime
|]

data Response = PersonRes { pName :: Text }
              | ConsumptionRes
                { cName :: !Text
                , cBar  :: !Text
                , cTime :: !UTCTime
                }
              | ApiRes { rList :: [Response] }
  deriving (Show, Generic)

instance ToJSON Response where
  toEncoding (PersonRes name) =
    pairs ("name" .= name)

  toEncoding (ConsumptionRes n b t) =
    pairs ("name" .= n <> "bar" .= b <> "time" .= t)

  toEncoding (ApiRes l) =
    pairs ("results" .= l)

data Parsed = Parsed !Text !Text !UTCTime

instance FromNamedRecord Parsed where
    parseNamedRecord r = r .: "date" >>= \s -> case iso8601ParseM s of
      Just (t :: UTCTime) ->
        Parsed
        <$> r .: "person"
        <*> r .: "meat-bar-type"
        <*> pure t
      Nothing -> fail "Invalid timestamp"

perDay :: [Response] -> [(Day,Int)]
perDay all@((ConsumptionRes _ _ time):rest) =
  let day             = utctDay time
      (dayCs, restCs) =
        span (\(ConsumptionRes _ _ t) -> day == (utctDay t)) all
  in concat[[(day, (length dayCs))], perDay restCs]
perDay _      = []

streaks :: [(Day, Int)] -> [[(Day, Int)]]
streaks cs@(c@(_,i):c'@(_,i'):rest) =
  if i < i'
  then let s = (c : c' : streak i' rest)
       in s : (streaks $ drop (length s) rest)
  else streaks (c' : rest)
streaks (c:[]) = []

streak :: Int -> [(Day, Int)] -> [(Day, Int)]
streak i (c@(_,i') : rest) =
  if i < i'
  then (c : (streak i' rest))
  else []

