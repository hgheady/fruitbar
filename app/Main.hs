{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Lib
import Control.Applicative
import Control.Monad (mzero)
import Control.Monad.IO.Class  (liftIO)
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.Time
import Data.Time.Format.ISO8601
import Data.Time (UTCTime)
import qualified Data.Vector as V
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH

-- main :: IO ()
-- main = someFunc

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name String
    deriving Show
ConsumptionDB
    personId PersonId
    meat String
    time UTCTime
|]

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
main = runSqlite ":memory:" $ do
  runMigration migrateAll
  csv <- liftIO $ BL.readFile "data.csv"
  _ <- case decodeByName csv of
    Left err -> liftIO $ putStrLn err
    Right (_, v) -> V.forM_ v $ \ p -> do
      pid <- insert $ Person $ name p
      _   <- insert $ ConsumptionDB pid (bar p) (time p)
      liftIO $ putStrLn "done"
  pure ()
