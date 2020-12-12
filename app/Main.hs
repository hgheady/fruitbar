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
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

-- import Lib
import Control.Monad.IO.Class  (liftIO, MonadIO(..))
import Control.Monad.Trans.Reader (ReaderT(..))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.Text.Lazy as T
import Data.Time.Format.ISO8601
import Data.Time (UTCTime)
import qualified Data.Vector as V
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Web.Scotty (scotty)
import qualified Web.Scotty as W


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person json
    name String
    UniqueName name
    deriving Show
Consumption
    personId PersonId
    meat String
    time UTCTime
|]

data ConsumptionDTO = ConsumptionDTO
    { cdtoName :: !String
    , cdtoBar  :: !String
    , cdtoTime :: !UTCTime
    }

instance FromNamedRecord ConsumptionDTO where
    parseNamedRecord r = r .: "date" >>= \s -> case iso8601ParseM s of
      Just (t :: UTCTime) ->
        ConsumptionDTO
        <$> r .: "person"
        <*> r .: "meat-bar-type"
        <*> pure t
      Nothing -> fail "Invalid timestamp"

insertConsumption :: MonadIO m => ConsumptionDTO -> ReaderT SqlBackend m (Key Consumption)
insertConsumption c = do
  res  <- insertBy $ Person $ cdtoName c
  pid <- case res of
    Left (Entity dup _) -> pure dup
    Right new           -> pure new
  insert $ Consumption pid (cdtoBar c) (cdtoTime c)

main :: IO ()
main = runSqlite ":memory:" $ do
  runMigration migrateAll

  csv <- liftIO $ BL.readFile "data.csv"
  _ <- case decodeByName csv of
    Left err -> liftIO $ putStrLn err
    Right (_, v) -> V.forM_ v $ \ c -> do
      insertConsumption c

  people <- selectList [] []
  liftIO $ print (people :: [Entity Person])

  liftIO $ scotty 3000 $ do
    W.get "/people" $ do
      W.text $ T.concat $ map (decodeUtf8 . A.encode ) (people :: [Entity Person])
