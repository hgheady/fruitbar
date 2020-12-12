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

-- Generic ToJSON
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

-- import Lib
-- import Control.Monad.IO.Class  (liftIO, MonadIO(..))
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Trans.Resource.Internal-- .ResourceT
import Data.Aeson (ToJSON(..))
import Data.Aeson (pairs, (.=))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import Data.Csv hiding ((.=))
-- import Data.Text hiding (map)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy (Text)
-- import qualified Data.Text.Lazy as T
import Data.Time.Format.ISO8601
import Data.Time (UTCTime)
import qualified Data.Vector as V
-- import qualified Database.Esqueleto as E
-- import Database.Esqueleto      ((^.))
import Database.Persist
-- import Database.Persist.Sql (rawSql)
import Database.Persist.Sqlite
import Database.Persist.TH
import GHC.Generics
import Web.Scotty (scotty)
import qualified Web.Scotty as W
-- import Yesod.Persist.Core (runDB)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person json
    name Text
    UniqueName name
    deriving Show
Consumption json
    personId PersonId
    meat String
    time UTCTime
|]

data PersonDTO = PersonDTO
    { pName :: Text
    } deriving (Show, Generic)

instance ToJSON PersonDTO where
  toEncoding (PersonDTO name) =
    pairs ("name" .= name)

data ConsumptionDTO = ConsumptionDTO
    { cName :: !Text
    , cBar  :: !String
    , cTime :: !UTCTime
    } deriving (Show, Generic)

instance ToJSON ConsumptionDTO where
  toEncoding (ConsumptionDTO n b t) =
    pairs ("name" .= n <> "bar" .= b <> "time" .= t)

instance FromNamedRecord ConsumptionDTO where
    parseNamedRecord r = r .: "date" >>= \s -> case iso8601ParseM s of
      Just (t :: UTCTime) ->
        ConsumptionDTO
        <$> r .: "person"
        <*> r .: "meat-bar-type"
        <*> pure t
      Nothing -> fail "Invalid timestamp"

runDB :: MonadUnliftIO m => ReaderT SqlBackend (NoLoggingT (ResourceT m)) a -> m a
runDB f = runSqlite "meatbar.db" $ f

insertConsumption :: MonadIO m => ConsumptionDTO -> ReaderT SqlBackend m (Key Consumption)
insertConsumption (ConsumptionDTO n b t) = do
  res  <- insertBy $ Person $ n
  pid <- case res of
    Left (Entity dup _) -> pure dup
    Right new           -> pure new
  insert $ Consumption pid b t

getConsumptionsDB :: ReaderT SqlBackend (NoLoggingT (ResourceT IO))
                   [(Entity Person, Entity Consumption)]
getConsumptionsDB = rawSql "SELECT ??, ?? \
  \FROM consumption INNER JOIN person \
  \ON consumption.person_id=person.id"
  []

getPeople :: [(Entity Person)] -> [PersonDTO]
getPeople es = map (\(Entity _ p) -> PersonDTO $ personName p) es

getConsumptions :: [(Entity Person, Entity Consumption)] -> [ConsumptionDTO]
getConsumptions es = map (\(p, c) ->
  ConsumptionDTO (personName p) (consumptionMeat c) (consumptionTime c))
  $ map (\((Entity _ p), (Entity _ c)) -> (p, c)) es

-- data Response = Response
--     { rName :: Text,
--       rList :: [ToJSON]
--     } deriving (Show, Generic)

-- instance ToJSON Response where
--   toEncoding (Response n l) =
--     pairs (n .= l)

-- respond :: Text -> [ToJSON] -> ByteString
-- respond os = 

main :: IO ()
main = do
  runDB $ runMigration migrateAll

  csv <- liftIO $ BL.readFile "data.csv"
  _ <- case decodeByName csv of
    Left err -> liftIO $ putStrLn err
    Right (_, v) -> V.forM_ v $ \ c -> do
      runDB (insertConsumption c)

  scotty 3000 $ do
    W.get "/people" $ do
      ps <- liftIO $ runDB (selectList [] [])
      W.text $ decodeUtf8 $ A.encode $ getPeople ps
    W.get "/consumptions" $ do
      cs <- liftIO $ runDB getConsumptionsDB
      W.text $ decodeUtf8 $ A.encode $ getConsumptions cs
