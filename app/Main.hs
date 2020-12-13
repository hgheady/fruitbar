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
-- import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import Data.Csv hiding ((.=))
-- import Data.Text hiding (map)
-- import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text hiding (map)
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

data Parsed = Parsed
              { iName :: !Text
              , iBar  :: !Text
              , iTime :: !UTCTime
              }

instance FromNamedRecord Parsed where
    parseNamedRecord r = r .: "date" >>= \s -> case iso8601ParseM s of
      Just (t :: UTCTime) ->
        Parsed
        <$> r .: "person"
        <*> r .: "meat-bar-type"
        <*> pure t
      Nothing -> fail "Invalid timestamp"

runDB :: MonadUnliftIO m => ReaderT SqlBackend (NoLoggingT (ResourceT m)) a -> m a
runDB f = runSqlite "meatbar.db" $ f

insertParsedConsumption :: (MonadIO m, MonadFail m)
                  => Parsed -> ReaderT SqlBackend m (Key Consumption)
insertParsedConsumption (Parsed n b t) = do
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

getPeople :: [(Entity Person)] -> [Response]
getPeople es = map (\(Entity _ p) -> PersonRes $ personName p) es

getConsumptions :: [(Entity Person, Entity Consumption)] -> [Response]
getConsumptions es = map (\(p, c) ->
  ConsumptionRes (personName p) (consumptionMeat c) (consumptionTime c))
  $ map (\((Entity _ p), (Entity _ c)) -> (p, c)) es

main :: IO ()
main = do
  runDB $ runMigration migrateAll

  csv <- liftIO $ BL.readFile "data.csv"
  _ <- case decodeByName csv of
    Left err -> liftIO $ putStrLn err
    Right (_, v) -> V.forM_ v $ \ c -> do
      runDB (insertParsedConsumption c)

  scotty 3000 $ do
    W.get "/people" $ do
      ps <- liftIO $ runDB (selectList [] [])
      W.json $ ApiRes $ getPeople ps
    W.get "/consumptions" $ do
      cs <- liftIO $ runDB getConsumptionsDB
      W.json $ ApiRes $ getConsumptions cs
