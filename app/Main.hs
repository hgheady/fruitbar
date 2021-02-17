{-# LANGUAGE OverloadedStrings   #-}

module Main where

import Lib
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Trans.Resource.Internal
import qualified Data.ByteString.Lazy as BL
import Data.Csv hiding ((.=))
import qualified Data.Text as T
import qualified Data.Vector as V
import Database.Persist
import Database.Persist.Sqlite
import Network.HTTP.Types.Status
import Web.Scotty (scotty)
import qualified Web.Scotty as W


runDB :: MonadUnliftIO m => ReaderT SqlBackend (NoLoggingT (ResourceT m)) a -> m a
runDB f = runSqlite "fruitbar.db" $ f

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
getConsumptionsDB = rawSql (T.concat
  ["SELECT ??, ?? "
  ,"FROM consumption INNER JOIN person "
  ,"ON consumption.person_id=person.id "
  ,"ORDER BY consumption.time"
  ]) []

peopleRes :: [(Entity Person)] -> [Response]
peopleRes es = map (\(Entity _ p) -> PersonRes $ personName p) es

consumptionsRes :: [(Entity Person, Entity Consumption)] -> [Response]
consumptionsRes es = map (\(p, c) ->
  ConsumptionRes (personName p) (consumptionFruit c) (consumptionTime c))
  $ map (\((Entity _ p), (Entity _ c)) -> (p, c)) es

streaksRes :: [(Entity Person, Entity Consumption)] -> [Response]
streaksRes es = streakRes $ streaks $ perDay $ consumptionsRes es

consumptions201 :: Response -> Response
consumptions201 = Consumption201

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
      W.json $ ApiRes $ peopleRes ps
    W.get "/consumptions" $ do
      cs <- liftIO $ runDB getConsumptionsDB
      W.json $ ApiRes $ consumptionsRes cs
    W.get "/consumptions/streaks" $ do
      cs <- liftIO $ runDB getConsumptionsDB
      W.json $ ApiRes $ streaksRes cs
    W.post "/consumptions" $ do
      (ConsumptionPost n b t) <- W.jsonData :: W.ActionM Request
      _ <- liftIO $ runDB (insertParsedConsumption $ Parsed n b t)
      W.status status201
      W.json $ consumptions201 $ ConsumptionRes n b t
