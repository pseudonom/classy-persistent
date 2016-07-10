{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Prelude

import Control.Monad.Fail (MonadFail)
import qualified Control.Monad.Fail as Fail
import Control.Monad.Logger (runStderrLoggingT, LoggingT)
import Control.Monad.Trans (liftIO, MonadIO, lift)
import Control.Monad.Trans.Reader (ReaderT)
import Data.These (These(..))
import Database.Persist.Sqlite (SqlBackend)
import qualified Database.Persist.Sqlite as Sqlite
import Database.Persist.TH

import Database.Persistent.Classy

share [mkPersist sqlSettings{ mpsGenerateLenses = True }, mkMigrate "migrateAll"] [persistLowerCase|
Test
  up Int Maybe
  down Int Maybe
  deriving Show
|]

data FancyTest
  = FancyTest (These Int Int)
  deriving (Show, Eq)

instance Refined Test FancyTest where
  type Key FancyTest = TestId
  toDB (FancyTest (These a b)) = Test (Just a) (Just b)
  toDB (FancyTest (This a)) = Test (Just a) Nothing
  toDB (FancyTest (That b)) = Test Nothing (Just b)

  fromDB (Test (Just a) (Just b)) = Right $ FancyTest (These a b)
  fromDB (Test (Just a) Nothing) = Right $ FancyTest (This a)
  fromDB (Test Nothing (Just b)) = Right $ FancyTest (That b)
  fromDB (Test Nothing Nothing) = Left $ "Invalid data: `Test` has two `Nothings`"

main :: IO ()
main =
  runStderrLoggingT . Sqlite.withSqliteConn ":memory:" $ \backend ->
    flip Sqlite.runSqlConn backend $ do
      Sqlite.runMigration migrateAll
      _ <- insert $ FancyTest (This 1)
      _ <- Sqlite.insert $ Test Nothing Nothing
      liftIO . print =<< successFetch
      liftIO . print =<< failFetch

successFetch :: (MonadFail m, MonadIO m) => ReaderT SqlBackend m [Entity FancyTest]
successFetch = selectList [TestUp Sqlite.==. Just 1] []

failFetch :: (MonadFail m, MonadIO m) => ReaderT SqlBackend m [Entity FancyTest]
failFetch = selectList [TestUp Sqlite.==. Nothing] []

instance MonadFail (LoggingT IO) where
  fail = lift . Fail.fail
