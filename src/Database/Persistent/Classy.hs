{-# LANGUAGE UndecidableInstances #-}

module Database.Persistent.Classy where

import Control.Monad.Fail (MonadFail)
import qualified Control.Monad.Fail as Fail
import Control.Monad.Trans (MonadIO)
import qualified Control.Monad.Trans as Trans
import Control.Monad.Trans.Reader (ReaderT)
import Database.Persist (PersistEntityBackend, PersistQuery, PersistEntity, PersistStore)
import qualified Database.Persist as Persist

class Refined a b | a -> b, b -> a where
  type Key b
  toDB :: b -> a
  fromDB :: a -> Either String b

selectList
  ::
    ( Refined val refined, Key refined ~ Persist.Key val
    , PersistEntity val, backend ~ PersistEntityBackend val, PersistQuery backend, MonadIO m, MonadFail m
    )
  =>
    [Persist.Filter val] -> [Persist.SelectOpt val] -> ReaderT backend m [Entity refined]
selectList filters opts = do
  plains <- Persist.selectList filters opts
  Trans.lift . either Fail.fail pure $ mapM (traverse' fromDB) plains

insert
  ::
    (MonadIO m, Refined val refined, PersistEntity val, backend ~ PersistEntityBackend val, PersistStore backend)
  =>
    refined -> ReaderT backend m (Persist.Key val)
insert = Persist.insert . toDB

data Entity a
  = Entity
  { entityKey :: Key a
  , entityVal :: a
  }
deriving instance (Show (Key a), Show a) => Show (Entity a)
deriving instance (Eq (Key a), Eq a) => Eq (Entity a)

traverse' :: (Functor f, Persist.Key a ~ Key b) => (a -> f b) -> Persist.Entity a -> f (Entity b)
traverse' f (Persist.Entity key val) = Entity key <$> f val
