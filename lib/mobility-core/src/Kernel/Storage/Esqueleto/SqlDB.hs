{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Kernel.Storage.Esqueleto.SqlDB
  ( SqlDBEnv (..),
    SqlDB (..),
    SelectSqlDB (..),
    FullEntitySqlDB,
    liftToFullEntitySqlDB,
    withFullEntity,
    withFullEntities,
    finalize,
  )
where

import Data.Time (UTCTime)
import Database.Esqueleto.Experimental (SqlBackend)
import EulerHS.Prelude
import Kernel.Storage.Esqueleto.Class
import Kernel.Storage.Esqueleto.Logger (LoggerIO)
import Kernel.Types.GuidLike
import Kernel.Types.MonadGuid
import Kernel.Types.Time (MonadTime (..))
import Kernel.Utils.Logging

data SqlDBEnv m = SqlDBEnv
  { currentTime :: UTCTime,
    actions :: m ()
  }

newtype SqlDB k a = SqlDB {unSqlDB :: StateT (SqlDBEnv k) (ReaderT SqlBackend LoggerIO) a}
  deriving newtype (Functor, Applicative, Monad, MonadTime, MonadGuid, Log, MonadThrow, MonadState (SqlDBEnv k), MonadIO)

instance Monad m => MonadTime (StateT (SqlDBEnv k) m) where
  getCurrentTime = gets (.currentTime)

instance MonadGuid (StateT (SqlDBEnv k) (ReaderT SqlBackend LoggerIO)) where
  generateGUIDText = lift $ lift generateGUID

instance Log (StateT (SqlDBEnv k) (ReaderT SqlBackend LoggerIO)) where
  logOutput a b = lift . lift $ logOutput a b
  withLogTag a (StateT f1) = StateT $ \env1 -> do
    let (ReaderT f2) = f1 env1
    ReaderT $ \env2 ->
      withLogTag a $ f2 env2

newtype SelectSqlDB m a = SelectSqlDB {unSelectSqlDB :: SqlDB m a}
  deriving newtype (Functor, Applicative, Monad, MonadTime, MonadGuid, Log, MonadThrow)

newtype FullEntitySqlDB m t = FullEntitySqlDB
  { getSqlDB :: SqlDB m t
  }
  deriving newtype (Functor, Applicative, Monad, MonadTime, MonadGuid)

liftToFullEntitySqlDB :: SqlDB m t -> FullEntitySqlDB m t
liftToFullEntitySqlDB = FullEntitySqlDB

withFullEntity' :: TType t a => a -> (t -> b) -> b
withFullEntity' dtype func = func $ toTType dtype

withFullEntity :: TType t a => a -> (t -> FullEntitySqlDB m b) -> SqlDB m b
withFullEntity dtype func = getSqlDB $ withFullEntity' dtype func

withFullEntities' :: TType t a => [a] -> ([t] -> b) -> b
withFullEntities' [] f = f []
withFullEntities' (x : xs) f =
  withFullEntity' x $ \y ->
    withFullEntities' xs \ys -> f (y : ys)

withFullEntities :: TType t a => [a] -> ([t] -> FullEntitySqlDB m b) -> SqlDB m b
withFullEntities dtypes func = getSqlDB $ withFullEntities' dtypes func

finalize :: forall m. (Monad m) => m () -> SqlDB m ()
finalize someAction = do
  env <- get
  let prevAction = actions env
  put $ env{actions = prevAction >> someAction}
