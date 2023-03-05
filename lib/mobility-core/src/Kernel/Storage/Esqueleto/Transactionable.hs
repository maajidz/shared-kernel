{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Storage.Esqueleto.Transactionable where

import Control.Monad.Trans.State.Strict
import Data.Typeable (cast)
import Database.Esqueleto.Experimental (runSqlPool)
import Database.Persist.Postgresql (runSqlPoolNoTransaction)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import Kernel.Storage.Esqueleto.DTypeBuilder
import Kernel.Storage.Esqueleto.Logger
import Kernel.Storage.Esqueleto.SqlDB
import Kernel.Types.Logging
import Kernel.Types.Time (getCurrentTime)
import Kernel.Utils.IOLogging (LoggerEnv)
import Kernel.Utils.Logging (logWarning)

type Transactionable m = Transactionable' SelectSqlDB m

class (MonadThrow m, Log m) => Transactionable' m1 m where
  runTransaction :: m1 a -> m a

instance {-# OVERLAPPING #-} Transactionable' SqlDB SqlDB where
  runTransaction = identity

instance {-# OVERLAPPING #-} Transactionable' SelectSqlDB SqlDB where
  runTransaction = unSelectSqlDB

instance {-# OVERLAPPING #-} Transactionable' SelectSqlDB SelectSqlDB where
  runTransaction = identity

instance {-# INCOHERENT #-} (HasEsqEnv m r, MonadThrow m, Log m, Typeable m) => Transactionable' SelectSqlDB m where
  runTransaction (SelectSqlDB m) = do
    dbEnv <- asks (.esqDBEnv)
    runNoTransactionImpl dbEnv m

instance {-# OVERLAPPING #-} Transactionable' SqlDB m => Transactionable' SqlDB (DTypeBuilder m) where
  runTransaction f = liftToBuilder $ runTransaction f

-- We need INCOHERENT here because in next case:
-- create :: a -> m ()
-- create = runTransaction  . create'
-- compiler cannot figure out which instance to use since in some cases it might be SqlDB
-- and in another it might be not.
-- But with INCOHERENT it will always use Transactionable' m instance.
-- It's fine since Transactionable' SqlDB instance should be used only in find... functions,
-- which have proper Transactionable' instance.
instance {-# INCOHERENT #-} (HasEsqEnv m r, MonadThrow m, Log m, Typeable m) => Transactionable' SqlDB m where
  runTransaction m = do
    dbEnv <- asks (.esqDBEnv)
    (result, SqlDBEnv {actions}) <- runTransactionImpl dbEnv m
    let mbActions = cast @_ @(m ()) actions
    case mbActions of
      Nothing -> do
        logWarning $
          "Couldn't apply finalizer action."
            <> "It caused because action was created in other monad, then monad in which we are trying to apply it."
        pure ()
      Just action -> action
    return result

runTransactionImpl ::
  forall m r a.
  (HasEsq m r, Typeable m) =>
  EsqDBEnv ->
  SqlDB a ->
  m (a, SqlDBEnv)
runTransactionImpl dbEnv run = do
  logEnv <- asks (.loggerEnv)
  liftIO $ runTransactionIO logEnv dbEnv run (Proxy @m)

runTransactionIO :: forall m a. (Typeable m, Monad m) => LoggerEnv -> EsqDBEnv -> SqlDB a -> Proxy m -> IO (a, SqlDBEnv)
runTransactionIO logEnv dbEnv (SqlDB run) _ = do
  now <- getCurrentTime
  let sqlDBEnv =
        SqlDBEnv
          { currentTime = now,
            actions = pure @m ()
          }
  runLoggerIO logEnv $ runSqlPool (runStateT run sqlDBEnv) dbEnv.connPool

runInReplica :: (EsqDBReplicaFlow m r, MonadThrow m, Log m, Typeable m) => SelectSqlDB a -> m a
runInReplica (SelectSqlDB m) = do
  dbEnv <- asks (.esqDBReplicaEnv)
  runNoTransactionImpl dbEnv m

runNoTransactionImpl ::
  forall m r a.
  (HasEsq m r, Typeable m) =>
  (HasEsq m r) =>
  EsqDBEnv ->
  SqlDB a ->
  m a
runNoTransactionImpl dbEnv run = do
  logEnv <- asks (.loggerEnv)
  liftIO $ runNoTransactionIO logEnv dbEnv run (Proxy @m)

runNoTransactionIO :: forall m a. (Typeable m, Monad m) => LoggerEnv -> EsqDBEnv -> SqlDB a -> Proxy m -> IO a
runNoTransactionIO logEnv dbEnv (SqlDB run) _ = do
  now <- getCurrentTime
  let sqlDBEnv =
        SqlDBEnv
          { currentTime = now,
            actions = pure @m ()
          }
  runLoggerIO logEnv $ runSqlPoolNoTransaction (evalStateT run sqlDBEnv) dbEnv.connPool Nothing
