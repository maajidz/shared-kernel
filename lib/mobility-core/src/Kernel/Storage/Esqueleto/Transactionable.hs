module Kernel.Storage.Esqueleto.Transactionable where

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

type Transactionable m = Transactionable' SelectSqlDB m

class (MonadThrow m, Log m) => Transactionable' m1 m where
  runTransaction :: m1 a -> m a

instance {-# OVERLAPPING #-} Transactionable' SqlDB SqlDB where
  runTransaction = identity

instance {-# OVERLAPPING #-} Transactionable' SelectSqlDB SqlDB where
  runTransaction = unSelectSqlDB

instance {-# OVERLAPPING #-} Transactionable' SelectSqlDB SelectSqlDB where
  runTransaction = identity

instance {-# INCOHERENT #-} (HasEsqEnv m r, MonadThrow m, Log m) => Transactionable' SelectSqlDB m where
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
instance {-# INCOHERENT #-} (HasEsqEnv m r, MonadThrow m, Log m) => Transactionable' SqlDB m where
  runTransaction m = do
    dbEnv <- asks (.esqDBEnv)
    runTransactionImpl dbEnv m

runTransactionImpl ::
  (HasEsq m r) =>
  EsqDBEnv ->
  SqlDB a ->
  m a
runTransactionImpl dbEnv run = do
  logEnv <- asks (.loggerEnv)
  liftIO $ runTransactionIO logEnv dbEnv run

runTransactionIO :: LoggerEnv -> EsqDBEnv -> SqlDB a -> IO a
runTransactionIO logEnv dbEnv (SqlDB run) = do
  now <- getCurrentTime
  let sqlDBEnv =
        SqlDBEnv
          { currentTime = now
          }
  runLoggerIO logEnv $ runSqlPool (runReaderT run sqlDBEnv) dbEnv.connPool

runInReplica :: (EsqDBReplicaFlow m r, MonadThrow m, Log m) => SelectSqlDB a -> m a
runInReplica (SelectSqlDB m) = do
  dbEnv <- asks (.esqDBReplicaEnv)
  runNoTransactionImpl dbEnv m

runNoTransactionImpl ::
  (HasEsq m r) =>
  EsqDBEnv ->
  SqlDB a ->
  m a
runNoTransactionImpl dbEnv run = do
  logEnv <- asks (.loggerEnv)
  liftIO $ runNoTransactionIO logEnv dbEnv run

runNoTransactionIO :: LoggerEnv -> EsqDBEnv -> SqlDB a -> IO a
runNoTransactionIO logEnv dbEnv (SqlDB run) = do
  now <- getCurrentTime
  let sqlDBEnv =
        SqlDBEnv
          { currentTime = now
          }
  runLoggerIO logEnv $ runSqlPoolNoTransaction (runReaderT run sqlDBEnv) dbEnv.connPool Nothing