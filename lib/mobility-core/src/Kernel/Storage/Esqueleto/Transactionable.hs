{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Kernel.Storage.Esqueleto.Transactionable
  ( Transactionable,
    Transactionable' (..),
    runTransactionF,
    ignoreFinalize,
    runTransactionIO,
    runInReplica,
  )
where

import Data.Typeable (cast)
import Database.Esqueleto.Experimental (runSqlPool)
import Database.Persist.Postgresql (runSqlPoolNoTransaction)
import EulerHS.Prelude
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import Kernel.Storage.Esqueleto.DTypeBuilder
import Kernel.Storage.Esqueleto.Logger
import Kernel.Storage.Esqueleto.SqlDB
import Kernel.Types.Error
import Kernel.Types.Logging
import Kernel.Types.Time (getCurrentTime)
import Kernel.Utils.Error.Throwing
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

instance {-# INCOHERENT #-} (HasEsqEnv m r, MonadThrow m, Log m, Finalize m) => Transactionable' SelectSqlDB m where
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
instance {-# INCOHERENT #-} (HasEsqEnv m r, MonadThrow m, Log m, Finalize m) => Transactionable' SqlDB m where
  runTransaction m = do
    dbEnv <- asks (.esqDBEnv)
    (a, _env) <- runTransactionImpl dbEnv m
    pure a

-- | Type safe plug for ignoring finalize function
ignoreFinalize :: forall m. Finalize m => (m () -> SqlDB ()) -> (m () -> SqlDB ())
ignoreFinalize _ _ = pure ()

-- This function created for type safety. Finalize function should be applied to the same monad within transaction
runTransactionF :: forall m r a. (HasEsqEnv m r, MonadThrow m, Log m, Finalize m) => ((m () -> SqlDB ()) -> SqlDB a) -> m a
runTransactionF mkAction = do
  dbEnv <- asks (.esqDBEnv)
  let m = mkAction (finalizeImpl @m)
  (result, env) <- runTransactionImpl dbEnv m
  applyFinalizeImpl @m env
  pure result

-- | This function should not be used outside of Transactionable module!
finalizeImpl :: forall m. Finalize m => m () -> SqlDB ()
finalizeImpl someAction = do
  SqlDBEnv {..} <- get
  let mbPrevActions = cast @_ @(m ()) actions
  prevActions <- case mbPrevActions of
    Nothing -> do
      throwError $
        InternalError $
          "Couldn't append finalizer action in \""
            <> monadType someAction
            <> "\"monad. It caused because previous action was created in \""
            <> monadType actions
            <> "\"monad."
    Just a -> pure a
  put $ SqlDBEnv {actions = prevActions >> someAction, currentTime}

-- | This function should not be used outside of Transactionable module!
applyFinalizeImpl :: forall m. (MonadThrow m, Log m, Finalize m) => SqlDBEnv -> m ()
applyFinalizeImpl SqlDBEnv {actions} = do
  let mbActions = cast @_ @(m ()) actions
  case mbActions of
    Nothing -> do
      let emptyAction = pure @m ()
      throwError $
        InternalError $
          "Couldn't apply finalizer action in \""
            <> monadType emptyAction
            <> "\"monad. It caused because previous action was created in \""
            <> monadType actions
            <> "\"monad."
    Just action -> action

runTransactionImpl ::
  forall m r a.
  (HasEsq m r, Finalize m) =>
  EsqDBEnv ->
  SqlDB a ->
  m (a, SqlDBEnv)
runTransactionImpl dbEnv run = do
  logEnv <- asks (.loggerEnv)
  liftIO $ runTransactionIO (Proxy @m) logEnv dbEnv run

runTransactionIO :: forall m a. Finalize m => Proxy m -> LoggerEnv -> EsqDBEnv -> SqlDB a -> IO (a, SqlDBEnv)
runTransactionIO _ logEnv dbEnv (SqlDB run) = do
  now <- getCurrentTime
  let sqlDBEnv =
        SqlDBEnv
          { currentTime = now,
            actions = pure @m ()
          }
  runLoggerIO logEnv $ runSqlPool (runStateT run sqlDBEnv) dbEnv.connPool

runInReplica :: (EsqDBReplicaFlow m r, MonadThrow m, Log m, Finalize m) => SelectSqlDB a -> m a
runInReplica (SelectSqlDB m) = do
  dbEnv <- asks (.esqDBReplicaEnv)
  runNoTransactionImpl dbEnv m

runNoTransactionImpl ::
  forall m r a.
  (HasEsq m r, Finalize m) =>
  EsqDBEnv ->
  SqlDB a ->
  m a
runNoTransactionImpl dbEnv run = do
  logEnv <- asks (.loggerEnv)
  liftIO $ runNoTransactionIO (Proxy @m) logEnv dbEnv run

runNoTransactionIO :: forall m a. Finalize m => Proxy m -> LoggerEnv -> EsqDBEnv -> SqlDB a -> IO a
runNoTransactionIO _ logEnv dbEnv (SqlDB run) = do
  now <- getCurrentTime
  let sqlDBEnv =
        SqlDBEnv
          { currentTime = now,
            actions = pure @m ()
          }
  runLoggerIO logEnv $ runSqlPoolNoTransaction (evalStateT run sqlDBEnv) dbEnv.connPool Nothing
