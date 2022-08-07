{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Beckn.Storage.Esqueleto.Queries
  ( findOne,
    findOne',
    findById,
    findById',
    findAll,
    findAll',
    create,
    create',
    update,
    update',
    createMany,
    createMany',
    updateReturningCount,
    updateReturningCount',
    deleteByKey,
    deleteByKey',
    delete,
    delete',
    deleteReturningCount,
    deleteReturningCount',
    repsert,
    repsert',
    upsert,
    upsert',
    upsertBy,
    upsertBy',
    insertSelect,
    insertSelect',
    insertSelectCount,
    insertSelectCount',
    (<#>),
    whenJust_,
    whenTrue_,
    updateWhenJust_,
    maybe_,
    module EsqExport,
  )
where

import Beckn.Prelude
import Beckn.Storage.Esqueleto.Class
import Beckn.Storage.Esqueleto.DTypeBuilder
import Beckn.Storage.Esqueleto.SqlDB
import Beckn.Storage.Esqueleto.Transactionable
import Beckn.Types.Logging (Log)
import Database.Esqueleto.Experimental as EsqExport hiding
  ( Table,
    delete,
    deleteCount,
    deleteKey,
    insert,
    insertSelect,
    insertSelectCount,
    repsert,
    selectOne,
    update,
    updateCount,
    upsert,
    upsertBy,
    (<&>),
  )
import qualified Database.Esqueleto.Experimental as Esq
import qualified Database.Esqueleto.Internal.Internal as Esq
import Database.Persist.Postgresql hiding (delete, repsert, update, upsert, upsertBy)

findOne :: (Transactionable m, Esq.SqlSelect b t, QEntity t a) => Esq.SqlQuery b -> m (Maybe a)
findOne = buildDType . findOneInternal

findOne' :: (Transactionable m, TEntity t a, Esq.SqlSelect b t) => Esq.SqlQuery b -> DTypeBuilder m (Maybe a)
findOne' q = extractTType <$> findOneInternal q

findOneInternal :: (Transactionable m, Esq.SqlSelect b t) => Esq.SqlQuery b -> DTypeBuilder m (Maybe t)
findOneInternal q = liftToBuilder . runTransaction $ lift selectOnlyOne
  where
    selectOnlyOne = do
      list <- Esq.select q
      case list of
        [res] -> return $ Just res
        _ -> return Nothing

findById :: forall a t m. (Transactionable m, QEntity (Entity t) a, TEntityKey t) => DomainKey t -> m (Maybe a)
findById = buildDType . findByIdInternal @t

findById' :: forall t m. (Transactionable m, TEntityKey t, TEntity (Entity t) t) => DomainKey t -> DTypeBuilder m (Maybe t)
findById' dkey = extractTType <$> findByIdInternal @t dkey

findByIdInternal :: forall t m. (Transactionable m, TEntityKey t, Log m) => DomainKey t -> DTypeBuilder m (Maybe (Entity t))
findByIdInternal dkey = findOneInternal $ do
  let key = toKey @t dkey
  res <- from $ table @t
  where_ $ res Esq.^. persistIdField Esq.==. val key
  return res

findAll :: (Transactionable m, Esq.SqlSelect b t, QEntity [t] [a]) => Esq.SqlQuery b -> m [a]
findAll q = buildDType $ findAllInternal q

findAll' :: (Transactionable m, Esq.SqlSelect b t, TEntity [t] [a]) => Esq.SqlQuery b -> DTypeBuilder m [a]
findAll' q = extractTType <$> findAllInternal q

findAllInternal :: (Transactionable m, Esq.SqlSelect b t) => Esq.SqlQuery b -> DTypeBuilder m [t]
findAllInternal q = liftToBuilder . runTransaction $ lift (Esq.select q)

create ::
  ( PersistEntity t,
    PersistEntityBackend t ~ SqlBackend,
    TType t a
  ) =>
  a ->
  SqlDB ()
create q = do
  let ttypes = toTType q
  lift $ Esq.insert_ ttypes

create' ::
  ( PersistEntity t,
    PersistEntityBackend t ~ SqlBackend
  ) =>
  t ->
  FullEntitySqlDB ()
create' q = do
  liftToFullEntitySqlDB . lift $ Esq.insert_ q

createMany ::
  ( PersistEntity t,
    PersistEntityBackend t ~ SqlBackend,
    TType t a
  ) =>
  [a] ->
  SqlDB ()
createMany q = do
  let ttypes = toTType `fmap` q
  lift $ Esq.insertMany_ ttypes

createMany' ::
  ( PersistEntity t,
    PersistEntityBackend t ~ SqlBackend
  ) =>
  [t] ->
  FullEntitySqlDB ()
createMany' q = do
  liftToFullEntitySqlDB . lift $ Esq.insertMany_ q

update ::
  ( PersistEntity a,
    BackendCompatible SqlBackend (PersistEntityBackend a)
  ) =>
  (Esq.SqlExpr (Entity a) -> Esq.SqlQuery ()) ->
  SqlDB ()
update = lift . Esq.update

update' ::
  ( PersistEntity a,
    BackendCompatible SqlBackend (PersistEntityBackend a)
  ) =>
  (Esq.SqlExpr (Entity a) -> Esq.SqlQuery ()) ->
  FullEntitySqlDB ()
update' = liftToFullEntitySqlDB . lift . Esq.update

updateReturningCount ::
  ( PersistEntity a,
    BackendCompatible SqlBackend (PersistEntityBackend a)
  ) =>
  (Esq.SqlExpr (Entity a) -> Esq.SqlQuery ()) ->
  SqlDB Int64
updateReturningCount = lift . Esq.updateCount

updateReturningCount' ::
  ( PersistEntity a,
    BackendCompatible SqlBackend (PersistEntityBackend a)
  ) =>
  (Esq.SqlExpr (Entity a) -> Esq.SqlQuery ()) ->
  FullEntitySqlDB Int64
updateReturningCount' = liftToFullEntitySqlDB . lift . Esq.updateCount

deleteByKey ::
  forall t.
  ( TEntityKey t
  ) =>
  DomainKey t ->
  SqlDB ()
deleteByKey = lift . Esq.deleteKey . toKey @t

deleteByKey' ::
  ( PersistEntity t,
    PersistEntityBackend t ~ SqlBackend
  ) =>
  Key t ->
  FullEntitySqlDB ()
deleteByKey' = liftToFullEntitySqlDB . lift . Esq.deleteKey

delete ::
  Esq.SqlQuery () ->
  SqlDB ()
delete = lift . Esq.delete

delete' ::
  Esq.SqlQuery () ->
  FullEntitySqlDB ()
delete' = liftToFullEntitySqlDB . lift . Esq.delete

deleteReturningCount ::
  Esq.SqlQuery () ->
  SqlDB Int64
deleteReturningCount = lift . Esq.deleteCount

deleteReturningCount' ::
  Esq.SqlQuery () ->
  FullEntitySqlDB Int64
deleteReturningCount' = liftToFullEntitySqlDB . lift . Esq.deleteCount

repsert ::
  ( PersistEntityBackend t ~ SqlBackend,
    TType t a,
    TEntityKey t
  ) =>
  DomainKey t ->
  a ->
  SqlDB ()
repsert k v = do
  let ttype = toTType v
  lift $ Esq.repsert (toKey k) ttype

repsert' ::
  ( PersistEntity t,
    PersistEntityBackend t ~ SqlBackend
  ) =>
  Key t ->
  t ->
  FullEntitySqlDB ()
repsert' k v = do
  liftToFullEntitySqlDB . lift $ Esq.repsert k v

upsert ::
  ( OnlyOneUniqueKey t,
    PersistEntityBackend t ~ SqlBackend,
    TType t a
  ) =>
  a ->
  [SqlExpr (Entity t) -> SqlExpr Esq.Update] ->
  SqlDB ()
upsert r u = do
  let uniqueKey = onlyUniqueP $ toTType r
  upsertBy uniqueKey r u

upsert' ::
  ( OnlyOneUniqueKey t,
    PersistEntityBackend t ~ SqlBackend
  ) =>
  t ->
  [SqlExpr (Entity t) -> SqlExpr Esq.Update] ->
  FullEntitySqlDB ()
upsert' r u = do
  let uniqueKey = onlyUniqueP r
  upsertBy' uniqueKey r u

upsertBy ::
  ( PersistEntity t,
    PersistEntityBackend t ~ SqlBackend,
    TType t a
  ) =>
  Unique t ->
  a ->
  [SqlExpr (Entity t) -> SqlExpr Esq.Update] ->
  SqlDB ()
upsertBy k r u = do
  mbEntity <- lift $ getBy k
  case mbEntity of
    Nothing -> create r
    Just ent -> update $ \tbl -> do
      Esq.set
        tbl
        u
      where_ $ tbl Esq.^. persistIdField Esq.==. val (entityKey ent)

upsertBy' ::
  ( PersistEntity t,
    PersistEntityBackend t ~ SqlBackend
  ) =>
  Unique t ->
  t ->
  [SqlExpr (Entity t) -> SqlExpr Esq.Update] ->
  FullEntitySqlDB ()
upsertBy' k r u = do
  mbEntity <- liftToFullEntitySqlDB . lift $ getBy k
  case mbEntity of
    Nothing -> create' r
    Just ent -> update' $ \tbl -> do
      Esq.set
        tbl
        u
      where_ $ tbl Esq.^. persistIdField Esq.==. val (entityKey ent)

insertSelect ::
  ( PersistEntity t
  ) =>
  SqlQuery (SqlExpr (Esq.Insertion t)) ->
  SqlDB ()
insertSelect = lift . Esq.insertSelect

insertSelect' ::
  ( PersistEntity t
  ) =>
  SqlQuery (SqlExpr (Esq.Insertion t)) ->
  FullEntitySqlDB ()
insertSelect' = liftToFullEntitySqlDB . lift . Esq.insertSelect

insertSelectCount ::
  ( PersistEntity t
  ) =>
  SqlQuery (SqlExpr (Esq.Insertion t)) ->
  SqlDB Int64
insertSelectCount = lift . Esq.insertSelectCount

insertSelectCount' ::
  ( PersistEntity t
  ) =>
  SqlQuery (SqlExpr (Esq.Insertion t)) ->
  FullEntitySqlDB Int64
insertSelectCount' = liftToFullEntitySqlDB . lift . Esq.insertSelectCount

(<#>) :: SqlExpr (Esq.Insertion (a -> b)) -> SqlExpr (Value a) -> SqlExpr (Esq.Insertion b)
(<#>) = (Esq.<&>)

whenJust_ :: Maybe a -> (a -> SqlExpr (Value Bool)) -> SqlExpr (Value Bool)
whenJust_ mbVal func = maybe (Esq.val True) func mbVal

whenTrue_ :: Bool -> SqlExpr (Value Bool) -> SqlExpr (Value Bool)
whenTrue_ bl func = bool (Esq.val True) func bl

updateWhenJust_ :: (a -> SqlExpr (Entity e) -> SqlExpr Esq.Update) -> Maybe a -> [SqlExpr (Entity e) -> SqlExpr Esq.Update]
updateWhenJust_ f = maybe [] (\value -> [f value])

maybe_ ::
  forall a b.
  (PersistField a, PersistField b) =>
  SqlExpr (Value b) ->
  (SqlExpr (Value a) -> SqlExpr (Value b)) ->
  SqlExpr (Value (Maybe a)) ->
  SqlExpr (Value b)
maybe_ def f mbVal =
  case_
    [when_ (Esq.isNothing mbVal) then_ def]
    ( else_ $ f $ Esq.veryUnsafeCoerceSqlExprValue mbVal
    )
