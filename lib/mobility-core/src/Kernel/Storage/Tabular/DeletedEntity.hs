{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Kernel.Storage.Tabular.DeletedEntity where

import Kernel.Prelude
import Kernel.Storage.Esqueleto
import qualified Kernel.Types.DeletedEntity as Domain
import Kernel.Types.Id

derivePersistField "Domain.DeletedBy"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    DeletedEntityT sql=deleted_entity
      id Text
      primaryId Text Maybe
      tableName Text
      rowData Text
      deletedBy Domain.DeletedBy
      deletedOn UTCTime

      Primary id
      deriving Generic
    |]

instance TEntityKey DeletedEntityT where
  type DomainKey DeletedEntityT = Id Domain.DeletedEntity
  fromKey (DeletedEntityTKey _id) = Id _id
  toKey (Id id) = DeletedEntityTKey id

instance FromTType DeletedEntityT Domain.DeletedEntity where
  fromTType DeletedEntityT {..} =
    return $
      Domain.DeletedEntity
        { id = Id id,
          ..
        }

instance ToTType DeletedEntityT Domain.DeletedEntity where
  toTType Domain.DeletedEntity {..} =
    DeletedEntityT
      { id = getId id,
        ..
      }
