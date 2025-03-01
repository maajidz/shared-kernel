{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Kernel.External.Notification.Interface
  ( module Reexport,
    module Kernel.External.Notification.Interface,
  )
where

import Data.Default.Class
import EulerHS.Prelude
import qualified Kernel.External.Notification.Interface.FCM as FCM
import qualified Kernel.External.Notification.Interface.PayTM as PayTM
import Kernel.External.Notification.Interface.Types as Reexport
import Kernel.External.Notification.Types as Reexport
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common

notifyPerson' ::
  ( MonadFlow m,
    EncFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r,
    Default a,
    ToJSON a,
    ToJSON b
  ) =>
  NotificationServiceConfig ->
  NotificationReq a b ->
  Bool ->
  m ()
notifyPerson' serviceConfig req isMutable = case serviceConfig of
  FCMConfig cfg -> FCM.notifyPerson cfg req isMutable
  PayTMConfig cfg -> PayTM.notifyPerson cfg req

notifyPerson ::
  ( MonadFlow m,
    EncFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r,
    Default a,
    ToJSON a,
    ToJSON b
  ) =>
  NotificationServiceConfig ->
  NotificationReq a b ->
  m ()
notifyPerson serviceConfig req = notifyPerson' serviceConfig req False

notifyPersonWithMutableContent ::
  ( MonadFlow m,
    EncFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r,
    Default a,
    ToJSON a,
    ToJSON b
  ) =>
  NotificationServiceConfig ->
  NotificationReq a b ->
  m ()
notifyPersonWithMutableContent serviceConfig req = notifyPerson' serviceConfig req True
