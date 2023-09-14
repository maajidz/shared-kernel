{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Ticket.Interface.Kapture
  ( createTicket,
  )
where

import Kernel.External.Encryption
import qualified Kernel.External.Ticket.Interface.Types as IT
import Kernel.External.Ticket.Kapture.Config
import qualified Kernel.External.Ticket.Kapture.Flow as KF
import Kernel.External.Ticket.Kapture.Types as Kapture
import Kernel.Prelude
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics

createTicket ::
  ( Metrics.CoreMetrics m,
    EncFlow m r
  ) =>
  KaptureCfg ->
  IT.CreateTicketReq ->
  m Kapture.CreateTicketResp
createTicket config req = do
  auth <- decrypt config.auth
  KF.createTicketAPI config.url config.version auth (mkCreateTicketReq req)

mkCreateTicketReq :: IT.CreateTicketReq -> Kapture.CreateTicketReq
mkCreateTicketReq IT.CreateTicketReq {..} =
  Kapture.CreateTicketReq
    { title = category,
      ticketDetails = issueDescription,
      customerId = personId,
      customerName = name,
      phone = phoneNo,
      issueDetails = mkIssueDetails,
      rideDetails = mkRideDescriptionDriver <$> rideDescription,
      classification = classification
    }
  where
    mkIssueDetails =
      Kapture.IssueDetails {..}

mkRideDescriptionDriver :: IT.RideInfo -> Kapture.RideInfo
mkRideDescriptionDriver IT.RideInfo {..} =
  Kapture.RideInfo
    { pickupLocation = mkLocation pickupLocation,
      dropLocation = mkLocation <$> dropLocation,
      ..
    }

mkLocation :: IT.Location -> Kapture.Location
mkLocation IT.Location {..} = Kapture.Location {..}
