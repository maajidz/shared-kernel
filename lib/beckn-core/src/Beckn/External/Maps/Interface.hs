module Beckn.External.Maps.Interface
  ( module Reexport,
    getDistance,
    getDistancesProvided,
    getDistances,
    getRoutesProvided,
    getRoutes,
    snapToRoadProvided,
    snapToRoad,
    autoCompleteProvided,
    autoComplete,
    getPlaceDetailsProvided,
    getPlaceDetails,
    getPlaceNameProvided,
    getPlaceName,
  )
where

import Beckn.External.Maps.Google.Config as Reexport
import Beckn.External.Maps.HasCoordinates as Reexport (HasCoordinates (..))
import qualified Beckn.External.Maps.Interface.Google as Google
import qualified Beckn.External.Maps.Interface.MMI as MMI
import qualified Beckn.External.Maps.Interface.OSRM as OSRM
import Beckn.External.Maps.Interface.Types as Reexport
import Beckn.External.Maps.MMI.Config as Reexport
import Beckn.External.Maps.OSRM.Config as Reexport
import Beckn.External.Maps.Types as Reexport
import Beckn.Prelude
import Beckn.Storage.Hedis as Redis
import Beckn.Tools.Metrics.CoreMetrics (CoreMetrics)
import Beckn.Types.Common hiding (id)
import Beckn.Types.Error
import Beckn.Utils.Common hiding (id)
import EulerHS.Prelude ((...))

getDistance ::
  ( EncFlow m r,
    CoreMetrics m,
    HasCoordinates a,
    HasCoordinates b
  ) =>
  MapsServiceConfig ->
  GetDistanceReq a b ->
  m (GetDistanceResp a b)
getDistance serviceConfig GetDistanceReq {..} =
  getDistances serviceConfig getDistancesReq >>= \case
    (a :| []) -> return a
    _ -> throwError (InternalError "Exactly one getDistance result expected.")
  where
    getDistancesReq =
      GetDistancesReq
        { origins = origin :| [],
          destinations = destination :| [],
          ..
        }

mkNotProvidedError :: Text -> MapsService -> Text
mkNotProvidedError functionName serviceName = "Function " <> functionName <> " is not provided by service " <> show serviceName

throwNotProvidedError :: (MonadFlow m) => Text -> MapsService -> m a
throwNotProvidedError =
  (throwError . InternalError) ... mkNotProvidedError

getDistancesProvided :: MapsService -> Bool
getDistancesProvided = \case
  Google -> True
  OSRM -> False
  MMI -> False

-- FIXME this logic is redundant, because we throw error always when getDistancesProvided service = False
getDistances ::
  ( EncFlow m r,
    CoreMetrics m,
    HasCoordinates a,
    HasCoordinates b
  ) =>
  MapsServiceConfig ->
  GetDistancesReq a b ->
  m (GetDistancesResp a b)
getDistances serviceConfig req = case serviceConfig of
  GoogleConfig cfg -> Google.getDistances cfg req
  OSRMConfig _ -> throwNotProvidedError "getDistances" OSRM
  MMIConfig _ -> throwNotProvidedError "getDistances" MMI

getRoutesProvided :: MapsService -> Bool
getRoutesProvided = \case
  Google -> True
  OSRM -> False
  MMI -> False

getRoutes ::
  ( EncFlow m r,
    CoreMetrics m,
    Log m
  ) =>
  MapsServiceConfig ->
  GetRoutesReq ->
  m GetRoutesResp
getRoutes serviceConfig req = case serviceConfig of
  GoogleConfig cfg -> Google.getRoutes cfg req
  OSRMConfig _ -> throwNotProvidedError "getRoutes" OSRM
  MMIConfig _ -> throwNotProvidedError "getRoutes" MMI

snapToRoadProvided :: MapsService -> Bool
snapToRoadProvided = \case
  Google -> True
  OSRM -> True
  MMI -> False

snapToRoad ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  MapsServiceConfig ->
  SnapToRoadReq ->
  m SnapToRoadResp
snapToRoad serviceConfig req = case serviceConfig of
  GoogleConfig cfg -> Google.snapToRoad cfg req
  OSRMConfig osrmCfg -> OSRM.callOsrmMatch osrmCfg req
  MMIConfig _ -> throwNotProvidedError "snapToRoad" MMI

autoCompleteProvided :: MapsService -> Bool
autoCompleteProvided = \case
  Google -> True
  OSRM -> False
  MMI -> True

autoComplete ::
  ( EncFlow m r,
    Redis.HedisFlow m r,
    CoreMetrics m
  ) =>
  MapsServiceConfig ->
  AutoCompleteReq ->
  m AutoCompleteResp
autoComplete serviceConfig req = case serviceConfig of
  GoogleConfig cfg -> Google.autoComplete cfg req
  OSRMConfig _ -> throwNotProvidedError "autoComplete" OSRM
  MMIConfig cfg -> MMI.autoSuggest cfg req

getPlaceDetailsProvided :: MapsService -> Bool
getPlaceDetailsProvided = \case
  Google -> True
  OSRM -> False
  MMI -> False

getPlaceDetails ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  MapsServiceConfig ->
  GetPlaceDetailsReq ->
  m GetPlaceDetailsResp
getPlaceDetails serviceConfig req = case serviceConfig of
  GoogleConfig cfg -> Google.getPlaceDetails cfg req
  OSRMConfig _ -> throwNotProvidedError "getPlaceDetails" OSRM
  MMIConfig _ -> throwNotProvidedError "getPlaceDetails" MMI

getPlaceNameProvided :: MapsService -> Bool
getPlaceNameProvided = \case
  Google -> True
  OSRM -> False
  MMI -> False

getPlaceName ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  MapsServiceConfig ->
  GetPlaceNameReq ->
  m GetPlaceNameResp
getPlaceName serviceConfig req = case serviceConfig of
  GoogleConfig cfg -> Google.getPlaceName cfg req
  OSRMConfig _ -> throwNotProvidedError "getPlaceName" OSRM
  MMIConfig _ -> throwNotProvidedError "getPlaceName" MMI
