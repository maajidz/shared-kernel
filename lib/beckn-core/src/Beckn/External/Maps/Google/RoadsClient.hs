{-# LANGUAGE DerivingStrategies #-}

module Beckn.External.Maps.Google.RoadsClient where

import Beckn.External.Maps.Types
import Beckn.Prelude
import Beckn.Tools.Metrics.CoreMetrics as Metrics
import Beckn.Types.App (MandatoryQueryParam, MonadFlow)
import Beckn.Types.Error (GenericError (InternalError))
import Beckn.Utils.Common (callAPI, fromEitherM, throwError)
import qualified Data.Text as T
import EulerHS.Types as Euler
import Servant hiding (throwError)

type SnapToRoadResponse = SnapToRoadResponse' LatLong

data SnapToRoadResponse' a = SnapToRoadResponse
  { snappedPoints :: [SnappedPoint' a],
    warningMessage :: Maybe Text
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

type SnappedPoint = SnappedPoint' LatLong

data SnappedPoint' a = SnappedPoint
  { location :: a,
    originalIndex :: Maybe Int,
    placeId :: Text
  }
  deriving (Generic, FromJSON, ToJSON)

data SPLocation = SPLocation
  { latitude :: Double,
    longitude :: Double
  }
  deriving (Generic, FromJSON, ToJSON)

type SnapToRoadAPI =
  "v1" :> "snapToRoads"
    :> MandatoryQueryParam "key" Text
    :> MandatoryQueryParam "interpolate" Bool
    :> MandatoryQueryParam "path" Text
    :> Get '[JSON] (SnapToRoadResponse' SPLocation)

snapToRoad ::
  ( HasCallStack,
    Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  [LatLong] ->
  m SnapToRoadResponse
snapToRoad roadsUrl apiKey pointsList = do
  let eulerClient = Euler.client (Proxy @SnapToRoadAPI)
      interpolate = True
  res <-
    callAPI roadsUrl (eulerClient apiKey interpolate $ convertPointsList pointsList) "snap-to-road"
      >>= fromEitherM (\err -> InternalError $ "Failed to call snap-to-road API: " <> show err)
  maybe
    (pure ())
    (\warning -> throwError $ InternalError ("Snap-to-road API throwing warning" <> warning))
    res.warningMessage
  return $ SnapToRoadResponse (map (\sp -> sp {location = spLocToLatLong sp.location}) res.snappedPoints) res.warningMessage
  where
    convertPoint :: LatLong -> Text
    convertPoint pt = mconcat [show pt.lat, ",", show pt.lon]

    convertPointsList :: [LatLong] -> Text
    convertPointsList = T.intercalate "|" . map convertPoint

    spLocToLatLong :: SPLocation -> LatLong
    spLocToLatLong s =
      LatLong
        { lat = s.latitude,
          lon = s.longitude
        }
