{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Kernel.Tools.Metrics.CoreMetrics
  ( module Kernel.Tools.Metrics.CoreMetrics,
    module Reexport,
  )
where

import Data.Text as DT
import Data.Time (NominalDiffTime, nominalDiffTimeToSeconds)
import qualified EulerHS.Language as L
import EulerHS.Prelude as E
import GHC.Records.Extra
import Kernel.Tools.Metrics.CoreMetrics.Types as Reexport
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Types.Error.BaseError.HTTPError (BaseException (..), HTTPException (..), IsBaseError (toMessage), IsHTTPError (toErrorCode, toHttpCode), IsHTTPException)
import Kernel.Types.Time (Milliseconds, getMilliseconds)
import Kernel.Utils.Servant.BaseUrl
import Prometheus as P
import Servant.Client (BaseUrl, ClientError (..), ResponseF (..))

incrementErrorCounterImplementation ::
  ( HasCoreMetrics r,
    L.MonadFlow m,
    MonadReader r m
  ) =>
  Text ->
  SomeException ->
  m ()
incrementErrorCounterImplementation errorContext err = do
  cmContainer <- asks (.coreMetrics)
  version <- asks (.version)
  incrementErrorCounterImplementation' cmContainer errorContext err version

addUrlCallRetriesImplementation ::
  ( HasCoreMetrics r,
    L.MonadFlow m,
    MonadReader r m
  ) =>
  BaseUrl ->
  Int ->
  m ()
addUrlCallRetriesImplementation url retryCount = do
  cmContainer <- asks (.coreMetrics)
  version <- asks (.version)
  addUrlCallRetriesImplementation' cmContainer url retryCount version

addUrlCallFailuresImplementation ::
  ( HasCoreMetrics r,
    L.MonadFlow m,
    MonadReader r m
  ) =>
  BaseUrl ->
  m ()
addUrlCallFailuresImplementation url = do
  cmContainer <- asks (.coreMetrics)
  version <- asks (.version)
  addUrlCallFailuresImplementation' cmContainer url version

addRequestLatencyImplementation ::
  ( HasCoreMetrics r,
    L.MonadFlow m,
    MonadReader r m
  ) =>
  Text ->
  Text ->
  Milliseconds ->
  Either ClientError a ->
  m ()
addRequestLatencyImplementation host serviceName dur status = do
  cmContainer <- asks (.coreMetrics)
  version <- asks (.version)
  addRequestLatencyImplementation' cmContainer host serviceName dur status version

addDatastoreLatencyImplementation ::
  ( HasCoreMetrics r,
    L.MonadFlow m,
    MonadReader r m
  ) =>
  Text ->
  Text ->
  NominalDiffTime ->
  m ()
addDatastoreLatencyImplementation storeType operation latency = do
  cmContainer <- asks (.coreMetrics)
  version <- asks (.version)
  L.runIO $
    P.withLabel
      cmContainer.datastoresLatency
      (storeType, operation, version.getDeploymentVersion)
      (`P.observe` (fromIntegral $ div (fromEnum . nominalDiffTimeToSeconds $ latency) 1000000000000))

addRequestLatencyImplementation' ::
  L.MonadFlow m =>
  CoreMetricsContainer ->
  Text ->
  Text ->
  Milliseconds ->
  Either ClientError a ->
  DeploymentVersion ->
  m ()
addRequestLatencyImplementation' cmContainer host serviceName dur status version = do
  let requestLatencyMetric = cmContainer.requestLatency
  L.runIO $
    P.withLabel
      requestLatencyMetric
      (host, serviceName, status', version.getDeploymentVersion)
      (`P.observe` ((/ 1000) . fromIntegral $ getMilliseconds dur))
  where
    status' =
      case status of
        Right _ -> "200"
        Left (FailureResponse _ (Response code _ _ _)) -> show code
        Left (DecodeFailure _ (Response code _ _ _)) -> show code
        Left (InvalidContentTypeHeader (Response code _ _ _)) -> show code
        Left (UnsupportedContentType _ (Response code _ _ _)) -> show code
        Left (ConnectionError _) -> "Connection error"

incrementErrorCounterImplementation' :: L.MonadFlow m => CoreMetricsContainer -> Text -> SomeException -> DeploymentVersion -> m ()
incrementErrorCounterImplementation' cmContainers errorContext exc version
  | Just (HTTPException err) <- fromException exc = incCounter' err
  | Just (BaseException err) <- fromException exc = incCounter' . InternalError . fromMaybe (show err) $ toMessage err
  | otherwise = incCounter' . InternalError $ show exc
  where
    errorCounterMetric = cmContainers.errorCounter

    incCounter' :: (L.MonadFlow m, IsHTTPException e) => e -> m ()
    incCounter' err =
      L.runIO $
        P.withLabel
          errorCounterMetric
          (show $ toHttpCode err, errorContext, toErrorCode err, version.getDeploymentVersion)
          P.incCounter

addUrlCallRetriesImplementation' :: L.MonadFlow m => CoreMetricsContainer -> BaseUrl -> Int -> DeploymentVersion -> m ()
addUrlCallRetriesImplementation' cmContainers url retryCount version = do
  let urlCallRetriesMetric = cmContainers.urlCallRetries
  L.runIO $
    P.withLabel
      urlCallRetriesMetric
      (showBaseUrlText url, show retryCount, version.getDeploymentVersion)
      P.incCounter

addUrlCallFailuresImplementation' :: L.MonadFlow m => CoreMetricsContainer -> BaseUrl -> DeploymentVersion -> m ()
addUrlCallFailuresImplementation' cmContainers url version = do
  let urlCallRetriesMetric = cmContainers.urlCallRetryFailures
  L.runIO $
    P.withLabel
      urlCallRetriesMetric
      (showBaseUrlText url, version.getDeploymentVersion)
      P.incCounter

incrementSortedSetCounterImplementation ::
  ( HasCoreMetrics r,
    L.MonadFlow m,
    MonadReader r m
  ) =>
  Text ->
  Int ->
  m ()
incrementSortedSetCounterImplementation context scheduledSecond = do
  cmContainer <- asks (.coreMetrics)
  version <- asks (.version)
  incrementSortedSetCounterImplementation' cmContainer context scheduledSecond version

incrementSortedSetCounterImplementation' :: L.MonadFlow m => CoreMetricsContainer -> Text -> Int -> DeploymentVersion -> m ()
incrementSortedSetCounterImplementation' cmContainers context scheduledSecond version = do
  let sortedSetMetric = cmContainers.sortedSetCounter
  L.runIO $
    P.withLabel
      sortedSetMetric
      (context, show scheduledSecond, version.getDeploymentVersion)
      P.incCounter

incrementStreamCounterImplementation ::
  ( HasCoreMetrics r,
    L.MonadFlow m,
    MonadReader r m
  ) =>
  Text ->
  Int ->
  m ()
incrementStreamCounterImplementation context executedseconds = do
  cmContainer <- asks (.coreMetrics)
  version <- asks (.version)
  incrementStreamCounterImplementation' cmContainer context executedseconds version

incrementStreamCounterImplementation' :: L.MonadFlow m => CoreMetricsContainer -> Text -> Int -> DeploymentVersion -> m ()
incrementStreamCounterImplementation' cmContainers context executedseconds version = do
  let sortedSetMetric = cmContainers.sortedSetCounter
  L.runIO $
    P.withLabel
      sortedSetMetric
      (context, show executedseconds, version.getDeploymentVersion)
      P.incCounter

addGenericLatencyImplementation ::
  ( HasCoreMetrics r,
    L.MonadFlow m,
    MonadReader r m
  ) =>
  Text ->
  NominalDiffTime ->
  m ()
addGenericLatencyImplementation operation latency = do
  cmContainer <- asks (.coreMetrics)
  version <- asks (.version)
  L.runIO $
    P.withLabel
      cmContainer.genericLatency
      (operation, version.getDeploymentVersion)
      (`P.observe` (fromIntegral $ div (fromEnum . nominalDiffTimeToSeconds $ latency) 1000000000000))
