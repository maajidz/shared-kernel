{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Tools.Metrics.CoreMetrics.Types
  ( HasCoreMetrics,
    CoreMetrics (..),
    CoreMetricsContainer (..),
    DeploymentVersion (..),
    registerCoreMetricsContainer,
  )
where

import Data.Time (NominalDiffTime)
import EulerHS.Prelude as E
import GHC.Records.Extra
import Kernel.Types.Time (Milliseconds)
import Prometheus as P
import Servant.Client (BaseUrl, ClientError)

type RequestLatencyMetric = P.Vector P.Label4 P.Histogram

type DatastoresLatencyMetric = P.Vector P.Label3 P.Histogram

type ErrorCounterMetric = P.Vector P.Label4 P.Counter

type URLCallRetriesMetric = P.Vector P.Label3 P.Counter

type URLCallRetryFailuresMetric = P.Vector P.Label2 P.Counter

type HasCoreMetrics r =
  ( HasField "coreMetrics" r CoreMetricsContainer,
    HasField "version" r DeploymentVersion
  )

newtype DeploymentVersion = DeploymentVersion {getDeploymentVersion :: Text}

class CoreMetrics m where
  addRequestLatency ::
    Text ->
    Text ->
    Milliseconds ->
    Either ClientError a ->
    m ()
  addDatastoreLatency :: Text -> Text -> NominalDiffTime -> m ()
  incrementErrorCounter :: Text -> SomeException -> m ()
  addUrlCallRetries :: BaseUrl -> Int -> m ()
  addUrlCallRetryFailures :: BaseUrl -> m ()

data CoreMetricsContainer = CoreMetricsContainer
  { requestLatency :: RequestLatencyMetric,
    datastoresLatency :: DatastoresLatencyMetric,
    errorCounter :: ErrorCounterMetric,
    urlCallRetries :: URLCallRetriesMetric,
    urlCallRetryFailures :: URLCallRetryFailuresMetric
  }

registerCoreMetricsContainer :: IO CoreMetricsContainer
registerCoreMetricsContainer = do
  requestLatency <- registerRequestLatencyMetric
  datastoresLatency <- registerDatastoresLatencyMetrics
  errorCounter <- registerErrorCounterMetric
  urlCallRetries <- registerURLCallRetriesMetric
  urlCallRetryFailures <- registerURLCallRetryFailuresMetric

  return CoreMetricsContainer {..}

registerDatastoresLatencyMetrics :: IO DatastoresLatencyMetric
registerDatastoresLatencyMetrics =
  P.register $
    P.vector ("datastore", "operation", "version") $
      P.histogram info P.defaultBuckets
  where
    info = P.Info "datastore_operation_duration" ""

registerRequestLatencyMetric :: IO RequestLatencyMetric
registerRequestLatencyMetric =
  P.register $
    P.vector ("host", "service", "status", "version") $
      P.histogram info P.defaultBuckets
  where
    info = P.Info "external_request_duration" ""

registerErrorCounterMetric :: IO ErrorCounterMetric
registerErrorCounterMetric =
  P.register $
    P.vector ("HttpCode", "ErrorContext", "ErrorCode", "version") $
      P.counter info
  where
    info = P.Info "error_counter" ""

registerURLCallRetriesMetric :: IO URLCallRetriesMetric
registerURLCallRetriesMetric =
  P.register $
    P.vector ("URL", "RetryCount", "version") $
      P.counter info
  where
    info = P.Info "url_call_retries_counter" ""

registerURLCallRetryFailuresMetric :: IO URLCallRetryFailuresMetric
registerURLCallRetryFailuresMetric =
  P.register $
    P.vector ("URL", "version") $
      P.counter info
  where
    info = P.Info "url_call_retry_failures_counter" ""
