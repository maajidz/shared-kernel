module Kernel.Beam.Connection.Types where

import Kernel.Storage.Esqueleto.Config
import Kernel.Storage.Hedis.Config

data ConnectionConfig = ConnectionConfig
  { esqDBCfg :: EsqDBConfig,
    esqDBReplicaCfg :: EsqDBConfig,
    hedisClusterCfg :: HedisCfg,
    locationDbCfg :: EsqDBConfig,
    locationDbReplicaCfg :: EsqDBConfig
  }
