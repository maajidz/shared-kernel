{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Kernel.Types.Beckn.City (City (..)) where

import Data.Aeson
import Data.Aeson.Types
import Data.Char (toLower)
import Data.OpenApi hiding (Example)
import qualified Data.Text as T
import EulerHS.Prelude
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnum)
import Kernel.Storage.Esqueleto
import Kernel.Utils.GenericPretty
import Servant.API (FromHttpApiData (..), ToHttpApiData (..))

derivePersistField "City"

data City = Bangalore | Kolkata | Paris | Kochi | AnyCity
  deriving (Eq, Generic, Show, Read, ToSchema, Ord, ToParamSchema)
  deriving (PrettyShow) via Showable City

$(mkBeamInstancesForEnum ''City)

instance FromJSON City where
  parseJSON (String "std:080") = pure Bangalore
  parseJSON (String "Bangalore") = pure Bangalore
  parseJSON (String "std:033") = pure Kolkata
  parseJSON (String "Kolkata") = pure Kolkata
  parseJSON (String "std:001") = pure Paris
  parseJSON (String "Paris") = pure Paris
  parseJSON (String "std:484") = pure Kochi
  parseJSON (String "Kochi") = pure Kochi
  parseJSON (String "*") = pure AnyCity
  parseJSON (String _) = parseFail "Invalid City"
  parseJSON e = typeMismatch "String" e

instance ToJSON City where
  toJSON Bangalore = String "std:080"
  toJSON Kolkata = String "std:033"
  toJSON Paris = String "std:001"
  toJSON Kochi = String "std:484"
  toJSON AnyCity = String "*"

instance FromHttpApiData City where
  parseUrlPiece a =
    let lower = map toLower $ T.unpack a
     in parseLowerCaseCity lower
    where
      parseLowerCaseCity "std:080" = Right Bangalore
      parseLowerCaseCity "bangalore" = Right Bangalore
      parseLowerCaseCity "std:033" = Right Kolkata
      parseLowerCaseCity "kolkata" = Right Kolkata
      parseLowerCaseCity "std:001" = Right Paris
      parseLowerCaseCity "paris" = Right Paris
      parseLowerCaseCity "std:484" = Right Kochi
      parseLowerCaseCity "kochi" = Right Kochi
      parseLowerCaseCity "*" = Right AnyCity
      parseLowerCaseCity city = Left . T.pack $ ("ParseFail: Unable to parse city: " <> city)

instance ToHttpApiData City where
  toUrlPiece Bangalore = "std:080"
  toUrlPiece Kolkata = "std:033"
  toUrlPiece Paris = "std:001"
  toUrlPiece Kochi = "std:484"
  toUrlPiece AnyCity = "*"
