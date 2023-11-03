{-# LANGUAGE DerivingStrategies #-}
{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedStrings #-}

module Kernel.External.Maps.HERE.MapsClient
  ( HEREAPI,
    PlaceNameAPI,
    GetPlaceNameResp (..),
    Item,
    getPlaceName,
  )
where

import EulerHS.Types (EulerClient, client)
import Kernel.External.Maps.Types
import Kernel.External.Types (Language)
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common
import Servant hiding (throwError)
import Servant.Client.Core (ClientError)

type HEREAPI =
  PlaceNameAPI

type PlaceNameAPI =
  "revgeocode"
    :> MandatoryQueryParam "key" Text
    :> MandatoryQueryParam "at" LatLong
    :> QueryParam "lang" Language
    :> Get '[JSON] GetPlaceNameResp

getPlaceNameClient :: Text -> LatLong -> Maybe Language -> EulerClient GetPlaceNameResp
getPlaceNameClient = client (Proxy :: Proxy HEREAPI)

newtype GetPlaceNameResp = GetPlaceNameResp
  { items :: [Item]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data Item = Item
  { title :: Text,
    id :: Text,
    politicalView :: Maybe Text,
    resultType :: Maybe Text,
    houseNumberType :: Maybe Text,
    addressBlockType :: Maybe Text,
    localityType :: Maybe Text,
    administrativeAreaType :: Maybe Text,
    address :: Address,
    position :: Location,
    access :: Maybe [Location],
    distance :: Maybe Int,
    -- , mapView :: Maybe Value
    -- , categories :: Maybe [Value]
    -- , foodTypes :: Maybe [Value]
    houseNumberFallback :: Maybe Bool
    -- , timeZone :: Value
    -- , streetInfo :: Maybe [Value]
    -- , countryInfo :: Maybe [Value]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data Address = Address
  { label :: Maybe Text,
    countryCode :: Maybe Text,
    countryName :: Maybe Text,
    stateCode :: Maybe Text,
    state :: Maybe Text,
    countyCode :: Maybe Text,
    county :: Maybe Text,
    city :: Maybe Text,
    district :: Maybe Text,
    subdistrict :: Maybe Text,
    street :: Maybe Text,
    block :: Maybe Text,
    subblock :: Maybe Text,
    postalCode :: Maybe Text,
    houseNumber :: Maybe Text,
    building :: Maybe Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data Location = Location
  { lat :: Double,
    lng :: Double
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

getPlaceName ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  LatLong ->
  Maybe Language ->
  m GetPlaceNameResp
getPlaceName url apiKey latLong language = do
  callAPI url (getPlaceNameClient apiKey latLong language) "getPlaceName" (Proxy :: Proxy HEREAPI)
    >>= checkHEREMapsError url

checkHEREMapsError :: (MonadThrow m, Log m) => BaseUrl -> Either ClientError a -> m a
checkHEREMapsError url =
  fromEitherM (hereMapsError url)

hereMapsError :: BaseUrl -> ClientError -> ExternalAPICallError
hereMapsError = ExternalAPICallError (Just "HERE_MAPS_API_ERROR")
