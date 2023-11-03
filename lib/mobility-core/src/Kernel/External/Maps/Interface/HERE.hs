{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Maps.Interface.HERE
  ( module Reexport,
    getPlaceName,
  )
where

import Kernel.External.Encryption
import Kernel.External.Maps.HERE.Config as Reexport
import qualified Kernel.External.Maps.HERE.MapsClient as HERE
import Kernel.External.Maps.Interface.Types
import Kernel.External.Maps.Types as Reexport
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Error
import Kernel.Utils.Common hiding (id)

getPlaceName ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  HERECfg ->
  GetPlaceNameReq ->
  m GetPlaceNameResp
getPlaceName cfg GetPlaceNameReq {..} = do
  case mbByLatLong of
    Just latLong -> do
      let mapsUrl = cfg.hereUrl
      key <- decrypt cfg.hereKey
      res <- HERE.getPlaceName mapsUrl key latLong language
      return $ map reformatePlaceName res.items
    _ -> throwError (InternalError "HERE maps supports reverse geocode only.")
  where
    reformatePlaceName (placeName :: HERE.Item) =
      PlaceName
        { formattedAddress = placeName.address.label,
          addressComponents = reformateAddressResp placeName.address,
          plusCode = Nothing, -- placeName.plus_code <&> (.compound_code),
          location = let loc = placeName.position in LatLong loc.lat loc.lng,
          placeId = Just placeName.id
        }
    reformateAddressResp aResp =
      (\(tys, ln, sn) -> AddressResp ln sn tys)
        <$> catMaybes
          [ (["house_number"],,) <$> aResp.houseNumber <*> aResp.houseNumber,
            (["route"],,) <$> aResp.street <*> aResp.street,
            (["neighborhood", "political"],,) <$> aResp.subblock <*> aResp.subblock,
            (["sublocalilty", "political"],,) <$> aResp.block <*> aResp.block,
            (["administrative_area_level_4", "political"],,) <$> aResp.subdistrict <*> aResp.subdistrict,
            (["administrative_area_level_3", "political"],,) <$> aResp.district <*> aResp.district,
            (["administrative_area_level_2", "political"],,) <$> aResp.city <*> aResp.city,
            (["administrative_area_level_1", "political"],,) <$> aResp.state <*> aResp.state,
            (["country", "political"],,) <$> aResp.countryName <*> aResp.countryCode,
            (["postal_code"],,) <$> aResp.postalCode <*> aResp.postalCode
          ]
    mbByLatLong = case getBy of
      ByPlaceId _ -> Nothing
      ByLatLong latLong -> Just latLong
