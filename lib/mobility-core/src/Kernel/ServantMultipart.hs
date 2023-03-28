{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Kernel.ServantMultipart
  ( module Servant.Multipart,
  )
where
-- import EulerHS.Prelude 
import Servant hiding (ResponseHeader (..))
import Servant.Multipart
import qualified Servant.OpenApi as S
-- import qualified Servant.Foreign as S 
import qualified Data.OpenApi as DS
import Kernel.Utils.Monitoring.Prometheus.Servant (SanitizedUrl (..))
import EulerHS.Prelude((.~),execState, at)
import Control.Lens ((?=),(?~))
import GHC.Exts (fromList)
import Kernel.Prelude hiding (at) 

-- instance (S.HasForeignType lang ftype a, S.HasForeign lang ftype api)
--       => S.HasForeign lang ftype (MultipartForm t a :> api) where
--   type Foreign ftype (MultipartForm t a :> api) = S.Foreign ftype api

--   foreignFor lang ftype Proxy req =
--     S.foreignFor lang ftype (Proxy @api) $
--       req & S.reqBody .~ Just t
--           & S.reqBodyContentType .~ S.ReqBodyMultipart
--     where
--       t = S.typeFor lang ftype (Proxy @a)


instance -- TODO: implementing OpenAPI interpretation for Multipart.
  ( S.HasOpenApi api, ToSchema a
  ) =>
  S.HasOpenApi (MultipartForm tag a :> api)
  where
  toOpenApi _ = 
    S.toOpenApi (Proxy @api)
    & addRequestBody "Hello"
      -- & S.addDefaultResponse400 headerName
      -- & addResponse401
    where
      methodName = "MultipartForm"

      addRequestBody :: Text -> DS.OpenApi -> DS.OpenApi
      addRequestBody description = execState $ do
        DS.components . DS.requestBodies . at methodName ?= multipartScheme
-- paths -- url -- method type -- request body -- content : multipart -- schema
        where
        multipartScheme =
           mempty
            & DS.description ?~ description
            & DS.content .~ content
            & DS.required ?~ True

        content =
          fromList
            [("multipart/form-data", mempty & DS.schema ?~ DS.Inline (DS.toSchema (Proxy :: Proxy a)))]
            -- mempty
            -- & DS._requestBodyDescription ?~ description
            -- & DS._requestBodyContent .~ content
        -- content =
        --   fromList
        --     [("multipart/form-data", mempty & DS.schema  ?~ DS.Inline (DS.toSchema (Proxy :: Proxy a)))]  -- & addParameters ""
            -- DS.RequestBody
            --   { _requestBodyDescription = Just description,
            --     _requestBodyContent = content --ReqBodyMultipart 
            --     where
            --       content = fromList [("multipart/form-data", mempty & MultiSchemaReqBody ?~ DS.toSchema (Proxy :: Proxy form))]  -- & addParameters ""

            --   }
            
          
-- instance -- TODO: implementing OpenAPI interpretation for Multipart.
--   ( S.HasOpenApi api
--   ) =>
--   S.HasOpenApi (MultipartForm tag a :> api)
--   where
--   toOpenApi _ = 
--     S.toOpenApi (Proxy @api)
--      & reqBody Control.Lens..~ Just t
--       & reqBodyContentType Control.Lens..~ ReqBodyMultipart
--     where
--       t = typeFor lang ftype (Proxy @a)
      -- addResponse401 :: DS.OpenApi -> DS.OpenApi
      -- addResponse401 = execState $ do
      --   DS.components . DS.responses . at response401Name ?= response401
      --   DS.allOperations . DS.responses . DS.responses . at 401
      --     ?= DS.Ref (DS.Reference response401Name)
      --   where
      --     response401Name = "Unauthorized"
      --     response401 = mempty & DS.description .~ "Unauthorized"


      -- & S.addDefaultResponse400 "Bad Request"
    -- where
    --   requestBody =
    --     mempty
    --       & DS._requestBodyDescription ?~ "Multipart form data"
    --       & DS._requestBodyContent .~ content
    --   content =
    --     fromList
    --       [("multipart/form-data", mempty & DS.mediaTypeSchema  ?~ DS.toSchema (Proxy :: Proxy form))]  -- & addParameters ""
    
    -- addResponse401 :: DS.OpenApi -> DS.OpenApi
    -- addResponse401 = execState $ do
    --   DS.components . DS.responses . at response401Name ?= response401
    --   DS.allOperations . DS.responses . DS.responses . at 401
    --     ?= DS.Ref (DS.Reference response401Name)
    --   where
    --     response401Name = "Unauthorized"
    --     response401 = mempty & DS.description .~ "Unauthorized"

instance
  SanitizedUrl (sub :: Type) =>
  SanitizedUrl (MultipartForm tag a :> sub)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy sub)
