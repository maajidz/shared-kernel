module Kernel.Types.Error.BaseError.HTTPError.BecknAPIError
  ( module Kernel.Types.Error.BaseError.HTTPError.BecknAPIError,
    Error.Error (..),
    Error.ErrorType (..),
  )
where

import Data.Aeson.Types
import EulerHS.Prelude hiding (Show, show, (.=))
import qualified Kernel.Types.Beckn.Error as Error
import Kernel.Types.Error.BaseError.HTTPError.FromResponse
import Prelude (Show (..))

class IsBecknAPIError e where
  toType :: e -> Error.ErrorType
  toType _ = Error.INTERNAL_ERROR

  toPath :: e -> Maybe Text
  toPath _ = Nothing

newtype BecknAPIError = BecknAPIError Error.Error
  deriving (Generic, Eq, Show)

instance FromJSON BecknAPIError where
  parseJSON (Object v) = BecknAPIError <$> v .: "error"
  parseJSON invalid =
    prependFailure
      "Parsing BecknAPIError failed, "
      (typeMismatch "Object" invalid)

instance ToJSON BecknAPIError where
  toJSON (BecknAPIError err) = object ["message" .= ack, "error" .= err]
    where
      ack = object ["ack" .= status]
      status = object ["status" .= ("NACK" :: Text)]

instance FromResponse BecknAPIError where
  fromResponse = fromJsonResponse