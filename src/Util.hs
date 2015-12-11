module Util where

import Data.Aeson (ToJSON)
import Data.Aeson.Encode.Pretty

import Happstack.Server (toResponse, Response)

import Types

-- | successful API response
success :: a -> ApiResponse a
success = ApiResponse . Right

-- | failed API response
failure :: ApiError -> ApiResponse a
failure = ApiResponse . Left

-- | process anything that can be encoded as JSON to a Response
respond :: ToJSON r => r -> Response
respond = toResponse . encodePretty' (defConfig { confCompare = compare })
