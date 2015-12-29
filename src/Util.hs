module Util where

import Data.Acid (Update)
import Data.Aeson (ToJSON)
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as B
import Data.IxSet (empty)
import qualified Data.Text as T

import Happstack.Server (toResponse, Response)

import Types

-- {{{ API helpers
-- | successful API response
success :: a -> ApiResponse i a
success = ApiResponse . Right

-- | failed API response
failure :: ApiError -> ApiResponse i a
failure = ApiResponse . Left

-- | process anything that can be encoded as JSON to a Response
respond :: ToJSON r => r -> Response
respond = toResponse . encodePretty' (defConfig { confCompare = compare })
-- }}}

liftStore :: (a -> Update Morgue b)
          -> ApiResponse r a -> Update Morgue (ApiResponse r b)
liftStore f a = sequence $ f <$> a

initialMorgueState :: Morgue
initialMorgueState = Morgue empty empty
