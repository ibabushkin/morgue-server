module TemplateUtil where

import Data.Aeson.TH
import Data.Char (isUpper, toLower)

-- | convert a name from camelCase to snake_case
toSnakeCase :: String -> String
toSnakeCase s = case foldr func "" s of
                  'i':'_':cs -> cs
                  '_':cs -> cs
                  cs -> cs
    where func c s | isUpper c = '_' : toLower c : s
                   | otherwise = c : s

-- | default options for instance generation
internalOptions :: Options
internalOptions = defaultOptions { fieldLabelModifier = toSnakeCase }
