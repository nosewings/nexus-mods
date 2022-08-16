module NexusMods.Internal.TH (
  deriveJSONOptions,
) where

import Data.Aeson

deriveJSONOptions :: Options
deriveJSONOptions = defaultOptions {fieldLabelModifier = camelTo2 '_'}
