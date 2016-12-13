{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
module Sigym4.Units.Meteo (
  CloudCover (..)
--, RelativeHumidity
--, AirTemperature
) where


import           Sigym4.Units
import           Sigym4.Null
import           Control.DeepSeq (NFData)
import           Control.Newtype
import           Data.Functor.Identity
import           Foreign.Storable (Storable)

-- | This represents the cloud coverage normalized ratio
newtype CloudCover a = CloudCover { getCloudCover :: NormRatio a }
  deriving (Eq, Ord, Show, HasNull, Storable, NFData)

deriveHasUnits [t|forall a. (Fractional a, Eq a) => CloudCover a -> NormRatio a|] 'CloudCover 'getCloudCover
