{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeOperators #-}
module Sigym4.Units.Meteo (
  CloudCover (..)
, DRainfall
, litersPerMeter
, Rainfall
--, RelativeHumidity
--, AirTemperature
) where


import           Sigym4.Units
import           Sigym4.Null
import           Control.DeepSeq (NFData)
import           Control.Newtype
import           Data.Default
import           Data.Fingerprint
import           Data.Functor.Identity
import           Foreign.Storable (Storable)
import qualified Numeric.Units.Dimensional as DP

-- | This represents the cloud coverage normalized ratio
newtype CloudCover a = CloudCover { getCloudCover :: NormRatio a }
  deriving (Eq, Ord, Show, HasNull, Storable, NFData)
instance Storable a => HasFingerprint (CloudCover a)

deriveHasUnits [t|forall a. (Fractional a, Eq a) => CloudCover a -> NormRatio a|] 'CloudCover 'getCloudCover


type DRainfall = DVolume DP./ DArea

litersPerMeter :: Fractional a => Unit 'NonMetric DRainfall a
litersPerMeter = weaken liter DP./ square meter

-- | This represents the rainfall (or snowfall) amount. (volume per area)
type Rainfall = Quantity DRainfall

instance (Fractional a, Eq a, d ~ DRainfall) => HasNull (Quantity d a) where
  nullValue = (-1) DP.*~ litersPerMeter

instance (Fractional a, d ~ DRainfall) => Default (Unit 'NonMetric d a) where
  def = litersPerMeter

--deriveHasUnits [t|Rainfall Double -> Quantity DRainfall Double|] 'Rainfall 'getRainfall
