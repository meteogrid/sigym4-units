{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Sigym4.Units.Meteo (
  CloudCover
--, RelativeHumidity
--, AirTemperature
) where


import           Sigym4.Units
import           Sigym4.Null
import           Control.Newtype
import           Data.Functor.Identity

-- | This represents the cloud coverage normalized ratio
newtype CloudCover = CC NormRatio
  deriving (Eq, Ord, Show, HasUnits, HasNull)

type instance Units       CloudCover = Units NormRatio
type instance MachineType CloudCover = MachineType NormRatio
instance Newtype CloudCover NormRatio where
  pack          = CC
  unpack (CC r) = r
