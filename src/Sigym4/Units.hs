{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
module Sigym4.Units (
-- * Types
  Units
, HasUnits(..)
, deriveHasUnits

-- * Basic units
--
-- ** Ratios\/ Fractions\/etc...
-- *** 'Ratio'
, Ratio (..)

-- *** 'NormRatio'
, NormRatio (..)
, nRatio

-- *** Common ratio units
, perCent
, perOne

-- ** 'Height'
, Height (..)

-- ** 'Distance'
, Distance (..)


-- * Re-exports
-- ** Types
, Dimensional (..) -- Re-export constructor so coerce can find it
, Quantity
, Unit
, Metricality (..)
-- ** Quantity Synonyms
, Dimensionless
, Length
, Mass
, Time
, ElectricCurrent
, ThermodynamicTemperature
, AmountOfSubstance
, LuminousIntensity
-- ** 'Dimension' Synonyms
, DOne
, DLength
, DMass
, DTime
, DElectricCurrent
, DThermodynamicTemperature
, DAmountOfSubstance
, DLuminousIntensity
-- ** Functions
, weaken
-- ** Modules
, module Numeric.Units.Dimensional.SIUnits
, module Numeric.Units.Dimensional.NonSI
) where

import           Sigym4.Units.Internal
import           Sigym4.Null
import           Control.DeepSeq (NFData(rnf))
import           Control.Newtype
import           Data.Functor.Identity
import           Data.Default
import           Foreign.Storable (Storable)
import           Numeric.Units.Dimensional as DP hiding ((*~), (/~), (+), (-), (*), (/))
import qualified Numeric.Units.Dimensional as DP
import qualified Numeric.Units.Dimensional.Coercion (Dimensional(Quantity))
import           Numeric.Units.Dimensional.UnitNames (atom)
import           Numeric.Units.Dimensional.SIUnits
import           Numeric.Units.Dimensional.NonSI

-- | This represents a dimensionless normalized ratio in the range [0,1]
newtype NormRatio a = NormRatio { getNormRatio :: DP.Dimensionless a }
  deriving (Eq, Ord, Show, Storable, NFData)

instance (Fractional a, Eq a) => HasNull (NormRatio a) where
  nullValue = NormRatio ((-1) DP.*~ DP.one)

deriveHasUnits [t|forall a. (Fractional a, Eq a) => NormRatio a -> DP.Dimensionless a|] 'NormRatio 'getNormRatio


-- | Smart constructor for 'NormRatio's which verifies that the value is indeed
-- in the range [0,1].
--
-- Useful to interface with external sources

nRatio :: (Num a, Ord a) => a -> Maybe (NormRatio a)
nRatio n | n>=0, n<=1 = Just (NormRatio (n DP.*~ DP.one))
nRatio _              = Nothing

-- | This represents a dimensionless ratio/fraction/etc...
newtype Ratio a = Ratio { getRatio :: DP.Dimensionless a }
  deriving (Eq, Ord, Show, Storable, NFData)

deriveHasUnits [t|forall a. (Fractional a, Eq a) => Ratio a -> DP.Dimensionless a|] 'Ratio 'getRatio


-- | This represents a height above sea level
newtype Height a = Height { getHeight :: DP.Quantity DP.DLength a }
  deriving (Eq, Ord, Show, Storable, NFData)

deriveHasUnits [t|forall a. (Fractional a, Eq a) => Height a -> DP.Length a|] 'Height 'getHeight

instance (Eq a, Fractional a) => HasNull (Height a) where
  nullValue = Height ((-9999) DP.*~ meter)


--
-- | This represents a distance
newtype Distance a = Distance { getDistance :: DP.Quantity DP.DLength a }
  deriving (Eq, Ord, Show, Storable, NFData)

deriveHasUnits [t|forall a. (Fractional a, Eq a) => Distance a -> DP.Length a|] 'Distance 'getDistance

instance (Eq a, Fractional a) => HasNull (Distance a) where
  nullValue = Distance ((-9999) DP.*~ meter)

instance Fractional a => Default (Unit 'NonMetric DOne a) where
  def = perOne

instance Fractional a => Default (Unit 'Metric DLength a) where
  def = meter

instance Fractional a => Default (Unit 'NonMetric DLength a) where
  def = weaken meter

perCent :: Fractional a => Unit 'NonMetric DOne a
perCent = mkUnitQ name 1e-2 one
  where name = atom "[%]" "%" "Per cent"

perOne :: Fractional a => Unit 'NonMetric DOne a
perOne = mkUnitQ name 1.0 one
  where name = atom "[one]" "one" "Ratio"
