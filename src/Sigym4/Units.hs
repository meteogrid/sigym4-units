{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Sigym4.Units (
-- * Types
  Units
, MachineType
, HasUnits(..)

-- * Basic units
--
-- ** Ratios/Fractions/etc..
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
, module NonSI
, module SIUnits
) where

import           Sigym4.Null
import           Control.Newtype
import           Data.Functor.Identity
import           Data.Default
import           Numeric.Units.Dimensional as DP hiding ((*~), (/~))
import qualified Numeric.Units.Dimensional as DP
import           Numeric.Units.Dimensional.UnitNames (atom)
import           Numeric.Units.Dimensional.SIUnits as SIUnits
import           Numeric.Units.Dimensional.NonSI as NonSI

type family Units       q :: *
type family MachineType q :: *

class HasUnits q where


  infixl 7 *~
  (*~) :: MachineType q
       -> Units q
       -> q


  infixl 7 /~
  (/~) :: q
       -> Units q
       -> MachineType q

type instance Units       (DP.Quantity u a) = DP.Unit 'DP.NonMetric u a
type instance MachineType (DP.Quantity u a) = a

instance (Num a, Fractional a) => HasUnits (DP.Quantity u a)
  where
  (*~) = (DP.*~)
  (/~) = (DP./~)
  {-# INLINE (*~) #-}
  {-# INLINE (/~) #-}

-- | This represents a dimensionless normalized ratio in the range [0,1]
newtype NormRatio = NormRatio (DP.Dimensionless Double)
  deriving (Eq, Ord, Show, HasUnits)

type instance Units       NormRatio = Units       (DP.Dimensionless Double)
type instance MachineType NormRatio = MachineType (DP.Dimensionless Double)

instance HasNull NormRatio where
  nullValue = NormRatio ((-1) DP.*~ DP.one)


-- | Smart constructor for 'NormRatio's which verifies that the value is indeed
-- in the range [0,1].
--
-- Useful to interface with external sources

nRatio :: Double -> Maybe NormRatio
nRatio n | n>=0, n<=1 = Just (NormRatio (n DP.*~ DP.one))
nRatio _              = Nothing

-- | WARNING: This instance allows the creation of invalid values (ie: not in
-- [0,1]), but so does '*~' so we provide it anyway.
instance Newtype NormRatio (DP.Dimensionless Double) where
  pack                 = NormRatio
  unpack (NormRatio r) = r


-- | This represents a dimensionless ratio/fraction/etc...
newtype Ratio = Ratio (DP.Dimensionless Double)
  deriving (Eq, Ord, Show, HasUnits)

perCent :: Fractional a => Unit 'NonMetric DOne a
perCent = mkUnitQ name 1e-2 one
  where name = atom "[%]" "%" "Per cent"

perOne :: Fractional a => Unit 'NonMetric DOne a
perOne = mkUnitQ name 1.0 one
  where name = atom "[one]" "one" "Ratio"

type instance Units       Ratio = Units       (DP.Dimensionless Double)
type instance MachineType Ratio = MachineType (DP.Dimensionless Double)

instance Newtype Ratio (DP.Dimensionless Double) where
  pack             = Ratio
  unpack (Ratio r) = r


-- | This represents a height above sea level
newtype Height = Height (DP.Quantity DP.DLength Double)
  deriving (Eq, Ord, Show, HasUnits)

type instance Units       Height = Units       (DP.Quantity DP.DLength Double)
type instance MachineType Height = MachineType (DP.Quantity DP.DLength Double)

instance HasNull Height where
  nullValue = Height ((-9999) DP.*~ meter)

instance Newtype Height (DP.Quantity DP.DLength Double) where
  pack              = Height
  unpack (Height r) = r


--
-- | This represents a distance
newtype Distance = Distance (DP.Quantity DP.DLength Double)
  deriving (Eq, Ord, Show, HasUnits)

type instance Units       Distance = Units       (DP.Quantity DP.DLength Double)
type instance MachineType Distance = MachineType (DP.Quantity DP.DLength Double)

instance HasNull Distance where
  nullValue = Distance ((-9999) DP.*~ meter)

instance Newtype Distance (DP.Quantity DP.DLength Double) where
  pack                = Distance
  unpack (Distance r) = r
