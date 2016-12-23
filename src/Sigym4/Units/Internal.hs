{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Sigym4.Units.Internal (
-- * Types
  Units
, HasUnits(..)
-- * Functions
, deriveHasUnits

-- * Re-exports
, Dimensional (..) -- Re-export constructor so coerce can find it
, Quantity
, Unit
, Metricality (..)
-- ** 'Quantity' Synonyms
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

import           Sigym4.Null
import           Control.DeepSeq (NFData(rnf))
import           Control.Newtype
import           Data.Fingerprint
import           Data.Functor.Identity
import           Data.Coerce
import           Data.ExactPi (approximateValue)
import           Foreign.Storable (Storable)
import           Numeric.Units.Dimensional as DP hiding ((*~), (/~), (+), (-), (*), (/))
import qualified Numeric.Units.Dimensional as DP
import qualified Numeric.Units.Dimensional.Coercion (Dimensional(Quantity))
import           Numeric.Units.Dimensional.UnitNames (atom)
import           Numeric.Units.Dimensional.SIUnits
import           Numeric.Units.Dimensional.NonSI
import           Language.Haskell.TH

type family Units q a :: *

class HasUnits q a | q -> a where

  infixl 7 *~
  (*~) :: a
       -> Units q a
       -> q


  infixl 7 /~
  (/~) :: q
       -> Units q a
       -> a

type instance Units (DP.Quantity u a) a = DP.Unit 'DP.NonMetric u a

instance (Num a, Fractional a) => HasUnits (DP.Quantity u a) a
  where
  (*~) = (DP.*~)
  (/~) = (DP./~)
  {-# INLINE (*~) #-}
  {-# INLINE (/~) #-}

instance Storable a => HasFingerprint (DP.Quantity u a)
instance (Floating a, HasFingerprint a) => HasFingerprint (DP.Unit k u a) where
  fingerprint u = fingerprint (approximateValue (exactValue u) :: a)

-- | Derives 'HasUnits' and 'Newtype' for a newtype of a 'Quantity'.
--
-- Usage:
--
--  >>> newtype AirTemperature a = AirTemperature (DP.Temperature a)
--  >>> deriveHasUnits [t| forall a. Num a => AirTemperature a -> DP.Temperature a|] 'AirTemperature 'getAirTemperature
deriveHasUnits :: TypeQ -> Name -> Name -> DecsQ
deriveHasUnits ta pa upa = ta >>= \case
  ForallT [ma'] cst (AppT (AppT ArrowT t') a') ->
    let sig = return $ foldl AppT (TupleT (length cst)) cst
        t = return t'
        a = return a'
        pack' = return (ConE pa)
        unpack' = return (VarE upa)
        ma = return (VarT (case ma' of {PlainTV a->a; KindedTV a _->a}))

    in [d|instance $sig => HasUnits $t $ma where
            a *~ u = $pack' (a *~ u)
            a /~ u = $unpack' a /~ u
            {-# INLINE (*~) #-}
            {-# INLINE (/~) #-}
          instance Newtype $t $a where
            pack   = $pack'
            unpack = $unpack'
          type instance Units $t $ma = Units $a $ma
      |]
  AppT (AppT ArrowT t') a'@(AppT _ ma') ->
    let t = return t'
        a = return a'
        pack' = return (ConE pa)
        unpack' = return (VarE upa)
        ma = return ma'
    in [d|instance HasUnits $t $ma where
            a *~ u = $pack' (a *~ u)
            a /~ u = $unpack' a /~ u
            {-# INLINE (*~) #-}
            {-# INLINE (/~) #-}
          instance Newtype $t $a where
            pack   = $pack'
            unpack = $unpack'
          type instance Units $t $ma = Units $a $ma
      |]
  e -> fail ("deriveHasUnits expects a type of the form: \"NewType -> UnderlyingType\", not " ++ show e)

