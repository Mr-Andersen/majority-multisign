{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
-- record-dot-preprocessor creates code that violates this warning, disable for this file
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# OPTIONS_GHC -fno-specialise #-}

module MajorityMultiSign.Schema (
  MajorityMultiSign,
  MajorityMultiSignDatum (..),
  MajorityMultiSignIdentifier (..),
  MajorityMultiSignRedeemer (..),
  MajorityMultiSignSchema,
  MajorityMultiSignValidatorParams (..),
  SetSignaturesParams (..),
  getMinSigners,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Kind (Type)
import GHC.Generics (Generic)
import Ledger.Crypto (PubKey)
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract (Endpoint, type (.\/))
import Plutus.V1.Ledger.Api (PubKeyHash)
import Plutus.V1.Ledger.Scripts (ValidatorHash)
import Plutus.V1.Ledger.Value (AssetClass)
import PlutusTx qualified
import PlutusTx.NatRatio (NatRatio, ceiling, fromNatural, natRatio)
import PlutusTx.Natural (Natural)
import PlutusTx.Prelude hiding (Eq)
import Prelude (Eq, Show)

{-# INLINEABLE fromJust #-}

-- | Inlineable fromJust
fromJust :: forall (a :: Type). Maybe a -> a
fromJust Nothing = error ()
fromJust (Just x) = x

{-# INLINEABLE signReq #-}

{- | Signing proportion required
 Known to be well formed at compile time, thus the fromJust
-}
signReq :: NatRatio
signReq = fromJust $ natRatio (toEnum 1) (toEnum 2)

{-# INLINEABLE intToNatRatio #-}
intToNatRatio :: Integer -> NatRatio
intToNatRatio = fromNatural . toEnum @Natural

{-# INLINEABLE ceilNatRatioToInt #-}
ceilNatRatioToInt :: NatRatio -> Integer
ceilNatRatioToInt = fromEnum . ceiling

{-# INLINEABLE getMinSigners #-}

-- | Given a list of Signers, gets the minimum number of signers needed for a transaction to be valid
getMinSigners :: [a] -> Integer
getMinSigners = ceilNatRatioToInt . (signReq *) . intToNatRatio . length

-- | Data type used to identify a majority multisign validator (the validator itself and the asset needed to call it)
data MajorityMultiSignIdentifier = MajorityMultiSignIdentifier
  { address :: ValidatorHash
  , asset :: AssetClass
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.unstableMakeIsData ''MajorityMultiSignIdentifier

-- | Params to the majority multisign validator, as the asset class of the `MajorityMultiSignDatum`
data MajorityMultiSignValidatorParams = MajorityMultiSignValidatorParams
  { asset :: AssetClass
  }

PlutusTx.makeLift ''MajorityMultiSignValidatorParams

{- | Datum held by the validator, storing the pub keys of the signatures needed
  This is also used as the params to the initialize endpoint
-}
data MajorityMultiSignDatum = MajorityMultiSignDatum
  { signers :: [PubKeyHash]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.unstableMakeIsData ''MajorityMultiSignDatum

-- | Redeemer of the validator, allowing for simple use (not modifying datum), or key updating
data MajorityMultiSignRedeemer
  = UseSignaturesAct
  | UpdateKeysAct
      { keys :: [PubKeyHash]
      }
  deriving stock (Eq, Show)

PlutusTx.unstableMakeIsData ''MajorityMultiSignRedeemer

-- | Params to the set signature endpoint
data SetSignaturesParams = SetSignaturesParams
  { mmsIdentifier :: MajorityMultiSignIdentifier
  , newKeys :: [PubKeyHash]
  , pubKeys :: [PubKey]
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.unstableMakeIsData ''SetSignaturesParams

data MajorityMultiSign

instance Scripts.ValidatorTypes MajorityMultiSign where
  type DatumType MajorityMultiSign = MajorityMultiSignDatum
  type RedeemerType MajorityMultiSign = MajorityMultiSignRedeemer

type MajorityMultiSignSchema =
  Endpoint "Initialize" MajorityMultiSignDatum
    .\/ Endpoint "SetSignatures" SetSignaturesParams