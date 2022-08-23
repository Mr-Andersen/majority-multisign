{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-specialise #-}

module MajorityMultiSign.OnChain (
  checkMultisigned,
  mkValidator,
  validator,
  validatorAddress,
  validatorFromIdentifier,
  validatorHash,
  validatorHashFromIdentifier,
) where

import Data.List.Extra (firstJust)
import Ledger (PaymentPubKeyHash (unPaymentPubKeyHash))
import MajorityMultiSign.Schema (
  -- MajorityMultiSign,
  MajorityMultiSignDatum (MajorityMultiSignDatum, signers),
  MajorityMultiSignIdentifier (MajorityMultiSignIdentifier, asset),
  MajorityMultiSignRedeemer (UpdateKeysAct, UseSignaturesAct),
  MajorityMultiSignValidatorParams (MajorityMultiSignValidatorParams, asset),
  getMinSigners,
  maximumSigners,
 )
import Plutus.Script.Utils.V2.Address (mkValidatorAddress)
import Plutus.Script.Utils.V2.Scripts (ValidatorHash)
import Plutus.Script.Utils.V2.Scripts qualified as Scripts (validatorHash)
import Plutus.Script.Utils.V2.Typed.Scripts.Validators (mkUntypedValidator)
import Plutus.V1.Ledger.Value (assetClassValueOf)
import Plutus.V2.Ledger.Api (Address, Datum(Datum), OutputDatum(OutputDatum), Validator, ScriptContext (scriptContextTxInfo), TxOut (txOutDatum, txOutValue), mkValidatorScript)
import Plutus.V2.Ledger.Contexts (TxInInfo (txInInfoResolved), TxInfo (txInfoInputs), getContinuingOutputs, txSignedBy)
import PlutusTx qualified
--import PlutusTx.List.Natural qualified as Natural
--import PlutusTx.Natural (Natural)
import PlutusTx.Prelude

{-# INLINEABLE mkValidator #-}
mkValidator ::
  MajorityMultiSignValidatorParams ->
  MajorityMultiSignDatum ->
  MajorityMultiSignRedeemer ->
  ScriptContext ->
  Bool
mkValidator params dat red ctx =
  hasCorrectToken params ctx (getExpectedDatum red dat)
    && isSufficientlySigned red dat ctx
    && isUnderSizeLimit red dat

{-# INLINEABLE removeSigners #-}
removeSigners :: [PaymentPubKeyHash] -> [PaymentPubKeyHash] -> [PaymentPubKeyHash]
removeSigners [] _ = []
removeSigners xs [] = xs -- Not strictly needed, but more efficient
removeSigners (x : xs) ys = if x `elem` ys then removeSigners xs ys else x : removeSigners xs ys

-- | Calculates the expected output datum from the current datum and the redeemer
{-# INLINEABLE getExpectedDatum #-}
getExpectedDatum :: MajorityMultiSignRedeemer -> MajorityMultiSignDatum -> MajorityMultiSignDatum
getExpectedDatum UseSignaturesAct datum = datum
getExpectedDatum (UpdateKeysAct keys) datum = datum {signers = keys}

-- | Checks if, when setting new signatures, all new keys have signed the transaction
{-# INLINEABLE hasNewSignatures #-}
hasNewSignatures :: MajorityMultiSignRedeemer -> MajorityMultiSignDatum -> ScriptContext -> Bool
hasNewSignatures UseSignaturesAct _ _ = True
hasNewSignatures (UpdateKeysAct keys) MajorityMultiSignDatum {signers} ctx =
  all (txSignedBy (scriptContextTxInfo ctx) . unPaymentPubKeyHash) $ keys `removeSigners` signers

-- | Checks the script has the correct token (containing the asset we want), forwards it to the right place, and has the datum we expect
{-# INLINEABLE hasCorrectToken #-}
hasCorrectToken :: MajorityMultiSignValidatorParams -> ScriptContext -> MajorityMultiSignDatum -> Bool
hasCorrectToken MajorityMultiSignValidatorParams {asset} ctx expectedDatum =
  isJust result
  where
    continuing :: [TxOut]
    continuing = getContinuingOutputs ctx

    checkAsset :: TxOut -> Maybe TxOut
    checkAsset txOut = if assetClassValueOf (txOutValue txOut) asset > 0 then Just txOut else Nothing

    traceIfNothing :: BuiltinString -> Maybe a -> Maybe a
    traceIfNothing errMsg = maybe (trace errMsg Nothing) Just

    result :: Maybe ()
    result = do
      assetTxOut <- traceIfNothing "Couldn't find asset" $ firstJust checkAsset continuing
      datum <- traceIfNothing "Continuing output does not have inline datum" $
            case txOutDatum assetTxOut of
              OutputDatum datum -> Just datum
              _ -> Nothing
      let datumMatches = traceIfFalse "Incorrect output datum" $ datum == Datum (PlutusTx.toBuiltinData expectedDatum)
      if datumMatches then Just () else Nothing

-- | External function called by other contracts to ensure multisigs present
{-# INLINEABLE checkMultisigned #-}
checkMultisigned :: MajorityMultiSignIdentifier -> ScriptContext -> Bool
checkMultisigned MajorityMultiSignIdentifier {asset} ctx =
  traceIfFalse "Missing Multisign Asset" $
    any containsAsset inputs
  where
    inputs :: [TxInInfo]
    inputs = txInfoInputs $ scriptContextTxInfo ctx

    containsAsset :: TxInInfo -> Bool
    containsAsset = (> 0) . flip assetClassValueOf asset . txOutValue . txInInfoResolved

-- | Checks the validator is signed by more than half of the signers on the datum
{-# INLINEABLE isSufficientlySigned #-}
isSufficientlySigned :: MajorityMultiSignRedeemer -> MajorityMultiSignDatum -> ScriptContext -> Bool
isSufficientlySigned red dat@MajorityMultiSignDatum {signers} ctx =
  traceIfFalse "Not enough signatures" (length signersPresent >= minSigners)
    && traceIfFalse "Missing signatures from new keys" (hasNewSignatures red dat ctx)
  where
    signersPresent, signersUnique :: [PaymentPubKeyHash]
    signersPresent = filter (txSignedBy (scriptContextTxInfo ctx) . unPaymentPubKeyHash) signersUnique
    signersUnique = nub signers
    minSigners :: Integer 
    minSigners = getMinSigners signersUnique

-- | Checks the validator datum fits under the size limit
{-# INLINEABLE isUnderSizeLimit #-}
isUnderSizeLimit :: MajorityMultiSignRedeemer -> MajorityMultiSignDatum -> Bool
isUnderSizeLimit UseSignaturesAct MajorityMultiSignDatum {signers} =
  traceIfFalse "Datum too large" (length signers <= maximumSigners)
isUnderSizeLimit (UpdateKeysAct keys) MajorityMultiSignDatum {signers} =
  traceIfFalse "Datum too large" (length signers <= maximumSigners)
    && traceIfFalse "Redeemer too large" (length keys <= maximumSigners)

validator :: MajorityMultiSignValidatorParams -> Validator
validator params = mkValidatorScript untyped
  where
    untyped = ($$(PlutusTx.compile [||mkUntypedValidator . mkValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode params)

validatorHash :: MajorityMultiSignValidatorParams -> ValidatorHash
validatorHash = Scripts.validatorHash . validator

validatorAddress :: MajorityMultiSignValidatorParams -> Address
validatorAddress = mkValidatorAddress . validator

-- | Gets the validator from an identifier
validatorFromIdentifier :: MajorityMultiSignIdentifier -> Validator
validatorFromIdentifier MajorityMultiSignIdentifier {asset} = validator $ MajorityMultiSignValidatorParams asset

-- | Gets the validator hash from an identifier
validatorHashFromIdentifier :: MajorityMultiSignIdentifier -> ValidatorHash
validatorHashFromIdentifier MajorityMultiSignIdentifier {asset} = validatorHash $ MajorityMultiSignValidatorParams asset
