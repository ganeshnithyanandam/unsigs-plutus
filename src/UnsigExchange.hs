{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module UnsigExchange
    ( RequestParam
    , UnsigExchangeSchema
    , Text
    , endpoints
    ) where

import Control.Lens (view)
import           Control.Monad                hiding (fmap)
import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Map             as Map
import           Data.Monoid                  (Last (..))
import           Data.Text                    (Text, pack)
import           GHC.Generics                 (Generic)
import           Ledger                       hiding (singleton)
import           Ledger.Ada                   as Ada
import           Ledger.Constraints           as Constraints
import           Ledger.Constraints.TxConstraints as TxConstraints
import           Ledger.Typed.Tx
import qualified Ledger.Typed.Scripts         as Scripts
import qualified PlutusTx.Builtins            as Builtins
import PlutusTx.Builtins.Class
import           Plutus.Contract              as Contract
import qualified PlutusTx
import           PlutusTx.Prelude             hiding (Semigroup(..), check, unless, map, filter)
import qualified Plutus.V1.Ledger.Value    as Value
import           Plutus.V1.Ledger.Value
import           Playground.Contract          (ToSchema)
import           Prelude                      (Semigroup (..), Show (..), String)
import qualified Prelude

data UnsigRedeemer =
    Offer
  | Bid
  | Sell
  deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)

{-Is there a better way to do makeIsDataIndexed ?-}
PlutusTx.makeLift ''UnsigRedeemer
PlutusTx.makeIsDataIndexed ''UnsigRedeemer [('Offer, 0), ('Bid, 1), ('Sell, 2)]

data TradeDetails = TradeDetails
  { traderRef  :: !PubKeyHash
  , unsigId    :: !BuiltinByteString
  , adaAmount  :: !Integer
  } deriving (Show, Generic, ToJSON, FromJSON)

data UnsigDatum = Offered TradeDetails | Bidding TradeDetails | Sold
    deriving (Show)

instance Eq UnsigDatum where
  (==) (Offered a) (Offered b) = a == b
  (==) (Bidding a) (Bidding b) = a == b
  (==) Sold Sold = True
  (==) _ _ = False

PlutusTx.makeIsDataIndexed ''UnsigDatum [('Offered, 0), ('Bidding, 1), ('Sold, 2)]

{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

data ContractInfo = ContractInfo
    { policyUnsig     :: !CurrencySymbol
    , policyBid       :: !CurrencySymbol
    , prefixUnsig     :: !BuiltinByteString
    , prefixUnsigBid  :: !BuiltinByteString
    , minPrice        :: !Integer
    } deriving (Generic, ToJSON, FromJSON)

mkValidator = ContractInfo
    { policyUnsig = "d5e6bf0500378d4f0da4e8dde6becec7621cd8cbf5cbb9b87013d4cc"
    , policyBid = "800df05a0cc6b6f0d28aaa1812135bd9eebfbf5e8e80fd47da9989eb"
    , prefixUnsig = "Unsig"
    , prefixUnsigBid = "UnsigBid"
    {-, owner1 = ("826d9fafe1b3acf15bd250de69c04e3fc92c4493785939e069932e89", 416, 625) -- 2.4% 1.6%
    , owner2 = ("88269f8b051a739300fe743a7b315026f4614ce1216a4bb45d7fd0f5", 2500) -- 0.4%
    , extraRecipient = 2500 -- 0.4%-}
    , minPrice = 70000000
    {-, bidStep = 10000-}
    }

{-# INLINABLE unsigExchangeValidator #-}
unsigExchangeValidator :: ContractInfo -> UnsigDatum -> UnsigRedeemer -> ScriptContext -> Bool
unsigExchangeValidator u d r ctx =
  case r of
     Bid -> True
     
     Sell -> isSignedByNFTOwner info

    where
      info :: TxInfo
      info = scriptContextTxInfo ctx

      hasSufficientAmount :: Bool
      hasSufficientAmount =
          traceIfFalse "Sorry. Not enough lovelace" $ checkAmount $ inValue ctx

{-# INLINABLE isSignedByNFTOwner #-}
isSignedByNFTOwner :: TxInfo -> Bool
isSignedByNFTOwner info =
  let nftScriptInputs = [i | i <- filter (isJust . toValidatorHash . txOutAddress) (txInInfoResolved <$> txInfoInputs info), isOfUnsigPolicy txOutValue i]
  in
    length nftScriptInputs == 1 &&
    case findDatumValue info (head nftScriptInputs) of
      Just tradeDetails -> do
                        let TradeDetails{traderRef=a} = tradeDetails
                        txSignedBy info a
      _                 -> False



data Typed
instance Scripts.ValidatorTypes Typed where
    type instance DatumType Typed    = UnsigDatum
    type instance RedeemerType Typed = UnsigRedeemer

typedValidator :: Scripts.TypedValidator Typed
typedValidator = Scripts.mkTypedValidator @Typed
    ($$(PlutusTx.compile [|| unsigExchangeValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode contractInfo)
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @UnsigDatum @UnsigRedeemer

validator :: Validator
validator = Scripts.validatorScript typedValidator

{-
valHash :: Unsigs -> Ledger.ValidatorHash
valHash = Scripts.validatorHash . typedValidator
-}

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

--Helper functions start
isUnsigNFT :: Value -> String -> Bool
isUnsigNFT v unsigId = valueOf v (policyUnsig contractInfo) (TokenName ((prefixUnsig contractInfo) <> unsigId)) >= 1

{-# INLINABLE isOfUnsigPolicy #-}
isOfUnsigPolicy :: Value -> Bool
isOfUnsigPolicy v  = symbols v == [policyUnsig contract]

isUnsigBidNFT :: Value -> String -> Bool
isUnsigBidNFT v unsigId = valueOf v (policyUnsigBid contractInfo) (TokenName ((prefixUnsigBid contractInfo) <> unsigId)) >= 1
--Helper functions end


data RequestParam = RequestParam
  { nftId     :: !String
  , amount :: !Integer
  } deriving (Generic, ToJSON, FromJSON, ToSchema)
    
offer :: AsContractError e => RequestParam -> Contract w s e ()
offer param =
  do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    let unsigId = nftId param
        tradeDatum = Offered TradeDetails {traderRef = pkh, unsigId = unsigId, adaAmount = amount param}
        tx = mustPayToTheScript tradeDatum (Value.singleton (policyUnsig contractInfo) (TokenName (prefixUnsig contractInfo <> unsigId)) 1)
    void $ submitTxConstraints typedValidator tx
    logInfo @String $ "Offer made - Submitted unsig NFT to script" <> show $ amount param

bid :: AsContractError e => RequestParam -> Contract w s e ()
bid param =
  do
    os <- map snd . Map.toList <$> utxosAt $ scrAddress ContractInfo
    pkh <- pubKeyHash <$> Contract.ownPubKey
    let unsigId      = nftId param
        unsigValue    = find (isUnsigNFT $ (view ciTxOutValue) unsigId) os
        unsigBidValue = find (isUnsigBidNFT $ (view ciTxOutValue unsigId)) os
    previousBidRef <- case unsigBidValue of
                          Just o -> datumFromHash either nftId Ledger.datumHash (view ciTxOutDatum o)
                          _      -> return Nothing

    case previousBidRef of
      Nothing -> do 
                  let tradeDatum = Bidding TradeDetails {traderRef = pkh, unsigId = nftId param, adaAmount = amount param}
                      valToScript = (Value.singleton ("policyUnsigBid") (TokenName $ stringToBuiltinByteString ("UnsigBidTokenName#" <> nftId)) 1) <>
                                   (lovelaceValueOf $ amount param)
                      tx = mustPayToTheScript tradeDatum valToScript
                  void $ submitTxConstraints typedValidator tx
                  logInfo @String $ "Bid made - Submitted lovelaces to script" <> show $ amount param

      Just TradeDetails{traderRef=a,unsigId=b,adaAmount=c} -> do
                  if c > amount param then logInfo @String $ "Insufficient bid" else do
                    let tradeDatum = Bidding TradeDetails {traderRef = pkh, unsigId = nftId param, adaAmount = amount param}
                        valToScript = (Value.singleton ("policyUnsigBid") (TokenName $ stringToBuiltinByteString ("UnsigBidTokenName#" <> nftId)) 1) <>
                                     (lovelaceValueOf $ amount param)
                        tx = collectFromScriptFilter isUnsigBidNFT os Bid <>
                             mustPayToTheScript tradeDatum valToScript <>
                             mustPayToPubKey a unsigBidValue
                    void $ submitTxConstraints typedValidator tx
                    logInfo @String $ "Bid made - Submitted lovelaces to script" <> show $ amount param

sell :: AsContractError e => RequestParam -> Contract w s e ()
sell param =
  do
    utxos <- utxosAt $ scrAddress ContractInfo
    os <- map snd . Map.toList <$> utxos
    pkh <- pubKeyHash <$> Contract.ownPubKey
    let unsigId      = nftId param
        unsigValue    = _ciTxOutValue $ find (isUnsigNFT $ (view ciTxOutValue) unsigId) os
        unsigBidValue = _ciTxOutValue $ find (isUnsigBidNFT $ (view ciTxOutValue unsigId)) os
    previousBidRef <- case unsigBidValue of
                          Just o -> datumFromHash $ either id Ledger.datumHash (view ciTxOutDatum o)
                          _      -> return Nothing

    case previousBidRef of
      Nothing -> logInfo @String $ "No bids recevied"

      Just TradeDetails{traderRef=a,unsigId=b,adaAmount=c} -> do
                 let tx = collectFromScript utxos Sell <>
                      mustPayToPubKey a unsigValue <>
                      mustPayToPubKey a (Value.singleton ("policyUnsigBid") (TokenName $ stringToBuiltinByteString ("UnsigBidTokenName#" <> unsigId)) 1)
                 void $ submitTxConstraints typedValidator tx
                 logInfo @String $ "Bid made - Submitted lovelaces to script" <> show $ amount param

type UnsigExchangeSchema = Endpoint "offer" RequestParam
                  .\/ Endpoint "bid" RequestParam
                  .\/ Endpoint "sell" RequestParam

offer' :: Promise () UnsigExchangeSchema Text ()
offer' = endpoint @"offer" offer

bid' :: Promise () UnsigExchangeSchema Text ()
bid' = endpoint @"bid" bid

sell' :: Promise () UnsigExchangeSchema Text ()
sell' = endpoint @"bid" bid

endpoints :: AsContractError e => Contract () UnsigExchangeSchema Text e
endpoints =
  do
    logInfo @String "Waiting for endpoing to be invoked."
    selectList [offer',bid',sell'] >>  endpoints



