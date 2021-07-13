
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Nftree.Contract where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.List.NonEmpty   (NonEmpty (..))
import           Data.Map             as Map
import qualified Data.List            as List
import           Data.Text            (pack, Text)
import           GHC.Generics         (Generic)
import           Plutus.Contract
import qualified PlutusTx             as PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import qualified PlutusTx.Prelude     as Plutus
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Scripts       as Scripts
import qualified Ledger.Typed.Scripts as Scripts hiding (validatorHash)
import           Ledger.Value         as Value
import           Ledger.Ada           as Ada
import           Playground.Contract  (ensureKnownCurrencies, printSchemas, stage, printJson)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (IO, Semigroup (..), Show (..), String)
import           Schema               (ToSchema)
import           Text.Printf          (printf)
import Control.Lens

data Nft = Nft
  { nName :: !String -- review if in mint we get this two
  , nSymbol :: !String -- review if in mint
  , nToken :: !String -- Should be AssetClass
  , nOwners :: (Map NTokenId NAddress)
  , nBalances :: (Map NAddress NTokenId)
  , nTokenApprovals  :: (Map NTokenId NAddress)
  , nOperatorApprovals  :: (Map NAddress NAddress)
  , nTokenURIs :: Map NTokenId NUrl
} deriving (Show, Generic, ToJSON, FromJSON)

type NAddress = String
type NTokenId = Integer
type NUrl = String

initialState :: NAddress -> [(NTokenId, NUrl)] -> Nft
initialState owner tokens = 
    Nft { 
        nName = "nName" 
      , nSymbol = "!String" -- review if in mint
      , nToken = "AssetClass" -- needed for statemachine? or will be generated
      , nOwners = tokenOwners
      , nBalances = Map.fromList [(owner, length tokens)]
      , nTokenApprovals  = Map.fromList []
      , nOperatorApprovals  = Map.fromList []
      , nTokenURIs = Map.fromList tokens
    }
  where
    tokenOwners = Map.fromList $ List.map (\(tokenId, _url) -> (tokenId, owner)) tokens

            

addTokenApproval :: Nft -> NTokenId -> NAddress -> Nft
addTokenApproval nft@Nft{nTokenApprovals = old } tokenId approved = nft { nTokenApprovals = Map.insert tokenId approved old}

addOperatorApproval :: Nft -> NAddress -> NAddress -> Nft
addOperatorApproval nft@Nft{nOperatorApprovals = old } owner approved = nft { nOperatorApprovals = Map.insert owner approved old}

-- transferToken :: Nft -> NTokenId -> NAddress -> Nft
-- transferToken nft@Nft{nOwners = owners} tokenId address do




