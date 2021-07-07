{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Nftree.Contract
    ( Nft (..)
    , endpoints
    ) where

import           Control.Monad                hiding (fmap)
import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Text                    (Text, pack)
import           GHC.Generics                 (Generic)
import           Plutus.Contract              as Contract hiding (when)
import           Plutus.Contract.StateMachine
import qualified PlutusTx
import           PlutusTx.Prelude             hiding (Semigroup(..), check, unless)
import           Ledger                       hiding (singleton)
import           Ledger.Ada                   as Ada
import           Ledger.Constraints           as Constraints
import           Ledger.Typed.Tx
import qualified Ledger.Typed.Scripts         as Scripts
import           Ledger.Value
import           Playground.Contract          (ToSchema)
import           Prelude                      (Semigroup (..))
import qualified Prelude

data Nft = Nft
  { nName :: !String -- review if in mint we get this two
  , nSymbol :: !String -- review if in mint
  , nToken :: !Assetclass -- needed for statemachine? or will be generated
  , nOwners :: [Owner]
  , nBalances :: [Balance]
  , nTokenApprovals  :: [TokenApproval]
  , nOperatorApprovals  :: [OperatorApproval]
  , nTokenURIs :: [TokenURI]
} deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)

data Owner = Owner
  { oTokenId :: !String
  , oPubkey :: !PubKeyHash
  }
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Balance = Balance
  { bOwner :: !PubKeyHash
  , bCount :: !Integer
  }
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data TokenApproval = TokenApproval
  { taTokenId :: !String
  , taAddress :: !PubKeyHash
  }
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data OperatorApproval = OperatorApproval
  { oaOwnerAddress :: !PubKeyHash
  , oaOperatorAddress :: !PubKeyHash
  }
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data TokenURI :: String

