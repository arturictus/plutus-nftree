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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- import           Prelude                      (Semigroup (..))
-- import qualified Prelude

data Nft = Nft
  { nName :: !String -- review if in mint we get this two
  , nSymbol :: !String -- review if in mint
  , nToken :: !Assetclass -- needed for statemachine? or will be generated
  , nOwners :: [Owner]
  , nBalances :: [Balance]
  , nTokenApprovals  :: [TokenApproval]
  , nOperatorApprovals  :: [OperatorApproval]
  , nTokenURIs :: [String]
} deriving (Show, Generic, Eq, Ord)

data Owner = Owner
  { oTokenId :: !String
  , oPubkey :: !String
  }
  deriving (Show, Generic, Eq, Ord)

data Balance = Balance
  { bOwner :: !String
  , bCount :: !Integer
  }
  deriving (Show, Generic, Eq, Ord)

data TokenApproval = TokenApproval
  { taTokenId :: !String
  , taAddress :: !String
  }
  deriving (Show, Generic, Eq, Ord)

data OperatorApproval = OperatorApproval
  { oaOwnerAddress :: !String
  , oaOperatorAddress :: !String
  }
  deriving (Show, Generic, Eq, Ord)
  -- deriving anyclass (ToJSON, FromJSON)

{- newtype String = String { getString :: ByteString }
    deriving stock (Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON, Newtype, ToJSONKey, FromJSONKey, NFData)
    deriving newtype (P.Eq, P.Ord, Serialise, Hashable) -}

-- type TokenURI = String
