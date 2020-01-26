{-# OPTIONS_GHC -Wall #-}

module DataTypes where

data ClientR
  = GovOrgR
      { clientRName :: String
      }
  | CompanyR
      { clientRName :: String
      , companyId :: Integer
      , person :: PersonR
      , duty :: String
      }
  | IndividualR
      { person :: PersonR
      }
  deriving (Show)

data PersonR =
  PersonR
    { firstName :: String
    , lastName :: String
    }
  deriving (Show)
