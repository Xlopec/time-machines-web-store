{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NamedFieldPuns #-}

module Chapter2.DataTypesR where

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

clientNameR :: ClientR -> String
clientNameR GovOrgR {clientRName} = clientRName
clientNameR CompanyR {clientRName} = clientRName
clientNameR IndividualR { person = PersonR { firstName, lastName } } = firstName ++ " " ++ lastName
