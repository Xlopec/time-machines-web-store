{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Chapter2.DataTypesR where

import qualified Chapter4.TypeClasses as TC

data TravelDirection
  = Past
  | Future
  deriving (Show)

data Client i
  = GovOrg
      { id :: i
      , clientName :: String
      }
  | Company
      { clientName :: String
      , id :: i
      , person :: Person
      , duty :: String
      }
  | Individual
      { id :: i
      , person :: Person
      }
  deriving (Show)

data Person =
  Person
    { firstName :: String
    , lastName :: String
    }
  deriving (Show)

data TimeMachine =
  TimeMachine
    { i :: Integer
    , name :: String
    , price :: Double
    , direction :: TravelDirection
    }
  deriving (Show)

clientNameR :: Client i -> String
clientNameR GovOrg {clientName} = clientName
clientNameR Company {clientName} = clientName
clientNameR Individual {person = Person {firstName, lastName}} = firstName ++ " " ++ lastName

-- Yeap, I know Haskell can derive Eq automatically
instance Eq Person where
  (==) Person {firstName = lfn, lastName = lln} Person {firstName = rfn, lastName = rln} = lfn == rfn && lln == rln

instance Eq i => Eq (Client i) where
  (==) GovOrg {clientName = cnl} GovOrg {clientName = cnr} = cnl == cnr
  (==) Company {clientName = cnl, id = il, person = pl, duty = dl} Company {clientName = cnr, id = ir, person = pr, duty = dr} =
    cnl == cnr && il == ir && pl == pr && dl == dr
  (==) Individual {person = pl} Individual {person = pr} = pl == pr
  (==) _ _ = False

instance TC.Nameable (Client i) where
  name GovOrg {clientName} = clientName
  name Company {clientName} = clientName
  name Individual {person = Person {firstName, lastName}} = firstName ++ " " ++ lastName

instance Ord i => Ord (Client i) where
  compare x y =
    if TC.name x /= TC.name y
      then compare (TC.name x) (TC.name y)
      else cmp x y
    where
      cmp Individual {} Individual {} = EQ
      cmp Company {} Company {} = EQ
      cmp GovOrg {} GovOrg {} = EQ
      cmp _ _ = LT

instance TC.Priceable TimeMachine where
  price TimeMachine {..} = price

totalPrice :: TC.Priceable p => [p] -> Double
totalPrice = foldl (\acc x -> (+) acc $ TC.price x) 0.0

{--Test data--}
testInd :: Client Int
testInd = Individual {id = 1, person = Person {firstName = "Max", lastName = "Lol"}}

testGov :: Client Int
testGov = GovOrg {id = 1, clientName = "Uk"}