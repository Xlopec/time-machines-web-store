{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Chapter6.Lens where

import qualified Chapter2.DataTypesR as R
import Lens.Micro.Platform

data Client0 i
  = GovOrg0 i String
  | Company0 String i Person String
  | Individual0 i Person Bool
  deriving (Show)

testPerson = R.Person "Max" "Oliinyk"

testPerson' = Person "Max" "Oliinyk"

firstName' :: Lens' R.Person String
firstName' = lens (\(R.Person f _) -> f) (\(R.Person _ l) newF -> R.Person newF l)

lastName' :: Lens' R.Person String
lastName' = lens (\(R.Person _ l) -> l) (\(R.Person f _) newL -> R.Person f newL)

identifier' :: Lens (Client0 i) (Client0 j) i j
identifier' =
  lens
    (\case
       (GovOrg0 id _) -> id
       (Company0 _ id _ _) -> id
       (Individual0 id _ _) -> id)
    (\client newId ->
       case client of
         GovOrg0 _ name -> GovOrg0 newId name
         Company0 name _ person responsibility -> Company0 name newId person responsibility
         Individual0 _ person isDirector -> Individual0 newId person isDirector)

-- generated lenses
data Client i
  = GovOrg
      { _identifier :: i
      , _name :: String
      }
  | Company
      { _identifier :: i
      , _name :: String
      , _person :: Person
      , _duty :: String
      }
  | Individual
      { _identifier :: i
      , _person :: Person
      }
  deriving (Show)

data Person =
  Person
    { _firstName :: String
    , _lastName :: String
    }
  deriving (Show)

makeLenses ''Person

data TimeMachine0 =
  TimeMachine0
    { _i :: Integer
    , _n :: String
    , _p :: Double
    , _d :: R.TravelDirection
    }
  deriving (Show)

makeLenses ''TimeMachine0

increasePriceBy :: [TimeMachine0] -> Int -> [TimeMachine0]
increasePriceBy l percent = l & traversed . p %~ (\x -> x * (1.0 + (fromIntegral percent / 100.0)))

testMachines = [TimeMachine0 {_i = 10, _n = "Some", _p = 100.0, _d = R.Future}]