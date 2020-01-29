{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Chapter2.DataTypes where

data Client
  = GovOrg String
  | Company String Integer Person String
  | Individual Person Bool
  deriving (Show)

data Person =
  Person String String Gender
  deriving (Show)

data Gender
  = Male
  | Female
  | Unknown
  deriving (Show)

data TimeMachine =
  TimeMachine
    { i :: Integer
    , name :: String
    , price :: Float
    , direction :: TravelDirection
    }
  deriving (Show)

data TravelDirection
  = Past
  | Future
  deriving (Show)

clientName :: Client -> String
clientName client =
  case client of
    GovOrg name -> name
    Company name _ _ _ -> name
    Individual (Person firstName lastName _) _ -> firstName ++ " " ++ lastName

responsibility :: Client -> String
responsibility (Company _ _ _ r) = r
responsibility _ = "Unknown"

specialClient :: Client -> Bool
specialClient (clientName -> "Max Oliinyk") = True
specialClient (responsibility -> "Director") = True
specialClient _ = False

-- Returns number of client for a given gender
genderStatistics :: [Client] -> Gender -> Integer
genderStatistics [] _ = 0
genderStatistics (x:xs) genderOf = genderStatistics xs genderOf + plus x genderOf

plus :: Client -> Gender -> Integer
plus client Male =
  case client of
    Company _ _ (Person _ _ Male) _ -> 1
    Individual (Person _ _ Male) _ -> 1
    _ -> 0
plus client Female =
  case client of
    Company _ _ (Person _ _ Female) _ -> 1
    Individual (Person _ _ Female) _ -> 1
    _ -> 0
plus _ _ = 0

-- applies percentage discount to a list of time machines
applyDiscount :: [TimeMachine] -> Integer -> [TimeMachine]
applyDiscount [] _ = []
applyDiscount (m@TimeMachine {..}:xs) discount = m {price = price - (price * (fromIntegral discount / 100.0))} : applyDiscount xs discount