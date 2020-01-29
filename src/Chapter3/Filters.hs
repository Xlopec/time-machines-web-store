{-# LANGUAGE LambdaCase #-}

module Chapter3.Filters where
import Chapter2.DataTypes

filterOnes :: [Int] -> [Int]
filterOnes l = filter (\x -> x == 1) l

filterANumber :: (Num a, Eq a) => [a] -> a -> [a]
filterANumber l n = filter (\x -> x == n) l

filterNot :: Eq a => [a] -> a -> [a]
filterNot l n = filter (\x -> not $ x == n) l

filterGovOrgs :: [Client] -> [Client]
filterGovOrgs l = filter (\case GovOrg _ -> True
                                _ -> False) l
