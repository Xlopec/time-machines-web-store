{-# LANGUAGE LambdaCase #-}

module Chapter3.Filters where
import Chapter2.DataTypes

filterOnes :: [Int] -> [Int]
filterOnes = filter (== 1)

filterANumber :: (Num a, Eq a) => a -> [a] -> [a]
filterANumber n = filter (== n)

filterNot :: Eq a => a -> [a] -> [a]
filterNot n = filter (/= n)

filterGovOrgs :: [Client] -> [Client]
filterGovOrgs = filter (\case GovOrg _ -> True
                              _ -> False)
