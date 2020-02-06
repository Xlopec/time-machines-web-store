{-# LANGUAGE NamedFieldPuns #-}

module Chapter3.Folds where

import Chapter2.DataTypesR

product1 :: Int -> [Int] -> Int
product1 acc (x:xs) = product1 (acc * x) xs
product1 acc [] = acc

product2 :: Int -> [Int] -> Int
product2 = foldl (*)

data InfClient
  = MinusInf
  | ClientInf ClientR
  | PlusInf deriving Show

shortest :: InfClient -> InfClient -> InfClient
shortest MinusInf b = b
shortest a MinusInf = a
shortest PlusInf _ = PlusInf
shortest _ PlusInf = PlusInf
shortest a@(ClientInf c1) b@(ClientInf c2) | length (clientRName c1) > length (clientRName c2) = b
shortest a _ = a

minimumClient1 :: [ClientR] -> InfClient -> InfClient
minimumClient1 [] current = current
minimumClient1 (x:xs) current = minimumClient1 xs (shortest (ClientInf x) current)

minimumClient2 :: InfClient -> [ClientR] -> InfClient
minimumClient2 = foldl (\x y -> shortest x (ClientInf y) )

all1 :: [Bool] -> Bool
all1 [] = True
all1 (True:xs) = all1 xs
all1 _ = False

all2 :: [Bool] -> Bool
all2 = foldl (&&) True