module Lib
  ( someFunc
  ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

firstOrEmpty lst =
  if not (null lst)
    then head lst
    else "empty"

unzip' :: [(Int, Int)] -> ([Int], [Int])
unzip' [] = ([], [])
unzip' ((f, s):xs) = (f : ff, s : ss)
  where
    ff = fst $ unzip' xs
    ss = snd $ unzip' xs

ackerman :: Int -> Int -> Int
ackerman m n
  | m == 0 = n + 1
  | m > 0 && n == 0 = ackerman (m - 1) 1
  | otherwise = ackerman (m - 1) (ackerman m (n - 1))