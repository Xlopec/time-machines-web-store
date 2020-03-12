module Main where

import Chapter2.Lib
import Data.Foldable (foldl')

main :: IO ()
main = putStrLn $ show result

result :: Integer
result = foldl' (*) 1 [1 .. 100000]
