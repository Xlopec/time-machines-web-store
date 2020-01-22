module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

firstOrEmpty lst = if not (null lst) then head lst else "empty"
