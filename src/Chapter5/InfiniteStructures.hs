module Chapter5.InfiniteStructures where

primes :: [Integer]
primes = sieve [2 ..]
  where
    sieve l = head l : sieve (filter (\x -> x `rem` head l /= 0) l)

--foo :: Maybe String -> String
foo ~(Just x) = "hello " ++ x