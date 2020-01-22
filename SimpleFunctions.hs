
firstOrEmpty :: [[Char]] -> [Char]
firstOrEmpty lst = if not (null lst) then head lst else "empty"

l1 +++ l2 = if null l1
            then l2
            else (head l1) : (tail l1 +++ l2)

reverse2 :: [a] -> [a]
reverse2 lst = if null lst
               then []
               else reverse2 (tail lst) +++ [head lst]

maxmin :: Ord a => [a] -> (a, a)
maxmin l = let h = head l
           in if null (tail l)
              then (h, h)
              else ( if h > t_max then h else t_max,
                     if h < t_min then h else t_min
                   )
                     where t = maxmin (tail l)
                           t_max = fst t
                           t_min = snd t
