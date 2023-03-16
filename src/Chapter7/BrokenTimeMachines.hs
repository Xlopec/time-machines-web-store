module Chapter7.BrokenTimeMachines where

brokenThreeJumps :: Int -> [Int]
brokenThreeJumps y0 = do
  y1 <- [-1, 3, 5]
  y2 <- [-1, 3, 5]
  y3 <- [-1, 3, 5]
  return $ y0 + y1 + y2 + y3
  
{-
same as kotlin version
fun brokenJumps(year: Int, jumps: Int): List<Int> {
    if (jumps == 0) return emptyList()
    if (jumps == 1) return listOf(year - 1, year + 3, year + 5)

    return listOf(year - 1, year + 3, year + 5).flatMap { brokenJumps(it, jumps - 1) }
}
-}
brokenJumps :: Int -> Int -> [Int]
brokenJumps _ 0 = []
brokenJumps y 1 = [y - 1, y + 3, y + 5]
brokenJumps y jumps = [y - 1, y + 3, y + 5] >>= flip brokenJumps (jumps - 1)
