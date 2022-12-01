import Data.List.Split (splitOn)
import Data.List (sort)

readInts :: [String] -> [Int]
readInts = map read

solve1 xs = maximum $ map sum xs

solve2 xs = sum $ take 3 $ (reverse.sort) sums
  where sums = map sum xs

main = do
  contents <- readFile "input.txt"
  let xs = map readInts $ map words $ splitOn "\n\n" contents
  -- print (solve1 xs)
  print (solve2 xs)

