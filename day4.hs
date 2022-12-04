import Data.List.Split (splitOn)

readInt :: String -> Int
readInt = read

readLine l = map readPair [p1, p2]
  where [p1,p2] = splitOn "," l
        readPair p = map readInt $ splitOn "-" p

contains [[a,b], [c,d]] = (a<=c && b>=d) || (c<=a && d>=b)

overlaps [[a,b], [c,d]] = (b>=c && a<=d) || (d>=a && c<=b)

solve1 ls = length $ filter contains ls

solve2 ls = length $ filter overlaps ls

main = do
  contents <- readFile "input4.txt"
  let ls = map readLine $ lines contents
  --let ans = solve1 ls
  let ans = solve2 ls
  print ans
