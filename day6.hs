import Data.List (group, sort)

uniq = map head . group

hasDup xs = let u = uniq $ sort xs in
  length u /= length xs

solve' n x i =
  if hasDup pfx then solve' n (tail x) (i+1)
  else i+n
  where (pfx,x') = splitAt n x

solve1 x = solve' 4 x 0
solve2 x = solve' 14 x 0

main = do
  s <- readFile "input6.txt"
  --let ans = solve1 s
  let ans = solve2 s 
  print ans
