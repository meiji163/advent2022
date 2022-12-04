import Data.Char (ord)

duplicates x y = foldr addIfElem "" y
  where addIfElem c cs = if elem c x then c:cs else cs

intersect3 s1 s2 s3 = duplicates (duplicates s1 s2) s3

priority c
  | 'a'<=c && c<='z' = ord c - ord 'a' + 1
  | 'A'<=c && c<='Z' = ord c - ord 'A' + 27
  | otherwise = 0

solve1 l = sum $ map (priority . head . solve1') l
  where solve1' l = duplicates c1 c2
          where (c1,c2) = splitAt (length l `div` 2) l

solve2 l = sum $ map (priority . head) $ solve2' l []
  where solve2' [] as = as
        solve2' l as = let (ss,l') = splitAt 3 l
                           [s1,s2,s3] = ss
                           a = intersect3 s1 s2 s3 in
                         solve2' l' (a:as)

main = do
  contents <- readFile "input3.txt"
  let l = lines contents
  --let ans = solve1 l
  let ans = solve2 l
  print ans
