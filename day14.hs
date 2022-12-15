import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Function (fix)

readPath l = map readCoord prs
  where prs = filter (/= "->") (words l)
        readCoord s =
          let [n1,n2] = splitOn "," s
          in (read n1 :: Int, read n2 :: Int)

path (x1,y1) (x2,y2) =
  if x1==x2 then [(x1,i) | i <- (range y1 y2)]
  else if y1==y2 then [(i,y1) | i <- (range x1 x2)]
  else []
  where range x y = if x < y then [x..y] else [y..x]

blockedCoords cs = go [] cs
  where go ps [c] = ps
        go ps cs = go (p ++ ps) (tail cs)
          where c1:c2:_ = cs
                p = path c1 c2

blockedSet ls = Set.fromList $ concatMap blockedCoords ls

sandStep max rocks sand (x,y) =
  if (isBlocked (x,y+1)) then
    if not (isBlocked (x-1,y+1)) then
      (x-1,y+1) -- go left
    else if not (isBlocked (x+1,y+1)) then
      (x+1,y+1) -- go right
    else (x,y)
  else if y >= max then
    (x,y) -- fell off
  else (x,y+1)
  where isBlocked x = (Set.member x sand) || (Set.member x rocks)

untilRepeat (x:xs) =
  if x==(head xs) then x
  else untilRepeat xs

-- drop one sand unit
dropOne max rocks sand src = untilRepeat
  $ iterate (sandStep max rocks sand) src

-- drop sand units until (stop end) is True
dropMany stop max rocks sand src =
  if (stop end) then sand
  else dropMany stop max rocks sand' src
  where end = dropOne max rocks sand src
        sand' = Set.insert end sand

solve1 ls = length
  $ dropMany (\(x,y) -> y >= max) max rocks sand src
  where src = (500,0)
        rocks = blockedSet ls
        sand = Set.empty
        max = 170

solve2 ls =
  (length $ dropMany (== src) max rocks sand src)
  + 1 -- add 1 for last unit at (500,0)
  where src = sandSrc
        rocks = blockedSet ls
        sand = Set.empty
        max = (maximum $ map snd (Set.toList rocks)) + 1

main = do
  contents <- readFile "input14.txt"
  let ls = map readPath (lines contents)
  --let s = solve1 ls
  let s = solve2 ls
  print s
