import Data.Char (digitToInt)
import Data.List (group, sort)

-- coords functions
add (a,b) (c,d) = (a+c, b+d)
sub (a,b) (c,d) = (a-c, b-d)
nbors (a,b) = [(a+i,b+j) | i<-[-1..1], j<-[-1..1]]
initCoords n = take n (repeat (0,0))

readMove x = take n $ repeat d
  where n = read nStr :: Int
        d = toVec (head dStr)
        [dStr,nStr] = words x
  
toVec dir = case dir of
  'D' -> (0,-1)
  'U' -> (0,1)
  'R' -> (1,0)
  'L' -> (-1,0)

uniq :: Eq a => [a] -> [a]
uniq = map head . group

-- move tail based on head position
moveTail h t =
  if elem t (nbors h) then t
  else add t v
  where v = let (a,b) = sub h t in
          if abs(a)==2 && abs(b)==2
          then (div a 2, div b 2)
          else if abs(b)==2 then (a, div b 2)
          else if abs(a)==2 then (div a 2, b)
          else (0,0)

doMove :: [(Int,Int)] -> (Int,Int) -> [(Int,Int)]
doMove (h:cs) dir = reverse $ foldl op [h'] cs
  where h' = add h dir
        op acc c = let x = head acc in (moveTail x c):acc

-- apply moves to length n rope
-- while keeping track of tail history 
solve :: Int -> [(Int,Int)] -> Int
solve n mvs = length $ uniq $ sort hst
  where (hst,_) = foldl op ([], initCoords n) mvs
          where op (hst,cs) mv =
                  let cs' = doMove cs mv in ((last cs'):hst, cs')

solve1 = solve 2
solve2 = solve 10

main = do
  contents <- readFile "input9.txt"
  let mvs = concat $ map readMove $ lines contents
  
  --let ans = solve1 mvs
  let ans = solve2 mvs
  print ans
