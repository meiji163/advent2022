import Data.List.Split (chunksOf, splitOn)
import Data.Array

noCrate = '-'
readCrate s = if (length s >= 3 && s!!0 == '[' && s!!2 == ']')
  then s!!1 else noCrate

toArray l = listArray (1,n) l where n = length l

-- read stacks into Array of Lists
readStacks ls = toArray $ map (filter (/= noCrate)) cols
  where cols = [column i xs | i <- [0..n]]
        column i xs = [x !! i | x <- xs]
        xs = map (map (readCrate))
             $ map (chunksOf 4) ls
        n = (length xs) - 1

readInt :: String -> Int
readInt = read

readMoves ls = map read' ls
  where read' l = map readInt [i,j,k]
          where [_,i,_,j,_,k] = words l

-- move i crates from j to k
-- if "doReverse" reverse the crates that are moved
applyMove doReverse stacks [i,j,k] =
  let sj = stacks!j
      sk = stacks!k
      (m,sj') = splitAt i sj
      sk' = if doReverse then ((reverse m) ++ sk) else m ++ sk
  in stacks // [(j,sj'),(k,sk')]

solve doReverse mvs stacks = map head
  $ elems
  $ foldl (applyMove doReverse) stacks mvs

solve1 st mv = solve True st mv
solve2 st mv = solve False st mv

main = do
  contents <- readFile "input5.txt"
  let [stackStr,moveStr] = splitOn "\n\n" contents
  let stks = readStacks (lines stackStr)
  let mvs = readMoves (lines moveStr)
  let ans = solve2 mvs stks
  print ans
