import Data.Array (Array,(//),(!))
import qualified Data.Array as A
import Data.Char (ord)

import Data.Map (Map)
import qualified Data.Map as M
import Data.Heap (MinPrioHeap)
import qualified Data.Heap as H

type Coord = (Int,Int)

readArr s = A.array ((0,0),(m,n)) xs
  where ls = lines s
        n = (length (ls!!0))-1
        m = (length ls)-1
        xs = [( (i,j), (ls!!i)!!j ) | i <- [0..m], j <- [0..n]]

-- reachable neighbor coordinates of (i,j)
nbors arr (i,j) = filter reachable coords
  where coords = [(i+1,j),(i-1,j),(i,j+1),(i,j-1)]
        (_,bd) = A.bounds arr
        aij = arr ! (i,j)
        reachable ij' =
          if not (inGrid ij' bd)
          then False
          else (ord aij >= ord aij') || ( 1 + ord aij == ord aij')
               where aij' = arr ! ij'
                     inGrid (i,j) (n,m) = (i>=0 && i<=n)&&(j>=0 && j<=m)

emptyQ = H.empty :: MinPrioHeap Int Coord
inf = maxBound :: Int

findInArr arr x = head $ filter (\t -> snd t == x) (A.assocs arr)

initArr arr = (arr',src,end)
  where (src,_) = findInArr arr 'S'
        (end,_) = findInArr arr 'E'
        arr' = arr // [(src,'a'),(end,'z')]

-- init state for Djisktra 
initDjikstra arr src = (pq,dist)
  where dist = M.fromList [(src,0)] 
        pq = H.insert (0,src) emptyQ

-- run one step of Djikstra
djikstra arr end pq dist = case H.view pq of
    Nothing -> (pq, dist)
    Just ( ((d,u), pq') ) ->
      if u == end then (emptyQ,dist) -- stop when end reached
      else foldr step (pq',dist) nbrs
      where nbrs = nbors arr u
            step v (q,dst) =
              if d' < (M.findWithDefault inf v dst)
              then ( --update state with new u->v path
                H.insert (d',v) q,
                M.insert v d' dst
                )
              else (q,dst) -- continue
              where d' = (dist M.! u) + 1

runDjikstra arr end src = last
             $ takeWhile (\(q,_) -> q /= emptyQ)
             $ iterate go init
  where init = initDjikstra arr src
        go (q,dst) = djikstra arr end q dst

solve1 arr end src = dist M.! end
  where (_,dist) = runDjikstra arr end src

solve2 arr end = minimum $ map (solve1 arr end) srcs
  where srcs = findSrcs arr

-- find 'a' srcs next to a 'b'
findSrcs arr = filter hasNbr
  $ map fst
  $ filter (\(_,c) -> c == 'a')
  $ A.assocs arr
  where hasNbr ij = elem 'b'
          $ map (\x -> arr!x)
          $ nbors arr ij

main = do
  contents <- readFile "input12.txt"
  let a = readArr contents
  let (arr, src, end) = initArr a
  --let ans = solve1 arr end src
  let ans = solve2 arr end
  print ans
