import Data.Array
import Data.Char (digitToInt)
import Data.List.Split (chunksOf)

intoArr ls = array ((0,0),(x,y)) [((i,j), (ls!!i)!!j) | i<-[0..x], j<-[0..y]]
  where x = (length ls)-1
        y = (length (head ls))-1

constArr c x y = array ((0,0),(x,y)) [((i,j), c) | i<-[0..x], j<-[0..y]]

solve1 arr = length $ filter (\c -> c>0)
  $ elems
  $ foldl op cnts ixLists
  where (_,(x,y)) = bounds arr
        cnts = constArr 0 x y
        ixLists = itrRowColIxs x y
        op cs is = let (_,cs') = scan1 arr cs is in cs'

solve2 arr = maximum $ elems
  $ foldl op cnt ixLists
  where (_,(x,y)) = bounds arr
        cnt = constArr 1 x y
        ixLists = itrRowColIxs x y
        op = scan2 arr

scan1 arr cnts ixs = foldl op (-1,cnts) ixs
  where op (maxH,cs) i = let cur = arr!i in
          if cur>maxH then (cur, cs // [(i,cs!i+1)])
          else (maxH,cs)

scan2 arr cnt ixs = if length ixs == 1
  then cnt // [(i,0)]
  else scan2 arr cnt' ix'
  where (i:ix') = ixs
        cur = arr!i
        ys = takeWhile (\j -> arr!j < cur) ix'
        ylen = length ys
        d = if ylen < length ix' then ylen+1 else ylen
        cnt' = cnt // [(i,(cnt!i)*d)]

-- index lists to iterate in 4 xy dirs
itrRowColIxs x y = (rowIx ++ colIx ++ (map reverse colIx) ++ (map reverse rowIx))
  where colIx = [[(i,j) | i<-[0..x]] | j<-[0..y]]
        rowIx = [[(i,j) | j<-[0..y]] | i<-[0..x]]

main = do
  contents <- readFile "input8.txt"
  let ls = map (map digitToInt) $ lines contents
  let arr = intoArr ls
  let ans = solve1 arr
  let ans = solve2 arr
  print ans
