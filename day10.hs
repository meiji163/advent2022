import Data.List.Split (chunksOf)

data OpCode = NoOp | AddX Int deriving (Show)

readOp s = case words s of
  ["addx",n] -> AddX (read n :: Int)
  _ -> NoOp

applyOp op x = case op of
  AddX n -> x + n
  _ -> x

applyOps ops x = reverse $ foldl go [x] ops
  where go hst op =
          let h = head hst
              op' = applyOp op h in
          case op of
            AddX n -> op':h:hst -- 2 cycles
            _ -> op':hst

drawLine xs = reverse $ foldl go "" (zip [0..] xs)
  where go ps (i,x) = p:ps
          where p = if abs(i-x)<2 then '#' else '.'
 
solve1 ops = sum [(hst!!(i-1)) * i | i <- [20,60..220]]
  where hst = applyOps ops 1


solve2 ops = unlines $ map drawLine (chunksOf 40 xs)
  where xs = applyOps ops 1

main = do
  contents <- readFile "input10.txt"
  let ops = map readOp $ lines contents

  --let ans = solve1 ops
  --print ans

  let ans = solve2 ops
  putStr ans
