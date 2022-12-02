data Move = Rock | Paper | Scissors deriving (Eq, Show)

readMove c = case c of
  'A' -> Rock
  'B' -> Paper
  'X' -> Rock
  'Y' -> Paper
  _ -> Scissors

counter m = case m of
  Rock -> Paper
  Paper -> Scissors
  Scissors -> Rock

val m = case m of
          Rock -> 1
          Paper -> 2
          Scissors -> 3

win (x,y)
  | x==y = 3
  | x==counter y = 0
  | y==counter x = 6

score (x,y) = val y + win (x,y)

readMoves s = let x = head s
                  y = last s in (readMove x, readMove y)

readMoves2 s = let x = readMove (head s)
                   y = last s in (x, myMove x y)
  where myMove x y = case y of
          'X' -> (counter.counter) x
          'Y' -> x
          'Z' -> counter x

solve1 xs = sum $ map score $ map readMoves xs
solve2 xs = sum $ map score $ map readMoves2 xs

main = do
  contents <- readFile "input2.txt"
  let ls = lines contents
  -- let ans = solve1 ls
  let ans = solve2 ls
  print ans
