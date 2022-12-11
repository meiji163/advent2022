import Data.List (sort)
import Data.List.Split (splitOn)
import Data.Array

data Monkey = Monkey
  {
    items :: [Int],
    op :: (Int -> Int),
    test :: (Int -> Int)
  }
instance Show Monkey where show = show . items

-- I am lame and parsed the input manually ;(
monkeys = listArray (0,7) [
  Monkey { --0
      items = [89,95,92,64,87,68],
      op = (* 11),
      test = \x -> if (x `mod` 2 == 0) then 7 else 4 },
  Monkey { --1
      items = [87,67],
      op = (+ 1),
      test = \x -> if (x `mod` 13 == 0) then 3 else 6 },
  Monkey { --2
      items = [95,79,92,82,60],
      op = (+ 6),
      test = \x -> if (x `mod` 3 == 0) then 1 else 6 },
  Monkey { --3
      items = [67,97,56], 
      op = (^ 2),
      test = \x -> if (x `mod` 17 == 0) then 7 else 0},
  Monkey { --4
      items = [80,68,87,94,61,59,50,68], 
      op = (* 7),
      test = \x -> if (x `mod` 19 == 0) then 5 else 2},
  Monkey { -- 5
      items = [73,51,76,59], 
      op = (+ 8),
      test = \x -> if (x `mod` 7 == 0) then 2 else 1},
  Monkey { -- 6
      items = [92],
      op = (+ 5),
      test = \x -> if (x `mod` 11 == 0) then 3 else 0},
  Monkey { -- 7
      items = [99,76,78,76,79,90,89],
      op = (+ 7),
      test = \x -> if (x `mod` 5 == 0) then 4 else 5}]

monkeyMod = 2*3*5*7*11*13*17*19
------------------------------------------------


-- do ith monkey's turn
doTurn1 = doTurn' (\x -> div x 3)
doTurn2 = doTurn' (\x -> mod x monkeyMod)
  
doTurn' f ms i = ms' // [(i,mi')]
  where ms' = foldr go ms (items $ ms!i)
        mi' = (ms!i){items=[]}
        go x ms =
          let Monkey{op=op, test=test} = ms!i
              x' = f (op x)
              j = test x' --new monkey index
              mj = ms!j
              mj'= mj{ items = x':(items mj) }
          in
            ms // [(j, mj')]

-- do round keeping track of items inpected per monkey
doRound1 = doRound' doTurn1
doRound2 = doRound' doTurn2

doRound' f (ms,nInsp) = (ms', nInsp')
  where (counts,ms') = foldl go ([],ms) [0..n]
        n = (length ms) - 1
        nInsp' = zipWith (+) (reverse counts) nInsp
        go (nInsp, ms) i =
          let insp = length . items $ ms!i
          in (insp:nInsp, f ms i)

solve1 = solve' doRound1 20
solve2 = solve' doRound2 10000

solve' f rounds ms = (product . take 2 . reverse . sort) nInsp
  where
    (_,nInsp) = (iterate f (ms,zeros)) !! rounds
    n = length ms
    zeros = take n (repeat 0)
  
main = do
  -- let ans = solve1 monkeys
  let ans = solve2 monkeys
  print ans
