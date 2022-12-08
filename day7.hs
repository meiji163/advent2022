import Data.List.Split (splitOn)
import Data.List (sort)
import Data.Tree
import Data.Tree.Zipper

type File = (String, Integer)

rootFile = Node (".",0) []
rootPos = fromTree rootFile

readCmds = map f
  . splitOn "\n$ "
  . drop 2
  where f x = if (take 2 x) == "cd" then words x else lines x

parseFile :: String -> Tree File
parseFile s = let xs = words s in
  case xs of
    ["dir", x] -> Node (x,0) []
    [n, x] -> Node (x, read n :: Integer) []

-- apply ls or cd command to build file tree
applyCmd pos ["cd",".."] = case parent pos of
  Nothing -> pos
  Just p' -> p'

applyCmd pos ["cd",dir] = insert newDir (children pos)
  where newDir = Node (dir,0) []

applyCmd pos ("ls":xs) = setTree t pos
  where t = Node (label pos) fs
        fs = map parseFile xs

-- construct file tree from commands
fileTree cmds = tree . root
  $ foldl applyCmd rootPos cmds

sizeOf = snd . rootLabel

-- construct tree where each dir has its total size
sizeTree :: Tree File -> Tree File
sizeTree (Node (x,0) ts) = Node (x,size) ts'
  where ts' = map sizeTree ts
        size = sum $ map sizeOf ts'
sizeTree t = t

-- find dirs in size tree satsifying predicate
findDirs pred (Node f ts) =
  if length ts == 0 then []
  else if pred f then f:ds
  else ds
  where ds = concat $ map (findDirs pred) ts

solve1 cmds = sum $ map snd dirs
  where dirs = findDirs (\f -> snd f <= 100000) sTree
        sTree = sizeTree $ fileTree cmds

solve2 cmds = head $ sort $ map snd dirs
  where dirs = findDirs (\f -> snd f >= need) sTree
        sTree = sizeTree $ fileTree cmds
        used = sizeOf sTree
        need = used - 40000000
        
main = do
  contents <- readFile "input7.txt"
  let cmds = readCmds contents
  --let ans = solve1 cmds
  let ans = solve2 cmds
  print ans
