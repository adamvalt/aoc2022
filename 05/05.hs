import Data.Char
import Data.List
import Text.Read (readMaybe)
import Data.Maybe

type Stacks = [[String]]
type Commands = [(Int, Int, Int)]

main = do
    f <- readFile "05.txt"
    print $ concat $ map head $ eval reverse (parseCommands (lines f)) (parseStacks (lines f))
    print $ concat $ map head $ eval id (parseCommands (lines f)) (parseStacks (lines f))

parseStacks :: [String] -> Stacks
parseStacks x = map ((filter(/="")).init) $ transpose $ map wordsCustom $ separateStacks x

parseCommands :: [String] -> Commands
parseCommands x = map (to3tuple.convertInts) $ tail $ map (filter(stringIsNumber).words) $ separeteCommands x

to3tuple :: [Int] -> (Int, Int, Int)
to3tuple (x:y:z:[]) = (x, y, z)
to3tuple _ = (0, 0, 0)

separateStacks :: [String] -> [String]
separateStacks = takeWhile (/="")

separeteCommands :: [String] -> [String]
separeteCommands = dropWhile (/="")

wordsCustom :: String -> [String]
wordsCustom s = if length s == 3 then 
                    if (isEmptyStack s) then 
                        [] else [extractLetter s] else 
                            if isEmptyStack (firstN 4) then 
                                "" : (wordsCustom rest) else (extractLetter (firstN 3)) : (wordsCustom rest)
    where 
        firstN n = take n s
        rest = drop 4 s
        isEmptyStack = all isSpace  
        extractLetter :: String -> String 
        extractLetter (_:y:_) = [y]
        extractLetter _ = []

stringIsNumber :: String -> Bool
stringIsNumber s = not $ isNothing (readMaybe s :: Maybe Int)

convertInts :: [String] -> [Int]
convertInts = map read

eval :: ([String] -> [String]) -> Commands -> Stacks -> Stacks
eval _ [] s = s
eval f (c:xs) s = eval f xs (move f c s)

move :: ([String] -> [String]) -> (Int, Int, Int) -> Stacks -> Stacks 
move f (n, from, to) s = insert2D f (take n (s !! (from - 1))) (to - 1) (dropNfrom n (from - 1) s)

dropNfrom :: Int -> Int -> Stacks -> Stacks 
dropNfrom n 0 (s:xs) = (drop n s) : xs
dropNfrom n from (s:xs) = s : dropNfrom n (from - 1) xs

insert2D :: ([String] -> [String]) -> [String] -> Int -> Stacks -> Stacks
insert2D orderFunc s 0 (r:rs) = ((orderFunc s) ++ r) : rs
insert2D orderFunc s pos (r:rs) = r : insert2D orderFunc s (pos - 1) rs