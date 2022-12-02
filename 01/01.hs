import Data.List
import Data.Ord

main = do
    f <- readFile "01.txt"
    print "part one:"
    print $ maximum $ map sum $ separateByNewline (lines f) []
    print "part two:"
    print $ sum $ take 3 $ sortOn Down $ map sum $ separateByNewline (lines f) []


separateByNewline :: [String] -> [Int] -> [[Int]]
separateByNewline [] templist = templist : []
separateByNewline (x:xs) templist = if x == "" then templist : separateByNewline xs [] else separateByNewline xs ((read x):templist) 