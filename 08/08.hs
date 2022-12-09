import Data.Char
import Data.List

main = do
    f <- readFile "08.txt"
    print $ sum $ map sum $ zipWith mergeList (normalEvaluated $ lines f) (transposedEvaluated $ lines f)
    print $ maximum $ concat $ zipWith (zipWith (*)) (normalScenic $ lines f) (transposedScenic $ lines f)

digitField :: [String] -> [[Int]]
digitField x = map (map digitToInt) x

normalEvaluated :: [String] -> [[Int]]
normalEvaluated x = map getVisible $ digitField x

transposedEvaluated :: [String] -> [[Int]]
transposedEvaluated x = transpose $ map getVisible $ transpose $ digitField x

normalScenic :: [String] -> [[Int]]
normalScenic x = map getScenicScores $ digitField x

transposedScenic :: [String] -> [[Int]]
transposedScenic x = transpose $ map getScenicScores $ transpose $ digitField x

getVisible :: [Int] -> [Int]
getVisible s = removeVisible s []
    where
        removeVisible [] _ = []    
        removeVisible (x:start) end = if isVisible end x start then -1 : removeVisible start (end ++ [x]) else x : removeVisible start (end ++ [x])
        isVisible :: [Int] -> Int -> [Int] -> Bool
        isVisible [] _ _ = True
        isVisible _ _ [] = True
        isVisible before n after = ((maximum before) < n) || ((maximum after) < n)


mergeList :: [Int] -> [Int] -> [Int]
mergeList (x:xs) (y:ys) = if x == -1 || y == -1 then 1 : mergeList xs ys else 0 : mergeList xs ys
mergeList _ _ = []

getScenicScores :: [Int] -> [Int]  
getScenicScores s = ssHelper s 0
    where
        ssHelper [] _ = []
        ssHelper (_:xs) i = (calculate (fst (splitAt i s)) (s !! i) (snd (splitAt i s))): ssHelper xs (i+1)
        calculate [] i after = calcVisible [] i * calcVisible after i
        calculate before i after = calcVisible (reverse (before)) i * calcVisible (tail after) i
        calcVisible [] _ = 0
        calcVisible [_] _ = 1
        calcVisible (y:ys) n = if n > y then 1 + calcVisible ys n else 1