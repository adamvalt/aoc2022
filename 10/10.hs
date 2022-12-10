

main = do
    f <- readFile "10.txt"
    print $ sum $ zipWith (*) (getCycleValues (eval $ lines f) cv) cv
    print $ getRow 0 [0..40] (eval $ lines f)
    print $ getRow 0 [40..80] (eval $ lines f)
    print $ getRow 0 [80..120] (eval $ lines f)
    print $ getRow 0 [120..160] (eval $ lines f)
    print $ getRow 0 [160..200] (eval $ lines f)
    print $ getRow 0 [200..240] (eval $ lines f)
    
    
cv :: [Int]   
cv = [20, 60, 100, 140, 180, 220]

eval :: [String] -> [Int] 
eval input = evalStep input [0] 1

evalStep :: [String] -> [Int] -> Int -> [Int]
evalStep (newInstruction:rest) (x:xs) result = (result + x) : evalStep rest (xs ++ evalInstruction newInstruction) (result + x)
evalStep _ (x:xs) result = (result + x) : evalStep [] xs (result + x)
evalStep _ _ result = result : []


evalInstruction :: String -> [Int]
evalInstruction s | take 4 s == "noop" = [0] 
                  | take 4 s == "addx" = [0, read $ last $ words s] 
                  | otherwise = [0]

getCycleValues :: [Int] -> [Int] -> [Int]
getCycleValues _ [] = []
getCycleValues v (x:xs) = v !! (x-1) : getCycleValues v xs

getRow :: Int -> [Int] -> [Int] -> String
getRow _ [_] _ = []
getRow i (c:xs) values | values !! c == i = '#' : getRow (i + 1) xs values 
                           | values !! c == i + 1 = '#' : getRow (i + 1) xs values
                           | values !! c == i - 1 = '#' : getRow (i + 1) xs values
                           | otherwise = '.' : getRow (i + 1) xs values
getRow _ _ _ = []