import Data.Char

main = do
    f <- readFile "02.txt"
    print $ sum $ map (calculateResult.convertToInt.toTuple.words) $ lines f
    print $ sum $ map (calculateResult.guessResult.convertToInt.toTuple.words) $ lines f

convertToInt :: (String, String) -> (Int, Int)
convertToInt (x, y) =  (ord (head x) - 64, ord (head y) - 87) 

toTuple :: [String] -> (String, String)
toTuple s = ((s!!0), (s!!1))

calculateResult :: (Int, Int) -> Int  
calculateResult (a, b) | a == b = b + 3
                       | a + 1 == b = b + 6
                       | a - 2 == b = b + 6
                       | otherwise = b

guessResult :: (Int, Int) -> (Int, Int)
guessResult (a, b) | b == 2 = (a, a)
                   | b == 3 = (a, mod (a + 3) 3 + 1)  
                   | b == 1 = (a, mod (a + 1) 3 + 1)
                   | otherwise = (0, 0)
                   
