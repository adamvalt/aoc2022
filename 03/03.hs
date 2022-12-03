import Data.Char (ord)

main = do
    f <- readFile "03.txt"
    print $ sum $ map (convertValues.ord.findCommon.splitString) $ lines f
    print $ sum $ map (convertValues.ord.find3Common) $ group3Lines $ lines f

splitString :: String -> (String, String)
splitString s = splitAt middle s 
    where 
        middle = div (length s) 2

findCommon :: (String, String) -> Char
findCommon (a, b) = commonHelper a b
    where
        commonHelper (x:xs) b = if elem x b then x else commonHelper xs b 

convertValues :: Int -> Int
convertValues v | v <= 90 = v - 38
                | v >= 97 = v - 96

group3Lines :: [String] -> [(String, String, String)]
group3Lines [] = []
group3Lines (x:y:z:xs) = (x, y, z) : group3Lines xs

find3Common :: (String, String, String) -> Char
find3Common (a, b, c) = commonHelper a b c
    where
        commonHelper (x:xs) b c = if elem x b && elem x c then x else commonHelper xs b c