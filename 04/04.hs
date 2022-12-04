
main = do
    f <- readFile "04.txt"
    print $ length $ filter (fullyOverlaps) $ map (resolveSectionBorders) $ map (splitPairs ',') $ lines f
    print $ length $ filter (partiallyOverlaps) $ map (resolveSectionBorders) $ map (splitPairs ',') $ lines f

splitPairs :: Char -> String -> (String, String)
splitPairs c s = ((takeWhile (/= c) s), (tail (dropWhile (/= c) s))) 

resolveSectionBorders :: (String, String) -> ((Int, Int), (Int, Int))
resolveSectionBorders (a, b) = ((tupleToInt (splitPairs '-' a)), (tupleToInt (splitPairs '-' b)))
    where
        tupleToInt :: (String, String) -> (Int, Int)
        tupleToInt (x, y) = (read x, read y)

fullyOverlaps :: ((Int, Int), (Int, Int)) -> Bool
fullyOverlaps ((a1, a2), (b1, b2)) = ((a1 <= b1) && (b2 <= a2)) || ((b1 <= a1) && (a2 <= b2))

partiallyOverlaps :: ((Int, Int), (Int, Int)) -> Bool
partiallyOverlaps ((a1, a2), (b1, b2)) = (a1 <= b1 && a2 >= b1) || (b1 <= a1 && a1 <= b2)
