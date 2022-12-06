import Data.List

main = do
    f <- readFile "06.txt"
    print $ getMarker 0 4 $ head $ lines f
    print $ getMarker 0 14 $ head $ lines f

getMarker :: Int -> Int -> String -> Int
getMarker i diffChars s = if length (nub (take diffChars s)) /= diffChars then getMarker (i + 1) diffChars (tail s) else i + diffChars