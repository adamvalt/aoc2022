import Data.List

-- trashcode please dont look

main = do
    f <- readFile "11.txt"
    print $ foldl1 (*) $ take 2 $ reverse $ sort $ extractInspected $ (iterate monkeyRound (parseMonkeys $ lines f)) !! 20
    print $ foldl1 (*) $ take 2 $ reverse $ sort $ extractInspected $ (iterate monkeyRoundNoWorry (parseMonkeys $ lines f)) !! 10000

type Items = [Int]
type Monkey = (Items, Operation, Test, Int)

type Operation = (String, String)

--           op, true, false
type Test = (Int, Int, Int)

parseMonkeys :: [String] -> [Monkey]
parseMonkeys [] = [] 
parseMonkeys t = map getMonkey $ splitter t


getMonkey :: [String] -> Monkey
getMonkey (items:op:test:true:false:_) = ((getItems items), (getOp op), (getCondition test, getTest true, getTest false), 0)
    where
        getItems :: String -> [Int] 
        getItems i = map read $ map (filter (/=',')) (drop 2 $ words i) 
        getOp :: String -> (String, String)
        getOp o = ((words o !! 4), (words o !! 5))
        getCondition c = read $ words c !! 3
        getTest x = read $ words x !! 5
getMonkey _ = ([], ("", "?"), (-1, -1, -1), -1)


splitter :: [String] -> [[String]]
splitter [] = []  
splitter n = takeWhile w tn : splitter (dropWhile w tn)
    where
        tn = tail n
        w = ((/= "Monkey").(take 6))

monkeyRound :: [Monkey] -> [Monkey]
monkeyRound m = roundHelper (flip div 3) m m 0

monkeyRoundNoWorry :: [Monkey] -> [Monkey]
monkeyRoundNoWorry m = roundHelper (flip mod $ getModNumber m) m m 0

getModNumber :: [Monkey] -> Int
getModNumber [] = 1 
getModNumber (x:xs) = (extractn x) * (getModNumber xs)
    where
        extractn (_, _, (n, _, _), _) = n 

roundHelper :: (Int -> Int) -> [Monkey] -> [Monkey] -> Int -> [Monkey]
roundHelper _ [] newM _ = newM
roundHelper divNumber (_:xs) newM i = roundHelper divNumber xs (nm divNumber i newM) (i + 1)

nm :: (Int -> Int) -> Int -> [Monkey] -> [Monkey]
nm divNumber i newM = clearItems i (fst $ inspect divNumber (newM !! i) newM) (snd $ inspect divNumber (newM !! i) newM)

clearItems :: Int -> [Monkey] -> Int -> [Monkey]
clearItems index n c = take index n ++ [(clearMonkeyItems (n !! index))] ++ drop (index + 1) n  
    where
        clearMonkeyItems :: Monkey -> Monkey
        clearMonkeyItems (_, a, b, _) = ([], a, b, c) 

inspect :: (Int -> Int) -> Monkey -> [Monkey] -> ([Monkey], Int)
inspect _ ([], _, _, i) m = (m, i)
inspect divNumber ((item:xs), (op, x), (c, t, f), insp) m = inspect divNumber (xs, (op, x), (c, t, f), (insp + 1)) (throwItem getThrownIndex)
    where
        inspectItem :: Int
        inspectItem | op == "*" = item * resolveOpAttr
                    | op == "+" = item + resolveOpAttr
                    | otherwise = -1
        
        resolveOpAttr :: Int
        resolveOpAttr | x == "old" = item
                      | otherwise = read x
        
        newItemValue :: Int
        newItemValue = divNumber inspectItem 

        testCase :: Bool
        testCase = newItemValue `rem` c == 0

        getThrownIndex :: Int
        getThrownIndex = if testCase then t else f

        throwItem :: Int -> [Monkey]
        throwItem index = (take index m) ++ [addItem (m !! index)] ++ (drop (index + 1) m) 

        addItem :: Monkey -> Monkey
        addItem (items, o, tc, inspe) = ((items ++ [newItemValue]), o, tc, inspe)

extractInspected :: [Monkey] -> [Int]
extractInspected [] = []
extractInspected (x:xs) = extract x : extractInspected xs
    where
        extract (_, _, _, inspected) = inspected 