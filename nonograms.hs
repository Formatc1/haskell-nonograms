import System.Environment

splitBy :: Char -> [Char] -> [[Char]]
splitBy delimiter = foldr f [[]]
    where f c (x:xs)
            | c == delimiter = []:(x:xs)
            | otherwise = (c:x):xs

transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

fillSimpleBoxes :: (Int -> Int) -> Int -> ([Int], [Int]) -> [Int]
fillSimpleBoxes _ _ ([], board) = board
fillSimpleBoxes f color (numbers@(num:nums), board@(field:fields)) = if field < 0 
    then field : (fillSimpleBoxes f color (numbers, fields)) 
    else if (itFits num board) 
        then if null nums 
            then (replicate num color) ++ (drop num board)
            else (replicate num color) ++ [0] ++ (fillSimpleBoxes f (f color) (nums, (drop (num + 1) board)))
        else 0 : fillSimpleBoxes f color (numbers, fields)
fillSimpleBoxes _ _ _ = []

itFits :: Int -> [Int] -> Bool
itFits 0 [] = True
itFits _ [] = False
itFits 0 (field:fields) = if field > 0 then False else True
itFits num (field:fields) = if field < 0 then False else itFits (num - 1) fields

--leftFillSimpleBoxes :: Int -> Int -> [Int] -> [Int]
--leftFillSimpleBoxes _ size [] = replicate size 0
--leftFillSimpleBoxes color size (number:next:xs) = (replicate number color) ++ [0] ++ (leftFillSimpleBoxes (color + 1) (size - number - 1) (next:xs))
--leftFillSimpleBoxes color size [number] = (replicate number color) ++ (replicate (size - number) 0)

--rightFillSimpleBoxes :: Int -> Int -> [Int] -> [Int]
--rightFillSimpleBoxes _ size [] = replicate size 0
--rightFillSimpleBoxes color size (number:next:xs) = (rightFillSimpleBoxes (color - 1) (size - number - 1) (next:xs)) ++ [0] ++ (replicate number color)  
--rightFillSimpleBoxes color size [number] = (replicate (size - number) 0 ) ++ (replicate number color)

--checkSimpleBoxes :: (Int, Int) -> Int
--checkSimpleBoxes x = if (fst x) == (snd x) && (fst x) > 0 then 1 else 0

intersectionElem :: (Int, Int) -> Int
intersectionElem x = if (fst x) > 0 && (snd x) > 0 && (fst x) == (snd x) then 1 else if (fst x) < 0 || (snd x) < 0 then -1 else 0

--simpleBoxes :: Int -> [Int] -> [Int]
--simpleBoxes size numbers = map checkSimpleBoxes (zip x (rightFillSimpleBoxes (maximum x) size (reverse numbers))) where x = (leftFillSimpleBoxes 2 size numbers)

unionElem :: (Int, Int) -> Int
unionElem x = if (fst x) > 0 || (snd x) > 0 then 1 else if (fst x) < 0 || (snd x) < 0 then -1 else 0

addSolutionList :: ((Int, Int) -> Int) -> ([Int], [Int]) -> [Int]
addSolutionList f x = map f (zip (fst x) (snd x))

addSolution :: ((Int, Int) -> Int) -> [[Int]] -> [[Int]] -> [[Int]]
addSolution f x y = map (addSolutionList f) (zip x y)

showBoard :: [[Int]] -> [Char]
showBoard board = (foldr (\a b -> a ++ "\n" ++ b) [] (map (map (\a -> if a > 0 then '█' else ' ')) board))

count :: [Int] -> Int
count row = length (filter (> 0) row)

checkAndCompleteRow :: ([Int], [Int]) -> [Int]
checkAndCompleteRow row = if (sum (fst row)) == (count (snd row)) then map (\x -> if x > 0 then 1 else -1) (snd row) else snd row

reversePair :: ([a], [a]) -> ([a], [a])
reversePair (a, b) = (reverse a, reverse b)

countProgress :: [[Int]] -> Int
countProgress board = sum (map count board)

stepSolution :: [[Int]] -> [[Int]] -> [[Int]] -> [[Int]] 
stepSolution horizontal vertical board = do
    board1 <- return board
    board2 <- return (transpose board)
    board3 <- return (addSolution intersectionElem (map (fillSimpleBoxes ((+) 1) 2) (zip horizontal board1)) (map (\x -> reverse (fillSimpleBoxes (flip (-) 1) ((length (fst x)) + 1) (reversePair x))) (zip horizontal board1)))
    board4 <- return (addSolution intersectionElem (map (fillSimpleBoxes ((+) 1) 2) (zip vertical board2)) (map (\x -> reverse (fillSimpleBoxes (flip (-) 1) ((length (fst x)) + 1) (reversePair x))) (zip vertical board2)))

    board3 <- return (addSolution unionElem board3 (transpose board4))
    board4 <- return (transpose board3)

    board3 <- return (map (checkAndCompleteRow) (zip horizontal board3))
    board4 <- return (map (checkAndCompleteRow) (zip vertical board4))

    board3 <- return (addSolution unionElem board3 (transpose board4))
    if (countProgress board3 > countProgress board1)
        then stepSolution horizontal vertical board3
        else board3


main = do
    -- read board parameters from file
    files <- getArgs
    x <- (>>=) (readFile (head files)) (return.splitBy '\n')
    x <- return (map (splitBy ' ') x)
    x <- return (map (map (read::String->Int)) x)
    size <- return (head x)
    width <- return (head size)
    height <- return (head (tail size))
    horizontal <- return (drop width (drop 1 x))
    vertical <- return (take width (drop 1 x))

    board <- return (replicate height (replicate width 0))

    board <- return (stepSolution horizontal vertical board)
    writeFile (head (tail files)) (showBoard board)
    print board
